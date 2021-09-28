package com.bl.core.bufferinventory.service.impl;

import com.bl.core.bufferinventory.service.BlBufferInventoryService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;
import de.hybris.platform.catalog.daos.CatalogVersionDao;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.servicelayer.exceptions.BusinessException;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is used to manage the buffer inventory of SKU products
 * @author Moumita
 */
public class DefaultBlBufferInventoryService implements BlBufferInventoryService
{
  private static final Logger LOG = Logger.getLogger(DefaultBlBufferInventoryService.class);
  private BlProductDao productDao;
  private BaseStoreService baseStoreService;
  private ModelService modelService;
  private BlStockLevelDao blStockLevelDao;
  private BlProductService productService;
  private CatalogVersionDao catalogVersionDao;
  private BlStockService blStockService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateBufferInventory() throws BusinessException {
    final BaseStoreModel baseStore = getBaseStoreService().getBaseStoreForUid(
        BlCoreConstants.BASE_STORE_ID);
    if(null != baseStore) {
      try {
        final Double bufferInvPercentage = baseStore.getBufferInventoryPercentage();
        final Double bufferInventoryPercentage =
            (null != bufferInvPercentage && bufferInvPercentage > 0) ?
                bufferInvPercentage : 0;
        final Integer minQtyForBufferInv = baseStore.getMinQtyForBufferInventory();
        final Integer minQtyForBufferInventory = (null != minQtyForBufferInv &&
            minQtyForBufferInv > 0) ? minQtyForBufferInv : 0;
        updateBufferInventoryForProducts(bufferInventoryPercentage, minQtyForBufferInventory);
      } catch (final Exception exception) {
        BlLogger.logMessage(LOG, Level.ERROR, "Exception occurred for BlBufferInventoryCronJob ",
            exception);
        throw new BusinessException("Error while updating the buffer inventory for products");
      }
    } else {
      throw new BusinessException("Base Store should not be null");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean minQtyEligibleForBufferInv(final Integer minQtyForBufferInventory, final BlProductModel blProductModel) {
    if(minQtyForBufferInventory > 0) {
      final int totalSerialProducts = blProductModel.getSerialProducts()
          .stream().filter(serialProduct -> getBlStockService().isActiveStatus(serialProduct.getSerialStatus()))
          .collect(Collectors.toList()).size();
      return totalSerialProducts >= minQtyForBufferInventory;
    }
    return Boolean.TRUE;
  }

  /**
   * It populates the attributes related to buffer inventory
   * @param bufferInvPercentage
   * @param minQtyForBufferInventory
   */
  private void updateBufferInventoryForProducts(final Double bufferInvPercentage, final Integer minQtyForBufferInventory) {
    final Collection<BlProductModel> skuProducts = getProductDao(). getAllActiveSkuProducts();
    final Collection<CatalogVersionModel> catalogModels =  getCatalogVersionDao().findCatalogVersions(BlCoreConstants
          .CATALOG_VALUE, BlCoreConstants.ONLINE);
    final CatalogVersionModel onlineCatalog = catalogModels.iterator().next();
      for (BlProductModel blProduct : skuProducts) {
        if (minQtyEligibleForBufferInv(minQtyForBufferInventory, blProduct) &&
              BooleanUtils.isTrue(blProduct.getForRent())) {
          if (BooleanUtils.isFalse(blProduct.isBufferInvPercChangedManually())) {
            blProduct.setBufferedInventoryPercentage(bufferInvPercentage);
            setSerialProductsBufferInv(blProduct, onlineCatalog);
            saveRecord(blProduct);
            setBufferInvPercentInOnlineVersionProduct(blProduct, onlineCatalog, bufferInvPercentage);
          } else {
            markSerialProductsAsBuffer(blProduct, onlineCatalog);
            setSerialProductsBufferInv(blProduct, onlineCatalog);
            setBufferInvPercentInOnlineVersion(blProduct, onlineCatalog);
          }
        } else {
          List<BlSerialProductModel> serialProducts = blProduct.getSerialProducts().stream()
              .filter(
                  blSerialProductModel -> null != blSerialProductModel.getIsBufferedInventory() &&
                      blSerialProductModel.getIsBufferedInventory()).collect(Collectors.toList());
          serialProducts.forEach(serialProductModel -> {
            serialProductModel.setIsBufferedInventory(Boolean.FALSE);
            saveRecord(serialProductModel);
            markBufferProductInOnlineVersion(serialProductModel.getCode(), onlineCatalog, Boolean.FALSE);
          });
          blProduct.setBufferedInventoryPercentage(Double.valueOf(0.0));
          saveRecord(blProduct);
          setBufferInvPercentInOnlineVersionProduct(blProduct, onlineCatalog, Double.valueOf(0.0));
        }
      }
  }

  /**
   * It clones if buffer inventory flag from staged to online version of the product
   * @param blProduct
   * @param onlineCatalog
   */
  private void markSerialProductsAsBuffer(final BlProductModel blProduct, final CatalogVersionModel onlineCatalog) {
    blProduct.getSerialProducts().forEach(serialProduct -> {
      if(Objects.nonNull(serialProduct.getIsBufferedInventory())) {
        markBufferProductInOnlineVersion(serialProduct.getCode(), onlineCatalog,
            serialProduct.getIsBufferedInventory());
      }
    });
  }

  /**
   * It sets buffer inventory percentage in product of online version
   * @param blProductModel bl product model
   * @param onlineCatalog online catalog version model
   */
  private void setBufferInvPercentInOnlineVersion(final BlProductModel blProductModel, final CatalogVersionModel
      onlineCatalog) {
    final BlProductModel blProductInOnlineVersion = getSkuProduct(blProductModel.getCode(), onlineCatalog);
    if(null != blProductInOnlineVersion) {
      blProductInOnlineVersion.setBufferedInventoryPercentage(blProductModel.getBufferedInventoryPercentage());
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "buffer inventory percentage {} is set for the product {}",
          blProductInOnlineVersion.getBufferedInventoryPercentage(), blProductInOnlineVersion.getCode());
      blProductInOnlineVersion.setBufferInvPercChangedManually(blProductModel.isBufferInvPercChangedManually());
      saveRecord(blProductInOnlineVersion);
    }
  }

  /**
   * It sets buffer inventory percentage in product of online version
   * @param code product code
   * @param onlineCatalog online catalog version model
   * @param isBufferProduct flag to identify buffer product
   */
  private void markBufferProductInOnlineVersion(final String code, final CatalogVersionModel onlineCatalog,
      final Boolean isBufferProduct) {
    final BlSerialProductModel serialProductInOnlineVersion = getSerialProduct(code, onlineCatalog);
    if(null != serialProductInOnlineVersion) {
      serialProductInOnlineVersion.setIsBufferedInventory(isBufferProduct);
      BlLogger
          .logFormatMessageInfo(LOG, Level.DEBUG, "{} product is marked as buffer inventory ",
              code);
      saveRecord(serialProductInOnlineVersion);
    }
  }

  /**
   * It sets buffer inventory percentage in product of online version
   * @param blProduct SKU product model
   * @param onlineCatalog online catalog version model
   * @param bufferInvPercentage buffer inventory percentage
   */
  private void setBufferInvPercentInOnlineVersionProduct(final BlProductModel blProduct, final CatalogVersionModel onlineCatalog,
      final Double bufferInvPercentage) {
    final BlProductModel blProductInOnlineVersion = getSkuProduct(blProduct.getCode(), onlineCatalog);
    if(null != blProductInOnlineVersion) {
      blProductInOnlineVersion.setBufferedInventoryPercentage(bufferInvPercentage);
      blProductInOnlineVersion.setBufferInvPercChangedManually(blProduct.isBufferInvPercChangedManually());
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "buffer inventory percentage {} is set for the product {}",
          bufferInvPercentage, blProduct.getCode());
      saveRecord(blProductInOnlineVersion);
    }
  }

  /**
   * It saves the product instance
   * @param object the product model
   */
  private void saveRecord(final Object object) {
    try {
      getModelService().save(object);
    } catch (final ModelSavingException exception) {
      BlLogger.logMessage(LOG, Level.ERROR, "Exception occurred while saving the instance {} as part of BlBufferInventoryCronJob ",
          object.toString(), exception);
    }
  }

  /**
   * It gets the sku product model
   * @param code product code
   * @param onlineCatalog online catalog version model
   * @return
   */
  private BlProductModel getSkuProduct(final String code, final CatalogVersionModel onlineCatalog) {
    try {
      return (BlProductModel) productService
          .getProductForCode(onlineCatalog, code);
    } catch(final UnknownIdentifierException exception) {
      BlLogger.logMessage(LOG, Level.ERROR, "Exception occurred while getting the product instance {} ",
          code, exception);
    }
    return null;
  }

  /**
   * It gets the serial product model
   * @param code product code
   * @param onlineCatalog online catalog version model
   * @return
   */
  private BlSerialProductModel getSerialProduct(final String code, final CatalogVersionModel onlineCatalog) {
    try {
      return (BlSerialProductModel) productService
          .getProductForCode(onlineCatalog, code);
    } catch(final UnknownIdentifierException exception) {
      BlLogger.logMessage(LOG, Level.ERROR, "Exception occurred while getting the product instance {} ",
          code, exception);
    }
    return null;
  }

  /**
   * It marks the serial products as buffer inventory as per the buffer inventory percentage
   * @param blProduct
   * @param onlineCatalog
   */
  private void setSerialProductsBufferInv(final BlProductModel blProduct, final CatalogVersionModel onlineCatalog) {
    final Double bufferInventoryPercentage = blProduct.getBufferedInventoryPercentage();
    final int totalActiveSerialProducts = blProduct.getSerialProducts()
        .stream().filter(serialProduct -> getBlStockService().isActiveStatus(serialProduct.getSerialStatus()))
        .collect(Collectors.toList()).size();
    if(totalActiveSerialProducts > 0) {
      final int totalBufferProducts = blProduct.getSerialProducts().stream()
          .filter(this::isBufferInventory).collect(Collectors.toList()).size();
      final Double productsNeededForBufferInv = (bufferInventoryPercentage * totalActiveSerialProducts)/100;
      final long actualProductsNeededForBufferInv = Math.round(productsNeededForBufferInv);
      if(actualProductsNeededForBufferInv > totalBufferProducts) {
        final long diffInBufferProducts = actualProductsNeededForBufferInv - totalBufferProducts;
        if(diffInBufferProducts > 0) {
          BlLogger
              .logFormatMessageInfo(LOG, Level.INFO, "{} products to be marked as buffer inventory for the product {} ",
                  diffInBufferProducts, blProduct.getCode());
          markProductsBufferInv(diffInBufferProducts, blProduct, onlineCatalog);
        }
      } else if(totalBufferProducts > actualProductsNeededForBufferInv) {
        final long diffInBufferProducts = totalBufferProducts - actualProductsNeededForBufferInv;
        if(diffInBufferProducts > 0) {
          BlLogger
              .logFormatMessageInfo(LOG, Level.INFO, "{} products to be removed from buffer inventory for the product {} ",
                  diffInBufferProducts, blProduct.getCode());
          unMarkProductsBufferInv(diffInBufferProducts, blProduct, onlineCatalog);
        }
      }
    }
  }

  /**
   * It checks whether the serial product is part of buffer inventory
   * @param blSerialProductModel
   * @return true if it is buffer inventory
   */
  private boolean isBufferInventory(BlSerialProductModel blSerialProductModel) {
    return null != blSerialProductModel
        .getIsBufferedInventory() && blSerialProductModel.getIsBufferedInventory();
  }

  /**
   * It un-marks the serial product from buffer inventory
   * @param productsNeededForBufferInv
   * @param blProduct
   * @param onlineCatalog
   */
  private void unMarkProductsBufferInv(long productsNeededForBufferInv,
      final BlProductModel blProduct, final CatalogVersionModel onlineCatalog) {
      for (BlSerialProductModel blSerialProductModel : blProduct.getSerialProducts()) {
        if (null != blSerialProductModel.getIsBufferedInventory() && BooleanUtils
            .isTrue(blSerialProductModel
                .getIsBufferedInventory())) {
          blSerialProductModel.setIsBufferedInventory(Boolean.FALSE);
          BlLogger
              .logFormatMessageInfo(LOG, Level.INFO, "{} product is released from buffer inventory ",
                  blSerialProductModel.getCode());
          saveRecord(blSerialProductModel);
          markBufferProductInOnlineVersion(blSerialProductModel.getCode(), onlineCatalog, Boolean.FALSE);
          productsNeededForBufferInv--;
          if (productsNeededForBufferInv == 0) {
            break;
          }
        }
      }
  }

  /**
   * It un-marks the serial product from buffer inventory
   * @param productsNeededForBufferInv
   * @param blProduct
   * @param onlineCatalog
   */
  private void markProductsBufferInv(long productsNeededForBufferInv,
      final BlProductModel blProduct, final CatalogVersionModel onlineCatalog) {
      final Date currentDate = Date
          .from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
      for (BlSerialProductModel blSerialProductModel : blProduct.getSerialProducts()) {
        if (null != blSerialProductModel.getIsBufferedInventory() && BooleanUtils
            .isFalse(blSerialProductModel
                .getIsBufferedInventory()) && CollectionUtils.isNotEmpty(getBlStockLevelDao()
            .checkProductAvailabilityForCurrentDate(blSerialProductModel.getCode(), currentDate,
                currentDate))) {
          blSerialProductModel.setIsBufferedInventory(Boolean.TRUE);
          BlLogger
              .logFormatMessageInfo(LOG, Level.INFO, "{} product is marked as buffer inventory ",
                  blSerialProductModel.getCode());
          saveRecord(blSerialProductModel);
          markBufferProductInOnlineVersion(blSerialProductModel.getCode(), onlineCatalog, Boolean.TRUE);
          productsNeededForBufferInv--;
          if (productsNeededForBufferInv == 0) {
            break;
          }
        }
      }
  }

  public BlProductDao getProductDao() {
    return productDao;
  }

  public void setProductDao(BlProductDao productDao) {
    this.productDao = productDao;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

  public BlProductService getProductService() {
    return productService;
  }

  public void setProductService(BlProductService productService) {
    this.productService = productService;
  }

  public CatalogVersionDao getCatalogVersionDao() {
    return catalogVersionDao;
  }

  public void setCatalogVersionDao(CatalogVersionDao catalogVersionDao) {
    this.catalogVersionDao = catalogVersionDao;
  }

  public BlStockService getBlStockService() {
    return blStockService;
  }

  public void setBlStockService(BlStockService blStockService) {
    this.blStockService = blStockService;
  }
}
