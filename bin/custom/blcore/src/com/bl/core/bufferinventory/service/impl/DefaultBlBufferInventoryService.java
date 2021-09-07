package com.bl.core.bufferinventory.service.impl;

import com.bl.core.bufferinventory.service.BlBufferInventoryService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.servicelayer.exceptions.BusinessException;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Collection;
import java.util.Date;
import java.util.List;
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

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateBufferInvProducts() throws BusinessException {
    final BaseStoreModel baseStore = getBaseStoreService().getBaseStoreForUid(
        BlCoreConstants.BASE_STORE_ID);
    if(null != baseStore) {
      final Double bufferInvPercentage = baseStore.getBufferInventoryPercentage();
      final Double bufferInventoryPercentage = (null != bufferInvPercentage && bufferInvPercentage > 0) ?
          bufferInvPercentage : 0;
      final Integer minQtyForBufferInv = baseStore.getMinQtyForBufferInventory();
      final Integer minQtyForBufferInventory = (null != minQtyForBufferInv &&
          minQtyForBufferInv > 0) ? minQtyForBufferInv : 0;
      updateBufferInvData(bufferInventoryPercentage, minQtyForBufferInventory);
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
          .stream().filter(serialProduct -> getProductService().isActiveSerialProduct(serialProduct.getSerialStatus()))
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
  private void updateBufferInvData(final Double bufferInvPercentage, final Integer minQtyForBufferInventory)
      throws BusinessException {
    final Collection<BlProductModel> skuProducts = getProductDao().getAllActiveSkuProducts();
    try {
      for (BlProductModel blProduct : skuProducts) {
        setBufferInvPercentageIfNull(blProduct);
        if (minQtyEligibleForBufferInv(minQtyForBufferInventory, blProduct)) {
          if (BooleanUtils.isFalse(blProduct.isBufferInvPercChangedManually())) {
            blProduct.setBufferedInventoryPercentage(bufferInvPercentage);
            getModelService().save(blProduct);
            setSerialProductsBufferInv(blProduct);
          } else {
            setSerialProductsBufferInv(blProduct);
          }
        } else {
          List<BlSerialProductModel> serialProducts = blProduct.getSerialProducts().stream()
              .filter(
                  blSerialProductModel -> null != blSerialProductModel.getIsBufferedInventory() &&
                      blSerialProductModel.getIsBufferedInventory()).collect(Collectors.toList());
          serialProducts.forEach(serialProductModel -> {
            serialProductModel.setIsBufferedInventory(Boolean.FALSE);
            getModelService().save(serialProductModel);
          });
          blProduct.setBufferedInventoryPercentage(Double.valueOf(0.0));
          getModelService().save(blProduct);
        }
      }
    } catch (final ModelSavingException exception) {
      BlLogger.logMessage(LOG , Level.ERROR , "Exception occurred while saving the instances as part "
          + "of BlBufferInventoryCronJob " , exception);
      throw new BusinessException("Error occurred while performing BlBufferInventoryCronJob");
    } catch(final Exception exception) {
      BlLogger.logMessage(LOG , Level.ERROR , "Exception occurred for BlBufferInventoryCronJob " , exception);
      throw new BusinessException("Error occurred while performing BlBufferInventoryCronJob");
    }
  }

  /**
   * It sets the buffer inventory percentage to 0.0 if it's null
   * @param blProduct
   */
  private void setBufferInvPercentageIfNull(final BlProductModel blProduct) {
    if(null == blProduct.getBufferedInventoryPercentage()) {
      blProduct.setBufferedInventoryPercentage(Double.valueOf(0.0));
      getModelService().save(blProduct);
    }
  }

  /**
   * It marks the serial products as buffer inventory as per the buffer inventory percentage
   * @param blProduct
   */
  private void setSerialProductsBufferInv(final BlProductModel blProduct) {
    final Double bufferInventoryPercentage = blProduct.getBufferedInventoryPercentage();
    final int totalActiveSerialProducts = blProduct.getSerialProducts()
        .stream().filter(serialProduct -> getProductService().isActiveSerialProduct(serialProduct.getSerialStatus()))
        .collect(Collectors.toList()).size();
    if(totalActiveSerialProducts > 0) {
      final int totalBufferProducts = blProduct.getSerialProducts().stream()
          .filter(this::isBufferInventory).collect(Collectors.toList()).size();
      final double currentBufferInventoryPercentage = ((double) totalBufferProducts / (double) totalActiveSerialProducts) *
          BlCoreConstants.DIVIDE_BY_HUNDRED;
      if(Double.compare(bufferInventoryPercentage, currentBufferInventoryPercentage) > 0) {
        final Double diffInPercentage = bufferInventoryPercentage - currentBufferInventoryPercentage;
        final int productsNeededForBufferInv = (int) ((diffInPercentage * totalActiveSerialProducts)/100);
        if(productsNeededForBufferInv > 0) {
          BlLogger
              .logFormatMessageInfo(LOG, Level.INFO, "{} products to be marked as buffer inventory for the product {} ",
                  productsNeededForBufferInv, blProduct.getCode());
          markProductsBufferInv(productsNeededForBufferInv, blProduct);
        }
      } else if(Double.compare(bufferInventoryPercentage, currentBufferInventoryPercentage) < 0) {
        final Double diffInPercentage = currentBufferInventoryPercentage - bufferInventoryPercentage;
        final int productsNeededForBufferInv = (int) ((diffInPercentage * totalActiveSerialProducts)/100);
        if(productsNeededForBufferInv > 0) {
          BlLogger
              .logFormatMessageInfo(LOG, Level.INFO, "{} products to be removed from buffer inventory for the product {} ",
                  productsNeededForBufferInv, blProduct.getCode());
          unMarkProductsBufferInv(productsNeededForBufferInv, blProduct);
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
   */
  private void unMarkProductsBufferInv(int productsNeededForBufferInv, BlProductModel blProduct) {
      for (BlSerialProductModel blSerialProductModel : blProduct.getSerialProducts()) {
        if (null != blSerialProductModel.getIsBufferedInventory() && BooleanUtils
            .isTrue(blSerialProductModel
                .getIsBufferedInventory())) {
          blSerialProductModel.setIsBufferedInventory(Boolean.FALSE);
          getModelService().save(blSerialProductModel);
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
   */
  private void markProductsBufferInv(int productsNeededForBufferInv, final BlProductModel blProduct) {
      final Date currentDate = Date
          .from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
      for (BlSerialProductModel blSerialProductModel : blProduct.getSerialProducts()) {
        if (null != blSerialProductModel.getIsBufferedInventory() && BooleanUtils
            .isFalse(blSerialProductModel
                .getIsBufferedInventory()) && CollectionUtils.isNotEmpty(getBlStockLevelDao()
            .checkProductAvailabilityForCurrentDate(blSerialProductModel.getCode(), currentDate,
                currentDate))) {
          blSerialProductModel.setIsBufferedInventory(Boolean.TRUE);
          getModelService().save(blSerialProductModel);
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
}
