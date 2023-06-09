package com.bl.core.product.service.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.catalog.daos.CatalogVersionDao;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.product.impl.DefaultProductService;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.stock.BlStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;


/**
 * Extended Custom Product Service to derive custom business logic
 *
 * @author Ravikumar
 */
public class DefaultBlProductService extends DefaultProductService implements BlProductService {

  private static final Logger LOG = Logger.getLogger(DefaultBlProductService.class);

  private UserService userService;
  private CatalogVersionDao catalogVersionDao;
  private SearchRestrictionService searchRestrictionService;
  private BlProductDao blProductDao;
  private BlStockService blStockService;
  private BlCommerceStockService blCommerceStockService;

	@Resource(name = "blStockLevelDao")
	private BlStockLevelDao blStockLevelDao;

	/**
	 * {@inheritDoc}
	 */
  @Override
  public boolean isFunctionalAndCosmeticIsAvailable(
      final BlSerialProductModel blSerialProductModel) {
    boolean isEligible = true;
    if (Objects.isNull(blSerialProductModel.getFunctionalRating())
        || StringUtils.equalsIgnoreCase(blSerialProductModel.getFunctionalRating().getCode(),
        BlCoreConstants.ZERO_RATING)) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
          "Cannot evaluate conditional overall rating because functional rating is null or it is 0 on serial {}",
          blSerialProductModel.getProductId());
      isEligible = false;
    }
    if (Objects.isNull(blSerialProductModel.getCosmeticRating())
        || StringUtils.equalsIgnoreCase(blSerialProductModel.getCosmeticRating().getCode(),
        BlCoreConstants.ZERO_RATING)) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
          "Cannot evaluate conditional overall rating because cosmetic rating is null or it is 0 on serial {}",
          blSerialProductModel.getProductId());
      isEligible = false;
    }
    return isEligible;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setLastUserChangedConditionRating(final BlSerialProductModel blSerialProduct)
  {
	  final UserModel currentUser = getUserService().getCurrentUser();
	  if (Objects.nonNull(currentUser))
	  {
		  final String currentUserUid = currentUser.getUid();
		  BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Current user id : {}", currentUserUid);
		  if(!currentUserUid.equalsIgnoreCase(BlCoreConstants.ANONYMOUS))
		  {
			  blSerialProduct.setUserChangedConditionRating(currentUserUid);
		  }
	  }
	  else
	  {
		  BlLogger.logMessage(LOG, Level.ERROR, "Unable to fetch current user from session");
	  }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAquatechProduct(final ProductModel productModel) {

    return BlCoreConstants.AQUATECH_BRAND_ID.equals(productModel.getManufacturerAID());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<ProductReferenceModel> getBundleProductReferenceModelFromEntry(final AbstractOrderEntryModel parentBundleEntry) {
    return getBundleProductReferenceModel(parentBundleEntry.getProduct());
  }

  public void changeBufferInvFlagInStagedVersion(final String productCode, final Boolean isBufferInventory) {
    final Collection<CatalogVersionModel> catalogModels =  getCatalogVersionDao().findCatalogVersions(BlCoreConstants
        .CATALOG_VALUE, BlCoreConstants.STAGED);
    if(CollectionUtils.isNotEmpty(catalogModels)) {
      final List<BlSerialProductModel> products = getProductsOfStagedVersion(productCode,
          catalogModels.iterator().next());
      if (CollectionUtils.isNotEmpty(products)) {
        final BlSerialProductModel product = products.get(0);
        product.setIsBufferedInventory(isBufferInventory);
        getModelService().save(product);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<ProductReferenceModel> getBundleProductReferenceModel(final ProductModel product){
    return product.getProductReferences().stream()
        .filter(productReferenceModel -> ProductReferenceTypeEnum.CONSISTS_OF
            .equals(productReferenceModel.getReferenceType())).collect(Collectors.toList());
  }

  public List<BlSerialProductModel> getProductsOfStagedVersion(final String productCode,
      final CatalogVersionModel catalogVersionModel) {
    return getSessionService().executeInLocalView(new SessionExecutionBody()
    {
      @Override
      public Object execute()
      {
        try
        {
          getSearchRestrictionService().disableSearchRestrictions();
          return getProductDao().findProductsByCode(catalogVersionModel,
              productCode);
        }
        finally
        {
          getSearchRestrictionService().enableSearchRestrictions();
        }
      }
    });
  }

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updateStockForCancelledProduct(final BlProductModel serialProduct, final Date optimizedShippingStartDate,
			Date optimizedShippingEndDate)
	{
        if(null == optimizedShippingEndDate) {
            optimizedShippingEndDate = BlDateTimeUtils.getNextYearsSameDay();
        }
		final Collection<StockLevelModel> findSerialStockLevelForDate = blStockLevelDao
				.findSerialStockLevelForDate(serialProduct.getCode(), optimizedShippingStartDate, optimizedShippingEndDate);
		if (CollectionUtils.isNotEmpty(findSerialStockLevelForDate))
		{
			findSerialStockLevelForDate.forEach(stockLevel -> {
				stockLevel.setHardAssigned(false);
				stockLevel.setReservedStatus(false);
				stockLevel.setOrder(null);
				((BlSerialProductModel) serialProduct).setHardAssigned(false); // NOSONAR
				getModelService().save(stockLevel);
				getModelService().save(serialProduct);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Reserved status set to {} and Hard Assigned set to {} for serial {}",
						stockLevel.getReservedStatus(), stockLevel.getHardAssigned(), serialProduct.getCode());
			});
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock level updated for serial {}", serialProduct.getCode());
		}
	}

	@Override
	public List<BlProductModel> getUsedProductsOnSale()
	{
		final List<BlProductModel> products = new ArrayList<BlProductModel>();
		for (final BlProductModel product : getBlProductDao().getUsedProductsOnSale())
		{
			if (product instanceof BlProductModel && !product.getSerialProducts().isEmpty())
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Current Used gear Product id for Google Feed : {}",
						product.getCode());
				outerloop:
				for (final BlSerialProductModel serial : product.getSerialProducts())
				{
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Current Used gear Serial Product id for Google Feed : {}",
							serial.getCode());
					if (getBlStockService().isVisibleInPdp(serial.getSerialStatus()) && serial.getForSale()
							&& getBlCommerceStockService().isUsedGearSerialNotAssignedToRentalOrder(serial.getCode())
							&& serial.getApprovalStatus().getCode().equalsIgnoreCase("approved"))
					{
						BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
								"Current Used gear Serial Product id for Google Feed is avaialble and added to feed : {}",
								serial.getCode());
						products.add(product);
						break outerloop;
					}
					else
					{
						BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
								"Current Used gear Serial Product id for Google Feed is not avaialble and removed to feed : {}",
								serial.getCode());
					}
				}
			}
		}
		return products;
	}

	@Override
	public ProductModel getProductForPK(final String code)
	{
		validateParameterNotNull(code, "Parameter code must not be null");
		final List<ProductModel> products = blProductDao.findProductsByPK(code);
		if (products != null)
		{
			return products.get(0);
		}
		return null;
	}



/**
 * @return the userService
 */
public UserService getUserService()
{
	return userService;
}

/**
 * @param userService the userService to set
 */
public void setUserService(final UserService userService)
{
	this.userService = userService;
}

public CatalogVersionDao getCatalogVersionDao() {
    return catalogVersionDao;
  }

public void setCatalogVersionDao(final CatalogVersionDao catalogVersionDao) {
  this.catalogVersionDao = catalogVersionDao;
}

public SearchRestrictionService getSearchRestrictionService() {
  return searchRestrictionService;
}

public void setSearchRestrictionService(
  final SearchRestrictionService searchRestrictionService) {
  this.searchRestrictionService = searchRestrictionService;
}

public BlProductDao getBlProductDao()
{
	return blProductDao;
}

public void setBlProductDao(final BlProductDao blProductDao)
{
	this.blProductDao = blProductDao;
}

public BlStockService getBlStockService()
{
	return blStockService;
}

public void setBlStockService(final BlStockService blStockService)
{
	this.blStockService = blStockService;
}

public BlCommerceStockService getBlCommerceStockService()
{
	return blCommerceStockService;
}

public void setBlCommerceStockService(final BlCommerceStockService blCommerceStockService)
{
	this.blCommerceStockService = blCommerceStockService;
}

}
