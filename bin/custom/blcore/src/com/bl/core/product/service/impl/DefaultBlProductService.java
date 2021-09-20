package com.bl.core.product.service.impl;

import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.catalog.daos.CatalogVersionDao;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.product.daos.ProductDao;
import de.hybris.platform.product.impl.DefaultProductService;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.user.UserService;
import java.util.List;
import java.util.Objects;

import java.util.stream.Collectors;
import java.util.Collection;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
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
  public void setLastUserChangedConditionRating(BlSerialProductModel blSerialProduct)
  {
	  final UserModel currentUser = getUserService().getCurrentUser();
	  if (Objects.nonNull(currentUser))
	  {
		  final String currentUserUid = currentUser.getUid();
		  BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Current user id : {}", currentUserUid);
		  blSerialProduct.setUserChangedConditionRating(currentUserUid);
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
    Collection<CatalogVersionModel> catalogModels =  getCatalogVersionDao().findCatalogVersions(BlCoreConstants
        .CATALOG_VALUE, BlCoreConstants.STAGED);
    if(CollectionUtils.isNotEmpty(catalogModels)) {
      List<BlSerialProductModel> products = getProductsOfStagedVersion(productCode,
          catalogModels.iterator().next());
      if (CollectionUtils.isNotEmpty(products)) {
        BlSerialProductModel product = products.get(0);
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
 * @return the userService
 */
public UserService getUserService()
{
	return userService;
}

/**
 * @param userService the userService to set
 */
public void setUserService(UserService userService)
{
	this.userService = userService;
}

public CatalogVersionDao getCatalogVersionDao() {
    return catalogVersionDao;
  }

public void setCatalogVersionDao(CatalogVersionDao catalogVersionDao) {
  this.catalogVersionDao = catalogVersionDao;
}

public SearchRestrictionService getSearchRestrictionService() {
  return searchRestrictionService;
}

public void setSearchRestrictionService(
  SearchRestrictionService searchRestrictionService) {
  this.searchRestrictionService = searchRestrictionService;
}

}
