package com.bl.core.product.service.impl;

import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.product.impl.DefaultProductService;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.Objects;

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

}
