package com.bl.core.product.service.impl;

import com.bl.core.enums.SerialStatusEnum;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.impl.DefaultProductService;

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
  public boolean isActiveSerialProduct(final SerialStatusEnum serialStatusEnum) {
    return null != serialStatusEnum && ("ACTIVE".equals(serialStatusEnum.getCode()) ||
        "UNBOXED".equals(serialStatusEnum.getCode()) ||
        "RECEIVED_OR_RETURNED".equals(serialStatusEnum.getCode()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAquatechProduct(final ProductModel productModel) {

    return BlCoreConstants.AQUATECH_BRAND_ID.equals(productModel.getManufacturerAID());
  }
}
