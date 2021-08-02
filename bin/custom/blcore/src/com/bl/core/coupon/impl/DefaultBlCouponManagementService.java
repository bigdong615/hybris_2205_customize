package com.bl.core.coupon.impl;

import com.bl.core.coupon.BlCouponManagementService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.CouponServiceException;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.impl.DefaultCouponManagementService;
import org.apache.commons.lang.BooleanUtils;

/**
 * This class is created to redeem the extend order
 * @author Manikandan
 */
public class DefaultBlCouponManagementService extends DefaultCouponManagementService implements BlCouponManagementService {

  /**
   * This method created to redeem the coupon for extend order
   */
  @Override
  public CouponResponse redeemForExtendOrder(final String couponCode, final OrderModel order) {
    final CouponResponse response = this.verifyCouponCode(couponCode, order);
    if (BooleanUtils.isTrue(response.getSuccess())) {
      this.createCouponRedemption(couponCode, order);
    }
    else {
     throw new CouponServiceException(response.getMessage());
    }
    return response;
  }
}
