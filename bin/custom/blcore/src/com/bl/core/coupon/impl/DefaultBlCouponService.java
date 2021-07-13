package com.bl.core.coupon.impl;

import com.bl.core.coupon.BlCouponService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.CouponServiceException;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.impl.DefaultCouponService;
import de.hybris.platform.servicelayer.util.ServicesUtil;

public class DefaultBlCouponService  extends DefaultCouponService implements BlCouponService {

  @Override
  public CouponResponse redeemCouponForExtendOrder(String couponCode, OrderModel order) {
    ServicesUtil.validateParameterNotNullStandardMessage("couponCode", couponCode);
    ServicesUtil.validateParameterNotNullStandardMessage("order", order);
    String clearedCouponCode = this.clearCouponCode(couponCode);
    if (!this.containsCouponCode(clearedCouponCode, order)) {
      throw new CouponServiceException("Cannot apply couponCode '" + couponCode + "'. It is already applied to order " + order.getCode());
    } else {
      return this.getCouponManagementService().redeem(clearedCouponCode, order);
    }
  }

  protected String clearCouponCode(String couponCode) {
    return couponCode.trim();
  }

}
