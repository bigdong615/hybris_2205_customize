package com.bl.core.coupon;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.CouponService;

public interface BlCouponService extends CouponService {

  CouponResponse redeemCouponForExtendOrder(final String couponCode, final OrderModel order);

  void redeemCouponCodeForExtendOrder(final OrderModel orderModel, final String clearedCouponCode, final CouponResponse response);

}
