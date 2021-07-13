package com.bl.core.coupon;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.service.data.CouponResponse;

public interface BlCouponService {

  CouponResponse redeemCouponForExtendOrder(String couponCode, OrderModel order);

}
