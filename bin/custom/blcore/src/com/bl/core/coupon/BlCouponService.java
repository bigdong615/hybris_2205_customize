package com.bl.core.coupon;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.CouponService;

/**
 * This interface created to customize promotion for extend rental orders
 * @author Manikandan
 */
public interface BlCouponService extends CouponService {

  /**
   * This method created for customing coupon code for extend order
   */
  CouponResponse redeemCouponForExtendOrder(final String couponCode, final OrderModel order);

  /**
   * This method  created to calculate the extend order once coupon is applied on extend order page
   */
  void redeemCouponCodeForExtendOrder(final OrderModel orderModel, final String clearedCouponCode, final CouponResponse response);

  /**'
   * This method created to remove the applied voucher from extend order page
   */
   void releaseCouponCodeForExtendOrder(final String couponCode, final AbstractOrderModel order);

}
