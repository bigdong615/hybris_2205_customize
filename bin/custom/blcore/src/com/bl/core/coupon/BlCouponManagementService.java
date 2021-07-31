package com.bl.core.coupon;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.CouponManagementService;

/**
 * This interface created to add custom logic for promotion
 * @author Manikandan
 */
public interface BlCouponManagementService extends CouponManagementService {

   /**
    * This method created to reedem the coupon code for extend order
    */
   CouponResponse redeemForExtendOrder(final String couponCode, final OrderModel order) ;
}
