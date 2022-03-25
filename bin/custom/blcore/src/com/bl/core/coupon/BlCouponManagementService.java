package com.bl.core.coupon;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.CouponManagementService;

/**
 * This interface created to add custom logic for promotion
 * @author Manikandan
 */
public interface BlCouponManagementService extends CouponManagementService {

   /**
    * This method created to reedem the coupon code for extend order.
    *
    * @param couponCode the coupon code
    * @param order the order
    * @return the coupon response
    */
   CouponResponse redeemForExtendOrder(final String couponCode, final OrderModel order);
   
   /**
    * Checks if is coupon available for use.
    *
    * @param couponCode the coupon code
    * @param user the user
    * @return true, if is coupon available for use
    */
   boolean isCouponAvailableForUse(final String couponCode, final UserModel user);
}
