package com.bl.core.coupon.impl;

import com.bl.core.coupon.BlCouponService;
import com.bl.core.utils.BlExtendOrderUtils;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.CouponServiceException;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.impl.DefaultCouponService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;

public class DefaultBlCouponService  extends DefaultCouponService implements BlCouponService {

  public CouponResponse redeemCouponForExtendOrder(final String couponCode, final OrderModel orderModel) {
    ServicesUtil.validateParameterNotNullStandardMessage("couponCode", couponCode);
    ServicesUtil.validateParameterNotNullStandardMessage("orderModel", orderModel);
    final String clearedCouponCode = this.clearCouponCode(couponCode);
    final CouponResponse response = this.assertCouponCodeInOrder(clearedCouponCode, orderModel);
    if (BooleanUtils.isTrue(response.getSuccess())) {
      redeemCouponCodeForExtendOrder(orderModel, clearedCouponCode, response);
    }

    return response;
  }

  public void redeemCouponCodeForExtendOrder(final OrderModel orderModel, final String clearedCouponCode, final CouponResponse response) {
    try {
      if (BooleanUtils.isTrue(getCouponManagementService().redeem(clearedCouponCode, orderModel).getSuccess())) {
        final Set<String> codes = new HashSet<>();
        if (CollectionUtils.isNotEmpty(orderModel.getAppliedCouponCodes())) {
          codes.addAll(orderModel.getAppliedCouponCodes());
        }

        codes.add(clearedCouponCode);
        orderModel.setAppliedCouponCodes(codes);
         this.getModelService().save(orderModel);
         super.recalculateOrder(orderModel);
         if(orderModel.getIsExtendedOrder()) {
           BlExtendOrderUtils.setCurrentExtendOrderToSession(orderModel);
         }
      }
    } catch (CouponServiceException var5) {
      response.setSuccess(Boolean.FALSE);
      response.setMessage(var5.getMessage());
    }

  }

}
