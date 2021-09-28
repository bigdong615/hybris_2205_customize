package com.bl.core.coupon.impl;

import com.bl.core.coupon.BlCouponService;
import com.bl.core.utils.BlExtendOrderUtils;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.CouponServiceException;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.couponservices.services.impl.DefaultCouponService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;

/**
 * This class is created to customize when coupon is applied from exend rental page
 * @author Manikandan
 */
public class DefaultBlCouponService extends DefaultCouponService implements BlCouponService {

  private DefaultBlCouponManagementService defaultBlCouponManagementService;

    /**
    * This method created for customize coupon code for extend order
    */
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

   /**
   * This method os created to calculate the extend order once coupon is applied on extend order page
   */
  public void redeemCouponCodeForExtendOrder(final OrderModel orderModel, final String clearedCouponCode, final CouponResponse response) {
    try {
      if (BooleanUtils.isTrue(getDefaultBlCouponManagementService().redeemForExtendOrder(clearedCouponCode, orderModel).getSuccess())) {
        final Set<String> codes = new HashSet<>();
        if (CollectionUtils.isNotEmpty(orderModel.getAppliedCouponCodes())) {
          codes.addAll(orderModel.getAppliedCouponCodes());
        }
        codes.add(clearedCouponCode);
        orderModel.setAppliedCouponCodes(codes);
         this.getModelService().save(orderModel);
         super.recalculateOrder(orderModel);
         if(BooleanUtils.isTrue(orderModel.getIsExtendedOrder())) {
           BlExtendOrderUtils.setCurrentExtendOrderToSession(orderModel);
         }
      }
    } catch (CouponServiceException var5) {
      response.setSuccess(Boolean.FALSE);
      response.setMessage(var5.getMessage());
    }
  }

  /**
   * This method created to remove the applies coupon from extend order page
   */
  @Override
  public void releaseCouponCodeForExtendOrder(final String couponCode, final AbstractOrderModel order) {
    ServicesUtil.validateParameterNotNullStandardMessage("couponCode", couponCode);
    ServicesUtil.validateParameterNotNullStandardMessage("order", order);
    this.getCouponManagementService().releaseCouponCode(couponCode);
    this.removeCouponAndTriggerCalculation(couponCode, order);

    if(BooleanUtils.isTrue(order.getIsExtendedOrder())) {
      BlExtendOrderUtils.setCurrentExtendOrderToSession(order);
    }
  }


  public DefaultBlCouponManagementService getDefaultBlCouponManagementService() {
    return defaultBlCouponManagementService;
  }

  public void setDefaultBlCouponManagementService(
      DefaultBlCouponManagementService defaultBlCouponManagementService) {
    this.defaultBlCouponManagementService = defaultBlCouponManagementService;
  }

}
