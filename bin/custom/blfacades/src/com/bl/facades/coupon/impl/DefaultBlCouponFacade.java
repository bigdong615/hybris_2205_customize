package com.bl.facades.coupon.impl;

import com.bl.core.coupon.impl.DefaultBlCouponService;
import com.bl.core.utils.BlExtendOrderUtils;
import com.bl.facades.coupon.BlCouponFacade;
import de.hybris.platform.commercefacades.voucher.exceptions.VoucherOperationException;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.couponservices.service.data.CouponResponse;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.Objects;
import java.util.function.BiFunction;
import org.apache.commons.lang3.BooleanUtils;

/**
 * This class created to add custom logic for extend order promotion
 * @author Manikandan
 */
public class DefaultBlCouponFacade implements BlCouponFacade {

  private DefaultBlCouponService defaultBlCouponService;

  /**
   * This method created to add custom logic extend order promotion
   */
  @Override
  public void applyVoucherForExtendOrder(final String voucherCode)
      throws VoucherOperationException {

    ServicesUtil.validateParameterNotNullStandardMessage("coupon code", voucherCode);
    final DefaultBlCouponService var10002 = getDefaultBlCouponService();
    var10002.getClass();
    final CouponResponse couponResponse = applyIfCartExists(voucherCode , var10002::redeemCouponForExtendOrder);
    if (BooleanUtils.isNotTrue(couponResponse.getSuccess())) {
      throw new VoucherOperationException(couponResponse.getMessage());
    }

  }

  /**
   * This method created to add custom logic extend order promotion
   */
  @Override
  public <R> R applyIfCartExists(final String code, final BiFunction<String, OrderModel, R> orderConsumer) throws VoucherOperationException {

     OrderModel orderModel = null;
    if(null != BlExtendOrderUtils.getCurrentExtendOrderToSession()){
       orderModel = BlExtendOrderUtils.getCurrentExtendOrderToSession();
    }
    if (Objects.nonNull(orderModel)) {
      return orderConsumer.apply(code, orderModel);
    } else {
      throw new VoucherOperationException("No order was found in session");
    }
  }

  public DefaultBlCouponService getDefaultBlCouponService() {
    return defaultBlCouponService;
  }

  public void setDefaultBlCouponService(
      DefaultBlCouponService defaultBlCouponService) {
    this.defaultBlCouponService = defaultBlCouponService;
  }

}
