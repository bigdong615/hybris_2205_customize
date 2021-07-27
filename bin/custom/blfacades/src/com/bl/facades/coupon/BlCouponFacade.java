package com.bl.facades.coupon;

import de.hybris.platform.commercefacades.voucher.exceptions.VoucherOperationException;
import de.hybris.platform.core.model.order.OrderModel;
import java.util.function.BiFunction;

/**
 * This Interface created to add custom logic for extend rental page promotion
 * @author Manikandan
 */
public interface BlCouponFacade {

  /**
   * This method created to add custom logic for extend rental voucher
   */
  void applyVoucherForExtendOrder(final String voucherCode) throws VoucherOperationException;

  /**
   * This method created to add custom logic for extend rental
   */
  <R> R applyIfCartExists(final String code, final BiFunction<String, OrderModel, R> orderConsumer) throws VoucherOperationException;

}
