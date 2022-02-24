package com.bl.facades.coupon;

import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.voucher.exceptions.VoucherOperationException;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;

/**
 * This Interface created to add custom logic for extend rental page promotion
 * @author Manikandan
 */
public interface BlCouponFacade {

  /**
   * This method created to add custom logic for extend rental voucher
   */
  OrderData applyVoucherForExtendOrder(final String voucherCode , final String referer , final List<String> errorList) throws VoucherOperationException;

  /**
   * This method created to add custom logic for extend rental
   */
  <R> R applyIfCartExists(final String code, final BiFunction<String, OrderModel, R> orderConsumer) throws VoucherOperationException;

  /**
   * This method created to add custom logic for extend rental promotion
   */
  OrderData releaseVoucherForExtendOrder(final String voucherCode , final String referer) throws VoucherOperationException;

  /**
   * This method created to add custom logic to remove the applied voucher from extend order page
   */
  void acceptIfCartExists(final String code, final BiConsumer<String, AbstractOrderModel> orderConsumer) throws VoucherOperationException;
  
  /**
   * Gets the order data for extended order.
   *
   * @return the order data for extended order
   */
  OrderData getOrderDataForExtendedOrder(final String referer);

}
