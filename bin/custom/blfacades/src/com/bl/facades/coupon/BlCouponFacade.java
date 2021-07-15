package com.bl.facades.coupon;

import de.hybris.platform.commercefacades.voucher.exceptions.VoucherOperationException;
import de.hybris.platform.core.model.order.OrderModel;
import java.util.function.BiFunction;

public interface BlCouponFacade {

  void applyVoucherForExtendOrder(final String voucherCode) throws VoucherOperationException;

  <R> R applyIfCartExists(String code, BiFunction<String, OrderModel, R> orderConsumer) throws VoucherOperationException;

}
