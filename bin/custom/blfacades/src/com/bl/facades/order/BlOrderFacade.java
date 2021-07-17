package com.bl.facades.order;

import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;

public interface BlOrderFacade extends OrderFacade {

  void addToCartAllOrderEnrties(final String orderCode) throws CommerceCartModificationException;

  void addToCart(ProductModel lProductModel, final String productCode, final int quantity) throws CommerceCartModificationException;

  OrderData calculatePriceForExtendOrders(final OrderModel orderModel, OrderData orderData, final String orderEndDate, final String selectedDate) throws CommerceCartModificationException;

  OrderData setRentalExtendOrderDetails(String orderCode , String rentalEndDate , String selectedDate)
      throws CommerceCartModificationException;

   OrderData getExtendedOrderDetailsFromOrderCode(final String orderCode);

  OrderModel getExtendOrderFromOrderModel(final OrderModel orderModel);

  OrderModel getExtendedOrderModelFromCode(final String orderCode);

  void updateOrderExtendDetails(final OrderModel orderModel);

}
