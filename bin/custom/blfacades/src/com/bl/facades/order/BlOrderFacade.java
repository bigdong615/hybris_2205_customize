package com.bl.facades.order;

import com.google.common.collect.Ordering;
import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;
import java.util.Map;
import org.springframework.ui.Model;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

public interface BlOrderFacade extends OrderFacade {

  boolean addToCartAllOrderEnrties(final String orderCode , final Model model ,final RedirectAttributes redirectAttributes , List<String> emptyCart) throws CommerceCartModificationException;

  CartModificationData addToCart(final ProductModel blProductModel, final long quantity , final AbstractOrderEntryModel abstractOrderEntryModel)  throws CommerceCartModificationException;

  OrderData calculatePriceForExtendOrders(final OrderModel orderModel, OrderData orderData, final String orderEndDate, final String selectedDate) throws CommerceCartModificationException;

  OrderData setRentalExtendOrderDetails(String orderCode , String rentalEndDate , String selectedDate)
      throws CommerceCartModificationException;

   OrderData getExtendedOrderDetailsFromOrderCode(final String orderCode);

  OrderModel getExtendOrderFromOrderModel(final OrderModel orderModel);

  OrderModel getExtendedOrderModelFromCode(final String orderCode);

  void updateOrderExtendDetails(final OrderModel orderModel);


  boolean addToCartAllRentalOrderEnrties(final String orderCode , final Model model) throws CommerceCartModificationException;

  OrderModel getOrderModelFromOrderCode(String orderCode);

  AbstractOrderModel getExtendOrderAfterPlaceingOrder(final OrderModel orderModel);

}
