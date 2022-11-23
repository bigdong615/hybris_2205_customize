package com.bl.facades.order;

import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import org.springframework.ui.Model;

/**
 * This Interface created to add custom logic for extend rental order and rent again
 * @author Manikandan
 */
public interface BlOrderFacade extends OrderFacade {

  /**
   * This method create to add all the products to cart from rent again
   */
  boolean addToCartAllOrderEnrties(final String orderCode , final Model model) throws CommerceCartModificationException;

  /**
   * This method created to add custom logic for rent again before adding to cart
   */
  CartModificationData addToCart(final ProductModel blProductModel, final long quantity , final AbstractOrderEntryModel abstractOrderEntryModel)  throws CommerceCartModificationException;

  /**
   * This method created to add custom logic for extend order product price based on extend rental date
   */
  OrderData calculatePriceForExtendOrders(final OrderModel orderModel, OrderData orderData, final String orderEndDate, final String selectedDate) throws CommerceCartModificationException;

  /**
   * This method created to set extend rental details
   */
  OrderData setRentalExtendOrderDetails(final String orderCode , final String rentalEndDate , final String selectedDate)
      throws CommerceCartModificationException;

  /**
   * This method created to get extend order details  from order code
   */
   OrderData getExtendedOrderDetailsFromOrderCode(final String orderCode);

  /**
   * This method created to get extend order from order model
   */
  OrderModel getExtendOrderFromOrderModel(final OrderModel orderModel);

  /**
   * This method is created t get extend order from code
   * @param orderCode
   * @return
   */
  OrderModel getExtendedOrderModelFromCode(final String orderCode);

  /**
   * This method created to updare the extend order details
   */
  void updateOrderExtendDetails(final OrderModel orderModel);

  /**
   * This method created to get order model from order code
   */
  OrderModel getOrderModelFromOrderCode(final String orderCode);

  /**
   * This method created to update the extend order after extending order successfully
   * @param orderModel
   * @return
   */
  AbstractOrderModel getExtendOrderAfterPlaceingOrder(final OrderModel orderModel);

  /**
   * This method created to save the PO payment
   * @param poNumber
   * @param poNotes
   */
  boolean savePoPaymentForExtendOrder(final String poNumber , final String poNotes , final String orderCode);
  
  /**
   * This method created to save the PO payment
   * @param poNumber
   * @param poNotes
   */
  boolean savePoPaymentForPayBillOrder(final String poNumber , final String poNotes , final String orderCode);

  /**
   * It populated all the fields needed for bill pay
   * @param target
   */
  public void setPayBillAttributes(final OrderData target);
  
  /**
   * Sets the resolved status on repair log for order code.
   *
   * @param orderCode the new resolved status on repair log
   */
  public void setResolvedStatusOnRepairLog(final String orderCode);

  /**
   * This method used to get pending date which is used for tracking shareASale lead pixel.
   * @param orderCode
   * @return
   */
   String getPendingDate(final String orderCode);

}
