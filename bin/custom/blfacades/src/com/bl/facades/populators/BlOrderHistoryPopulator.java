package com.bl.facades.populators;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.commercefacades.order.converters.populator.OrderHistoryPopulator;
import de.hybris.platform.commercefacades.order.data.OrderHistoryData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.payment.constants.GeneratedPaymentConstants.Enumerations.PaymentTransactionType;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.springframework.util.Assert;


/**
 * This Populator Overridden to add Rental Dates
 * @author Manikandan
 */
public class BlOrderHistoryPopulator extends OrderHistoryPopulator {

  /**
   * This Method Populate Rental Dates to target to display on storefront
   */
  @Override
  public void populate(final OrderModel source, final OrderHistoryData target)
  {
    Assert.notNull(source, "Parameter source cannot be null.");
    Assert.notNull(target, "Parameter target cannot be null.");

    target.setCode(source.getCode());
    target.setGuid(source.getGuid());
    target.setPlaced(source.getDate());
    target.setStatus(source.getStatus());
    target.setStatusDisplay(source.getStatusDisplay());
    if (source.getTotalPrice() != null)
    {
      target.setTotal(getPriceDataFactory().create(PriceDataType.BUY, updateTotalIfOrderExtended(source), source.getCurrency()));
    }
   if(source.isGiftCardOrder())
    {
   	target.setIsGiftCard(Boolean.TRUE);
    }
   if(BooleanUtils.isTrue(source.getIsRetailGearOrder()))
    {
   	target.setRetailGearOrder(Boolean.TRUE);
    }

   if(null != source.getRentalStartDate()){
    target.setRentalStartDate(convertDateToString(source.getRentalStartDate()));
  }
   if(null != source.getRentalEndDate()) {
     target.setRentalEndDate(convertDateToString(source.getRentalEndDate()));
   }
   target.setRentalCart(source.getIsRentalOrder());
   target.setOrderDate(convertDateToString(source.getDate()));
   if(source.isGiftCardOrder() || BooleanUtils.isFalse(source.getIsRentalOrder())) {
     final List<String> productQtyAndName = new ArrayList<>();
     for (AbstractOrderEntryModel abstractOrderEntryModel : source.getEntries()) {
       final ProductModel product = abstractOrderEntryModel.getProduct();
       if(source.isGiftCardOrder()){
      	 target.setProductCode(product.getCode());
       }
       else if(product instanceof BlSerialProductModel) {
         final BlProductModel productModel = ((BlSerialProductModel) product).getBlProduct();
         productQtyAndName.add(abstractOrderEntryModel.getQuantity() + BlFacadesConstants.BLANK + BlFacadesConstants.PRODUCT_SEPERATOR +
             BlFacadesConstants.BLANK + productModel.getName());
       }
     }
     target.setProductNameAndQuantity(productQtyAndName);
   }
   if(CollectionUtils.isNotEmpty(source.getExtendedOrderCopyList())) {
     updateRentalDetailsIfExtendOrderExist(source, target);
   }


    target.setOrderStatus(BooleanUtils.isTrue(source.getIsRentalOrder()) ? setRentalOrderStatus(source) : setUsedOrderStatus(source));

    if(null != source.getRentalStartDate() && null != source.getRentalEndDate() ){
     target.setIsRentalActive(isRentalCartAcive(source));
     if(source.getPaymentTransactions().stream().anyMatch(paymentTransactionModel ->
          paymentTransactionModel.getEntries().stream().anyMatch(paymentTransactionEntryModel -> paymentTransactionEntryModel.getType().getCode().equalsIgnoreCase(
              PaymentTransactionType.CAPTURE))) || (CollectionUtils.isNotEmpty(source.getGiftCard()) && Double.compare(source.getTotalPrice(), 0.0) == 0)) {
       target.setIsRentalStartDateActive(Boolean.TRUE);
     }
   }
   target.setOrderReturnedToWarehouse(source.isOrderReturnedToWarehouse());
   final AtomicDouble totalAmt = new AtomicDouble(0.0);

    if (source.getStatus().getCode().startsWith(OrderStatus.INCOMPLETE.getCode())
        && BooleanUtils.isTrue(source.isOrderReturnedToWarehouse())) {
	 source.getConsignments()
			  .forEach(consignment -> consignment.getConsignmentEntries().forEach(consignmentEntry -> consignmentEntry
					  .getBillingCharges().forEach((serialCode, listOfCharges) -> listOfCharges.forEach(billing -> {
					    if(BooleanUtils.isFalse(billing.isBillPaid())) {
                totalAmt.addAndGet(billing.getChargedAmount().doubleValue());
              }
					  }))));
	 }

	  target.setPayBillingCost(convertDoubleToPriceData(totalAmt.get(), source));


    if (null != source.getReturnRequestForOrder() && BooleanUtils.isTrue(source.getIsReplacementOrder())) {
      target.setReplacementFor(source.getReturnRequestForOrder().getOrder().getCode());
      target.setIsReplacementOrder(Boolean.TRUE);
    }

      if(BooleanUtils.isTrue(source.getIsCaptured())){
          target.setIsCaptured(Boolean.TRUE);
      }else{
          target.setIsCaptured(Boolean.FALSE);
      }

//    BLS-67 Update Order Status Terminology in Order Details
      populateUpdatedOrderStatus(source, target);
  }

  /**
 * @param source
 * @param target
 */
private void populateUpdatedOrderStatus(OrderModel source, OrderHistoryData target)
{
	switch (source.getStatus())
	{
		case INCOMPLETE_BALANCE_DUE:
			
			target.setOrderStatus("Incomplete");
			break;

		case INCOMPLETE_ITEMS_IN_REPAIR:
			
			target.setOrderStatus("Incomplete");
			break;

		case INCOMPLETE_LOST_IN_TRANSIT:
			
			target.setOrderStatus("Incomplete");
			break;

		case INCOMPLETE_MISSING_ITEMS:
			
			target.setOrderStatus("Incomplete");
			break;

		case INCOMPLETE_STOLEN:
			
			target.setOrderStatus("Incomplete");
			break;

		case LATE:
			
			target.setOrderStatus("Order Not Returned");
			break;

		case PAYMENT_CAPTURED:
			
			target.setOrderStatus("Order Placed");
			break;

		case PAYMENT_NOT_AUTHORIZED:
			
			target.setOrderStatus("Payment Declined");
			break;

		case PENDING:
			
			target.setOrderStatus("Order Placed");
			break;

		case RECEIVED_IN_VERIFICATION:
			
			target.setOrderStatus("Verfication Required");
			break;

		case RECEIVED_PAYMENT_DECLINED:
			
			target.setOrderStatus("Payment Declined");
			break;

		case RECEIVED_READY_FOR_PICKUP:
			
			target.setOrderStatus("Ready for Pickup");
			break;

		case SOLD:
			
			target.setOrderStatus("Sold - Order Placed");
			break;

		case SOLD_SHIPPED:
			
			target.setOrderStatus("Sold - Order Shipped");
			break;

		case SUSPENDED:
			
			target.setOrderStatus("Contact Customer Service");
			break;

		case UNBOXED_COMPLETELY:
			
			target.setOrderStatus("Returned to Warehouse");
			break;

		case UNBOXED_PARTIALLY:
			
			target.setOrderStatus("Returned to Warehouse");
			break;

		case VERIFICATION_REQUIRED:
			
			target.setOrderStatus("Verfication Required");
			break;

		case WAIT_FRAUD_MANUAL_CHECK:
			
			target.setOrderStatus("Verfication Required");
			break;

			
		default:
			break;
	}
	
}

/**
   * This method created to update the total price if order is extended
   */
  private BigDecimal updateTotalIfOrderExtended(final OrderModel orderModel) {

    BigDecimal price = BigDecimal.valueOf(orderModel.getTotalPrice());
    if(CollectionUtils.isNotEmpty(orderModel.getExtendedOrderCopyList())) {

      for(final AbstractOrderModel extendOrder : orderModel.getExtendedOrderCopyList()){
        if(BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) &&
            extendOrder.getExtendOrderStatus().getCode().equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())){
          price = price.add(BigDecimal.valueOf(extendOrder.getTotalPrice()));
        }
      }
    }
    return price;
  }

  /**
   * This Method converts rental startDate and rental endDate to String
   */
  private String convertDateToString(final Date rentalDate) {
    return BlDateTimeUtils.convertDateToStringDate(rentalDate, BlFacadesConstants.RENTAL_DATE_FORMAT);
  }

  /**
   * This method created to update rental details if order already contains extend order
   */
  private void updateRentalDetailsIfExtendOrderExist(final OrderModel orderModel , final OrderHistoryData orderData){
    final List<AbstractOrderModel> orderModelList = orderModel.getExtendedOrderCopyList();
    final int size = orderModelList.size();
    for (final AbstractOrderModel extendOrder :orderModelList) {
      if (BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) && extendOrder
          .getExtendOrderStatus().getCode()
          .equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())
          && orderModelList.get(size - 1).getPk()
          .equals(extendOrder.getPk())) {
        orderData.setRentalEndDate(convertDateToString(extendOrder.getRentalEndDate()));
      }
    }
  }

  /**
   * This method created to check whether rental order is active or not
   */
  private boolean isRentalCartAcive(final OrderModel orderModel){
    final Date date = new Date();
    return (date.before(orderModel.getRentalStartDate()) || DateUtils.isSameDay(orderModel.getRentalStartDate(), date)) || (date.before(orderModel.getRentalEndDate())
        || DateUtils.isSameDay(orderModel.getRentalEndDate(), date));
  }


  /**
   * This method converts double to price data
   */
  private PriceData convertDoubleToPriceData(final Double price , OrderModel orderModel) {
    return getPriceDataFactory().create(PriceDataType.BUY ,BigDecimal.valueOf(price),orderModel.getCurrency());
  }


  /**
   * This method created to set rental gear order status
   * @param abstractOrderModel abstractOrderModel
   * @return order status as String
   */
  private String setRentalOrderStatus(final AbstractOrderModel abstractOrderModel) {

    final AtomicReference<String> orderStatus = new AtomicReference<>();
    if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.PENDING.getCode()) ||
        abstractOrderModel.getPaymentTransactions().stream().noneMatch(paymentTransactionModel ->
            paymentTransactionModel.getEntries().stream().noneMatch(paymentTransactionEntryModel -> paymentTransactionEntryModel.getType().getCode().equalsIgnoreCase(
            PaymentTransactionType.CAPTURE)))) {
      orderStatus.set(BlFacadesConstants.PENDING);
    }

    if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.PAYMENT_CAPTURED.getCode()) || isOrderCaptured(abstractOrderModel)) {
      orderStatus.set(BlFacadesConstants.SHIPPED);
    }


    if(BooleanUtils.isTrue(abstractOrderModel.isOrderReturnedToWarehouse()) && abstractOrderModel.getStatus().getCode().equalsIgnoreCase(
        OrderStatus.UNBOXED_COMPLETELY.getCode())) {
      orderStatus.set(BlFacadesConstants.RETURNED);
    }

    if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.COMPLETED.getCode())){
      orderStatus.set(BlFacadesConstants.COMPLETED);
    }

    if (BooleanUtils.isTrue(abstractOrderModel.isOrderReturnedToWarehouse())) {
      abstractOrderModel.getConsignments().forEach(
          consignmentModel -> consignmentModel.getConsignmentEntries()
              .forEach(consignmentEntryModel -> {
                consignmentEntryModel.getBillingCharges()
                    .forEach((serialCode, listOfCharges) -> listOfCharges.forEach(billing -> {
                      if (BooleanUtils.isFalse(billing.isBillPaid())) {
                        orderStatus.set(BlFacadesConstants.INCOMPLETE);
                      } else if (BooleanUtils.isTrue(billing.isBillPaid())) {
                        orderStatus.set(BlFacadesConstants.COMPLETED);
                      }
                    }));
              }));
    }


    if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.CANCELLED.getCode())) {
      orderStatus.set(BlFacadesConstants.CANCELED);
    }

    if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.LATE.getCode())){
      orderStatus.set(BlFacadesConstants.LATE);
    }

    return orderStatus.get();
  }

/**
 * Checks if is order captured.
 *
 * @param abstractOrderModel the abstract order model
 * @return true, if is order captured
 */
private boolean isOrderCaptured(final AbstractOrderModel abstractOrderModel)
{
	return abstractOrderModel.getPaymentTransactions().stream().anyMatch(paymentTransactionModel ->
        paymentTransactionModel.getEntries().stream().anyMatch(paymentTransactionEntryModel -> {
      	  return paymentTransactionEntryModel.getType().getCode().equalsIgnoreCase(
                 PaymentTransactionType.CAPTURE) && TransactionStatus.ACCEPTED.name().equals(paymentTransactionEntryModel.getTransactionStatus())
     				&& paymentTransactionEntryModel.getTransactionStatusDetails().startsWith(TransactionStatusDetails.SUCCESFULL.name());
        }));
}


  /**
   * This method created to set used gear order status
   * @param abstractOrderModel  abstractOrderModel
   * @return order status as a string
   */
  private String setUsedOrderStatus(final  AbstractOrderModel abstractOrderModel){
    String orderStatus = StringUtils.EMPTY;
    if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.PENDING.getCode()) || abstractOrderModel.getPaymentTransactions().stream().noneMatch(paymentTransactionModel ->
        paymentTransactionModel.getEntries().stream().noneMatch(paymentTransactionEntryModel -> paymentTransactionEntryModel.getType().getCode().equalsIgnoreCase(
            PaymentTransactionType.CAPTURE)))) {
      orderStatus =  BlFacadesConstants.SOLD;
    }


    if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.PAYMENT_CAPTURED.getCode()) || isOrderCaptured(abstractOrderModel)) {
      orderStatus = BlFacadesConstants.SHIPPED;
    }

    final Calendar calendar = Calendar.getInstance();
    calendar.setTime(abstractOrderModel.getDate());
    calendar.add(Calendar.DAY_OF_MONTH ,30);
    Date addedDate = calendar.getTime();
     if(DateUtils.isSameDay(addedDate , new Date()) || new Date().after(addedDate)){
       orderStatus =  BlFacadesConstants.COMPLETED;
    }

     if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.CANCELLED.getCode())) {
       orderStatus = BlFacadesConstants.CANCELED;
     }

     if(abstractOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.RETURNED.getCode())){
       orderStatus = BlFacadesConstants.RETURNED;
     }

    return orderStatus;
  }

}
