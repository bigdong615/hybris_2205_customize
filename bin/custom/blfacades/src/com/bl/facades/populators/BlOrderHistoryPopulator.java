package com.bl.facades.populators;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import de.hybris.platform.commercefacades.order.converters.populator.OrderHistoryPopulator;
import de.hybris.platform.commercefacades.order.data.OrderHistoryData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
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

   if(null != source.getRentalStartDate()){
    target.setRentalStartDate(convertDateToString(source.getRentalStartDate()));
  }
   if(null != source.getRentalEndDate()) {
     target.setRentalEndDate(convertDateToString(source.getRentalEndDate()));
   }
   target.setRentalCart(source.getIsRentalCart());
   target.setOrderDate(convertDateToString(source.getDate()));
   if(source.isGiftCardOrder() || BooleanUtils.isFalse(source.getIsRentalCart())) {
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

   if(null != source.getRentalStartDate() && null != source.getRentalEndDate()){
     target.setIsRentalActive(isRentalCartAcive(source));
     target.setIsRentalStartDateActive(isExtendOrderButtonEnable(source));
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

  private boolean isExtendOrderButtonEnable(final OrderModel orderModel){
    return DateUtils.isSameDay(orderModel.getRentalStartDate() , new Date()) || new Date().after(orderModel.getRentalStartDate());
  }

}
