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
      final BigDecimal totalPrice = BigDecimal.valueOf(source.getTotalPrice());
      target.setTotal(getPriceDataFactory().create(PriceDataType.BUY, totalPrice, source.getCurrency()));
    }

   if(null != source.getRentalStartDate()){
    target.setRentalStartDate(convertDateToString(source.getRentalStartDate()));
  }
   if(null != source.getRentalEndDate()) {
     target.setRentalEndDate(convertDateToString(source.getRentalEndDate()));
   }
   target.setRentalCart(source.getIsRentalCart());
   target.setOrderDate(convertDateToString(source.getDate()));
   if(BooleanUtils.isFalse(source.getIsRentalCart())) {
     final List<String> productQtyAndName = new ArrayList<>();
     for (AbstractOrderEntryModel abstractOrderEntryModel : source.getEntries()) {
       final ProductModel product = abstractOrderEntryModel.getProduct();
       if(product instanceof BlSerialProductModel) {
         final BlProductModel productModel = ((BlSerialProductModel) product).getBlProduct();
         productQtyAndName.add(abstractOrderEntryModel.getQuantity() + " x " + productModel.getName());
       }
     }
     target.setProductNameAndQuantity(productQtyAndName);
   }
   if(CollectionUtils.isNotEmpty(source.getExtendedOrderCopyList())) {
     updateRentalDetailsIfExtendOrderExist(source, target);
   }
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
        final BigDecimal totalPrice = BigDecimal.valueOf(extendOrder.getTotalPrice());
        orderData.setTotal(getPriceDataFactory().create(PriceDataType.BUY, totalPrice, extendOrder.getCurrency()));
      }
    }

  }

}
