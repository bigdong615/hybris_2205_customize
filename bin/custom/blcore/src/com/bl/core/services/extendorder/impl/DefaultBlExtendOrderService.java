package com.bl.core.services.extendorder.impl;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.services.extendorder.BlExtendOrderService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Collections;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;

public class DefaultBlExtendOrderService implements BlExtendOrderService {

  private ModelService modelService;
  private KeyGenerator orderIDGenerator;

  @Override
  public OrderModel cloneOrderModelForExtendRental(final OrderModel originalOrder) {

    if(null == originalOrder.getExtendedOrderCopy()) {
      OrderModel extendOrderModel = getModelService().clone(originalOrder);
      extendOrderModel.setTotalDiscounts(0.0);
      extendOrderModel.setAppliedCouponCodes(Collections.emptyList());
      extendOrderModel.setDeliveryCost(0.0);
      extendOrderModel.setAllPromotionResults(Collections.emptySet());
      extendOrderModel.setTotalTax(0.0);
      extendOrderModel.setTotalTaxValues(Collections.emptyList());
      extendOrderModel.setDeliveryCost(0.0);
      extendOrderModel.setAvalaraTaxCalculated(false);
      extendOrderModel.setCalculated(false);
      extendOrderModel.setVersionID(String.valueOf(getOrderIDGenerator().generate()));
      extendOrderModel.setIsExtendedOrder(true);
      extendOrderModel.setExtendOrderStatus(ExtendOrderStatusEnum.PROCESSING);
      originalOrder.setExtendedOrderCopy(extendOrderModel);
      getModelService().save(originalOrder);
      getModelService().refresh(originalOrder);
      return extendOrderModel;
    }
    return originalOrder.getExtendedOrderCopy();

  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public KeyGenerator getOrderIDGenerator() {
    return orderIDGenerator;
  }

  public void setOrderIDGenerator(KeyGenerator orderIDGenerator) {
    this.orderIDGenerator = orderIDGenerator;
  }



}
