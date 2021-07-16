package com.bl.core.services.extendorder.impl;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.services.extendorder.BlExtendOrderService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Collections;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;

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


      getModelService().save(extendOrderModel);
      getModelService().refresh(extendOrderModel);

      final Set<ConsignmentModel> clonedList = new HashSet<>();

      for(ConsignmentModel consignmentModel : originalOrder.getConsignments()) {

        final ConsignmentModel clonedConsignment = getModelService().clone(consignmentModel);

        final Set<ConsignmentEntryModel> consignmentEntryModellist = new HashSet<>();

        for(ConsignmentEntryModel consignmentEntryModel : consignmentModel.getConsignmentEntries()) {
         final ConsignmentEntryModel consignmentEntryModel1 = getModelService().clone(consignmentEntryModel);
          consignmentEntryModel1.setConsignment(clonedConsignment);
          getModelService().save(consignmentEntryModel1);
          getModelService().refresh(consignmentEntryModel1);
          consignmentEntryModellist.add(consignmentEntryModel1);
        }
        clonedConsignment.setConsignmentEntries(consignmentEntryModellist);
          clonedList.add(clonedConsignment);
      }
      extendOrderModel.setConsignments(clonedList);
      getModelService().save(extendOrderModel);
      getModelService().refresh(extendOrderModel);
      getModelService().saveAll(clonedList);


      if(CollectionUtils.isNotEmpty(originalOrder.getOrderNotes())) {
        extendOrderModel.setOrderNotes(originalOrder.getOrderNotes());
      }
      getModelService().save(extendOrderModel);
      getModelService().refresh(extendOrderModel);

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
