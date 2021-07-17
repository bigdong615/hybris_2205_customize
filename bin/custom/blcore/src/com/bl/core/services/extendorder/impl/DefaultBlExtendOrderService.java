package com.bl.core.services.extendorder.impl;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.services.extendorder.BlExtendOrderService;
import de.hybris.platform.commerceservices.constants.GeneratedCommerceServicesConstants.Enumerations.OrderStatus;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.ArrayList;
import java.util.Collections;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;

public class DefaultBlExtendOrderService implements BlExtendOrderService {

  private ModelService modelService;
  private KeyGenerator orderIDGenerator;
  private UserService userService;
  private CustomerAccountService customerAccountService;
  private BaseStoreService baseStoreService;

  @Override
  public OrderModel cloneOrderModelForExtendRental(final OrderModel originalOrder , long defaultAddedTimeForExtendRental) {

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
      extendOrderModel.setTotaExtendDays((int) defaultAddedTimeForExtendRental);

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

  public void updateExtendOrder(final AbstractOrderModel extendOrderModel) {

    if(BooleanUtils.isTrue(extendOrderModel.getIsExtendedOrder()) && extendOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.PAYMENT_CAPTURED)) {

      final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
      final OrderModel originalOrder = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), extendOrderModel.getCode(),
          baseStoreModel);
      if(extendOrderModel.getExtendOrderStatus().getCode().equalsIgnoreCase(ExtendOrderStatusEnum.PROCESSING.getCode())) {
        extendOrderModel.setExtendOrderStatus(ExtendOrderStatusEnum.COMPLETED);
      }
      getModelService().save(extendOrderModel);
      getModelService().refresh(extendOrderModel);
      if(CollectionUtils.isNotEmpty(originalOrder.getExtendedOrderCopyList())) {
        originalOrder.getExtendedOrderCopyList().add(extendOrderModel);
      }
      else {
        final List<AbstractOrderModel> orderModelList = new ArrayList<>();
        orderModelList.add(extendOrderModel);
        originalOrder.setExtendedOrderCopyList(orderModelList);
      }
        originalOrder.setExtendedOrderCopy(null);

      getModelService().save(originalOrder);
      getModelService().refresh(originalOrder);
    }

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

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public CustomerAccountService getCustomerAccountService() {
    return customerAccountService;
  }

  public void setCustomerAccountService(
      CustomerAccountService customerAccountService) {
    this.customerAccountService = customerAccountService;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }



}
