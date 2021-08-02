package com.bl.core.services.extendorder.impl;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.extendorder.BlExtendOrderService;
import com.bl.core.stock.BlStockLevelDao;
import de.hybris.platform.commerceservices.constants.GeneratedCommerceServicesConstants.Enumerations.OrderStatus;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;

/**
 * This method created for cloning and updating extend order
 * @author Manikandan
 */
public class DefaultBlExtendOrderService implements BlExtendOrderService {

  private ModelService modelService;
  private KeyGenerator orderIDGenerator;
  private UserService userService;
  private CustomerAccountService customerAccountService;
  private BaseStoreService baseStoreService;
  private BlStockLevelDao blStockLevelDao;

  /**
   * This method created to clone the extend order from order model
   */
  @Override
  public OrderModel cloneOrderModelForExtendRental(final OrderModel originalOrder , long defaultAddedTimeForExtendRental) {
    return null == originalOrder.getExtendedOrderCopy() ?
        getExtendOrder(originalOrder , defaultAddedTimeForExtendRental) : originalOrder.getExtendedOrderCopy();
  }

  /**
   * This method is created to get extend order once order is cloned
   */
  private OrderModel getExtendOrder(final OrderModel originalOrder ,final long defaultAddedTimeForExtendRental)
  {
      final OrderModel extendOrderModel = getModelService().clone(originalOrder);

      // Set default values once order is cloned
      setDefaultValuesForExtendOrder(extendOrderModel ,defaultAddedTimeForExtendRental);
      saveAndRefreshModel(extendOrderModel);

      final Set<ConsignmentModel> clonedList = new HashSet<>();
      // clone consignment and consignment entries for extend order
      cloneConsignmentForExtendOrder(originalOrder , clonedList);
      extendOrderModel.setConsignments(clonedList);
      saveAndRefreshModel(extendOrderModel);
      getModelService().saveAll(clonedList);
      if(CollectionUtils.isNotEmpty(originalOrder.getOrderNotes())) {
        extendOrderModel.setOrderNotes(originalOrder.getOrderNotes());
      }
      saveAndRefreshModel(extendOrderModel);
      originalOrder.setExtendedOrderCopy(extendOrderModel);
      saveAndRefreshModel(originalOrder);
      return extendOrderModel;
  }

  /**
   * This method created to set the default values , once order is cloned
   */
  private void setDefaultValuesForExtendOrder(final OrderModel extendOrderModel , final long defaultAddedTimeForExtendRental){
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
    extendOrderModel.setTotalExtendDays((int) defaultAddedTimeForExtendRental);
    extendOrderModel.setExtendedOrderCopyList(Collections.emptyList());
  }

  /**
   * This method is created to  clone ths consignment and consignment entries
   */
  private void cloneConsignmentForExtendOrder(final OrderModel originalOrder ,final Set<ConsignmentModel> clonedList)
  {
    for(final ConsignmentModel consignmentModel : originalOrder.getConsignments()) {
      final ConsignmentModel clonedConsignment = getModelService().clone(consignmentModel);
      final Set<ConsignmentEntryModel> consignmentEntryModellist = new HashSet<>();
      for(final ConsignmentEntryModel consignmentEntryModel : consignmentModel.getConsignmentEntries()) {
        final ConsignmentEntryModel consignmentEntryModel1 = getModelService().clone(consignmentEntryModel);
        consignmentEntryModel1.setConsignment(clonedConsignment);
        getModelService().save(consignmentEntryModel1);
        getModelService().refresh(consignmentEntryModel1);
        consignmentEntryModellist.add(consignmentEntryModel1);
      }
      clonedConsignment.setConsignmentEntries(consignmentEntryModellist);
      clonedList.add(clonedConsignment);
    }
  }
  /**
   * This method created to update the extend order details once the extend is successfully
   */
  public void updateExtendOrder(final AbstractOrderModel extendOrderModel) {

    if(BooleanUtils.isTrue(extendOrderModel.getIsExtendedOrder()) &&
        extendOrderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.PAYMENT_CAPTURED) || StringUtils.isNotBlank(extendOrderModel.getPoNumber())) {

      final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
      final OrderModel originalOrder = getCustomerAccountService()
          .getOrderForCode((CustomerModel) getUserService().getCurrentUser(),
              extendOrderModel.getCode(),
              baseStoreModel);
      if (extendOrderModel.getExtendOrderStatus().getCode()
          .equalsIgnoreCase(ExtendOrderStatusEnum.PROCESSING.getCode())) {
        extendOrderModel.setExtendOrderStatus(ExtendOrderStatusEnum.COMPLETED);
      }
      saveAndRefreshModel(extendOrderModel);
      setExtendedOrderCopyListToOrder(originalOrder , extendOrderModel);
      originalOrder.setExtendedOrderCopy(null);

      saveAndRefreshModel(originalOrder);
      updateStockForExtendedOrder(extendOrderModel);
    }
    }

  /**
   * This method created to copy the extendOrder to extendOrder list of original order
   */
    private void setExtendedOrderCopyListToOrder(final AbstractOrderModel originalOrder , final AbstractOrderModel extendOrderModel) {
      if (CollectionUtils.isNotEmpty(originalOrder.getExtendedOrderCopyList())) {
        final List<AbstractOrderModel> extendOrderModelList = new ArrayList<>(
            originalOrder.getExtendedOrderCopyList());
        extendOrderModelList.add(extendOrderModel);
        originalOrder.setExtendedOrderCopyList(extendOrderModelList);
      } else {
        final List<AbstractOrderModel> orderModelList = new ArrayList<>();
        orderModelList.add(extendOrderModel);
        originalOrder.setExtendedOrderCopyList(orderModelList);
      }
    }

  /**
   * This method created to update the stock for extend order
   */
  private void updateStockForExtendedOrder(final AbstractOrderModel extendOrderModel) {
      final List<String> allocatedProductCodes = new ArrayList<>();
      if(CollectionUtils.isNotEmpty(extendOrderModel.getConsignments())) {
        for (final ConsignmentModel consignmentModel : extendOrderModel.getConsignments()) {
          for (final ConsignmentEntryModel consignmentEntryModel : consignmentModel.getConsignmentEntries()) {
            for (final BlProductModel blProductModel : consignmentEntryModel.getSerialProducts()) {
              getAllocatedProductCode(blProductModel , allocatedProductCodes);
            }
          }
        }
      }
      updateSerialStocks(allocatedProductCodes , extendOrderModel);
  }

  /**
   * This method created to get the serial product codes
   */
  private void getAllocatedProductCode(final BlProductModel blProductModel , final List<String> allocatedProductCodes) {
    if (blProductModel instanceof BlSerialProductModel && !blProductModel.getProductType()
        .equals(ProductTypeEnum.SUBPARTS)) {
      final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) blProductModel;
      allocatedProductCodes.add(blSerialProductModel.getCode());
    }
  }

  /**
   * This method create to allocate the stocks for respective serials which are extended
   */
  private void updateSerialStocks(final List<String> allocatedProductCodes , final AbstractOrderModel extendOrderModel) {
    final Collection<StockLevelModel> serialStocks = getSerialsForDateAndCodes(extendOrderModel,
        new HashSet<>(allocatedProductCodes));

    if (CollectionUtils.isNotEmpty(allocatedProductCodes) && serialStocks.stream()
        .allMatch(stock -> allocatedProductCodes.contains(stock.getSerialProductCode()))) {
      serialStocks.forEach(stock -> stock.setReservedStatus(true));
      this.getModelService().saveAll(serialStocks);
    }
  }

  /**
   * This method created for getting stock for serial product
   */
  private Collection<StockLevelModel> getSerialsForDateAndCodes(final AbstractOrderModel order,
      final Set<String> serialProductCodes) {

    return getBlStockLevelDao().findSerialStockLevelsForDateAndCodes(serialProductCodes, order.getActualRentalStartDate(),
            order.getActualRentalEndDate());
  }


  /**
   * This method created as common for saving and refreshing model
   */
  private void saveAndRefreshModel(final AbstractOrderModel orderModel) {
    getModelService().save(orderModel);
    getModelService().refresh(orderModel);
  }


  /**
   * This method created to store the PO number to order
   */
  @Override
  public boolean savePoPayment(final String poNumber , final String poNotes , final OrderModel orderModel){
    if(null != orderModel){
      orderModel.setPoNumber(poNumber.trim());
      orderModel.setPoNotes(poNotes);
      if(orderModel.getPaymentInfo() != null){
        orderModel.setPaymentInfo(null);
      }
      saveAndRefreshModel(orderModel);
      return true;
    }
    return false;
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


  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }




}
