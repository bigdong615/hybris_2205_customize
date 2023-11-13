package com.bl.core.services.extendorder.impl;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.enums.PaymentTransactionTypeEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.services.extendorder.BlExtendOrderService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;

import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;

/**
 * This method created for cloning and updating extend order
 * @author Manikandan
 */
public class DefaultBlExtendOrderService implements BlExtendOrderService {

  private static final Logger LOG = Logger.getLogger(DefaultBlExtendOrderService.class);
  private ModelService modelService;
  private KeyGenerator orderIDGenerator;
  private UserService userService;
  private CustomerAccountService customerAccountService;
  private BaseStoreService baseStoreService;
  private BlStockLevelDao blStockLevelDao;
  private DefaultBlESPEventService defaultBlESPEventService;
  @Resource(name = "defaultBlUserService")
  private DefaultBlUserService defaultBlUserService;

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
      extendOrderModel.setOrderNotes(Collections.EMPTY_LIST);
      // Set default values once order is cloned
      setDefaultValuesForExtendOrder(extendOrderModel ,defaultAddedTimeForExtendRental);
      saveAndRefreshModel(extendOrderModel);

      final Set<ConsignmentModel> clonedList = new HashSet<>();
      // clone consignment and consignment entries for extend order
      cloneConsignmentForExtendOrder(originalOrder , clonedList);
      extendOrderModel.setConsignments(clonedList);
      extendOrderModel.setOrderModifiedDate(null);
      saveAndRefreshModel(extendOrderModel);
      getModelService().saveAll(clonedList);
      clonedOrderNotesForExtendOrder(originalOrder , extendOrderModel);
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
    extendOrderModel.setIsLatestOrder(true);
    extendOrderModel.setIsSAPOrder(Boolean.TRUE);
    extendOrderModel.setGiftCardAmount(0.0);
    extendOrderModel.setGiftCard(Collections.emptyList());
  }

  /**
   * This method is created to  clone ths consignment and consignment entries
   */
  private void cloneConsignmentForExtendOrder(final OrderModel originalOrder ,final Set<ConsignmentModel> clonedList)
  {
    for(final ConsignmentModel consignmentModel : originalOrder.getConsignments()) {
      final ConsignmentModel clonedConsignment = getModelService().clone(consignmentModel);
      clonedConsignment.setOrderNotes(Collections.emptyList());
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
   * @param extendOrderModel the extended order model
   */
  public void updateExtendOrder(final AbstractOrderModel extendOrderModel) {

    if(BooleanUtils.isTrue(extendOrderModel.getIsExtendedOrder()) &&
        isExtendOrderPaymentCaptured(extendOrderModel) || StringUtils.isNotBlank(extendOrderModel.getPoNumber())) {

      final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
      final OrderModel originalOrder = getCustomerAccountService()
          .getOrderForCode((CustomerModel) getUserService().getCurrentUser(),
              extendOrderModel.getCode(),
              baseStoreModel);
      if (extendOrderModel.getExtendOrderStatus().getCode()
          .equalsIgnoreCase(ExtendOrderStatusEnum.PROCESSING.getCode())) {
        extendOrderModel.setExtendOrderStatus(ExtendOrderStatusEnum.COMPLETED);
      }
      originalOrder.setOrderModifiedDate(new Date());
      final long daysBetweenDates = BlDateTimeUtils.getDaysBetweenDates(extendOrderModel.getRentalStartDate(),
      		extendOrderModel.getRentalEndDate());
      extendOrderModel.setRunTot_daysRented((int)daysBetweenDates);
      saveAndRefreshModel(extendOrderModel);
      setExtendedOrderCopyListToOrder(originalOrder , extendOrderModel);
      originalOrder.setExtendedOrderCopy(null);

      saveAndRefreshModel(originalOrder);
      updateStockForExtendedOrder(extendOrderModel);

      // To call Extend order ESP Event
      getDefaultBlESPEventService().sendExtendOrderEvent((OrderModel) extendOrderModel);

    }
    }

  /**
   * This method created to copy the extendOrder to extendOrder list of original order
   */
    private void setExtendedOrderCopyListToOrder(final AbstractOrderModel originalOrder , final AbstractOrderModel extendOrderModel) {
      if (CollectionUtils.isNotEmpty(originalOrder.getExtendedOrderCopyList())) {
        final List<AbstractOrderModel> extendOrderModelList = new ArrayList<>(
            originalOrder.getExtendedOrderCopyList());
        final AbstractOrderModel lastExtendedOrderModel = extendOrderModelList.get(extendOrderModelList.size() - 1);
        lastExtendedOrderModel.setIsLatestOrder(false);
        saveAndRefreshModel(lastExtendedOrderModel);
        extendOrderModelList.add(extendOrderModel);
        originalOrder.setExtendedOrderCopyList(extendOrderModelList);
        setRunningTotalsOrderExtensionAttributes(extendOrderModel, lastExtendedOrderModel);
      } else {
        final List<AbstractOrderModel> orderModelList = new ArrayList<>();
        orderModelList.add(extendOrderModel);
        originalOrder.setExtendedOrderCopyList(orderModelList);
        setRunningTotalsOrderExtensionAttributes(extendOrderModel, originalOrder);
      }
      saveAndRefreshModel(extendOrderModel);
      originalOrder.setIsLatestOrder(false);
      saveAndRefreshModel(originalOrder);
    }
    
    /**
 	 * Sets the values for Running Totals Order Extension Attributes.
 	 *
 	 * @param extendedOrder
 	 *           the extended order
 	 * @param order
 	 *           the order
 	 */
 	private void setRunningTotalsOrderExtensionAttributes(final AbstractOrderModel extendedOrder, final AbstractOrderModel order)
 	{
 		extendedOrder.setRunTot_grandTotal(getDefaultValueIfNull(order.getRunTot_grandTotal()) + getGrandTotalFromOrder(extendedOrder));
 		extendedOrder.setRunTot_subtotal(getDefaultValueIfNull(order.getRunTot_subtotal()) + getDefaultValueIfNull(extendedOrder.getSubtotal()));
 		extendedOrder.setRunTot_totalOptionsCost(getDefaultValueIfNull(order.getRunTot_totalOptionsCost()) + getDefaultValueIfNull(extendedOrder.getTotalOptionsCost()));
 		extendedOrder.setRunTot_totalPrice(getDefaultValueIfNull(order.getRunTot_totalPrice()) + getDefaultValueIfNull(extendedOrder.getTotalPrice()));
 		extendedOrder.setRunTot_totalTax(getDefaultValueIfNull(order.getRunTot_totalTax()) + getDefaultValueIfNull(extendedOrder.getTotalTax()));
 		if(ObjectUtils.allNotNull(extendedOrder.getRentalStartDate(),extendedOrder.getRentalEndDate()))
		{
 			extendedOrder.setRunTot_daysRented(
					Long.valueOf(BlDateTimeUtils.getDaysBetweenDates(extendedOrder.getRentalStartDate(), extendedOrder.getRentalEndDate())).intValue());
		}
		else
		{
			extendedOrder.setRunTot_daysRented(Integer.valueOf(0));
		}
 	}
 	
 	/**
	 * Gets the default value if null.
	 *
	 * @param value the value
	 * @return the default value if null
	 */
	private Double getDefaultValueIfNull(final Double value)
	{
		return ObjectUtils.defaultIfNull(value, Double.valueOf(0.0d));
	}
 	
 	/**
	 * Gets the grand total from order.
	 *
	 * @param order
	 *           the order
	 * @return the grand total from order
	 */
	private Double getGrandTotalFromOrder(final AbstractOrderModel order)
	{
		if (Objects.isNull(order.getGrandTotal()) || order.getGrandTotal().compareTo(Double.valueOf(0.0d)) <= 0)
		{
			return order.getTotalPrice();
		}
		return order.getGrandTotal();
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
    if(CollectionUtils.isNotEmpty(extendOrderModel.getConsignments())) {
      final String orderCode = extendOrderModel.getCode();
      extendOrderModel.getConsignments().forEach(consignmentModel -> {
        final Collection<StockLevelModel> serialStocks = getSerialsForDateAndCodes(consignmentModel,
            new HashSet<>(allocatedProductCodes));
        if (CollectionUtils.isNotEmpty(allocatedProductCodes) && serialStocks.stream()
            .allMatch(stock -> allocatedProductCodes.contains(stock.getSerialProductCode()))) {
          serialStocks.forEach(stock -> {
            try {
              BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                      "Reserve stock for serial product {}, for stock date {} while extends rental date before change Hard Assign {}, reserve status {}, associated order {}"
                              + ",current date {} current user {}",stock.getSerialProductCode(), stock.getDate(), stock.getHardAssigned(), stock.getReservedStatus(),
                      stock.getOrder(), new Date(), (defaultBlUserService.getCurrentUser()!=null? defaultBlUserService.getCurrentUser().getUid():"In Automation"));
            }catch (Exception e){
              BlLogger.logMessage(LOG,Level.ERROR,"Some error occur while reserve stock in replace serial flow",e);
            }
            stock.setOrder(orderCode);
            stock.setReservedStatus(true);


          });
          this.getModelService().saveAll(serialStocks);
        }
      });

    }
  }

  /**
   * This method created for getting stock for serial product
   */
  private Collection<StockLevelModel> getSerialsForDateAndCodes(final ConsignmentModel consignmentModel,
      final Set<String> serialProductCodes) {

    return getBlStockLevelDao().findSerialStockLevelsForDateAndCodes(serialProductCodes, consignmentModel.getOptimizedShippingStartDate(),
        consignmentModel.getOptimizedShippingEndDate(), Boolean.FALSE);
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

  /**
   * This method created to check whether the payment captured for extend order
   * @param extendOrderModel the extended order model
   * @return boolean based on results
   */
  private boolean isExtendOrderPaymentCaptured(final AbstractOrderModel extendOrderModel) {
    final AtomicBoolean isPaymentCaptured = new AtomicBoolean(false);
    if(CollectionUtils.isNotEmpty(extendOrderModel.getPaymentTransactions())) {
      final PaymentTransactionModel transaction= extendOrderModel.getPaymentTransactions().stream().filter(paymentTransactionModel ->
          PaymentTransactionTypeEnum.EXTEND_ORDER.equals(paymentTransactionModel.getTransactionType())).findAny().orElse(null);
      if(Objects.nonNull(transaction)) {
        transaction.getEntries().stream().forEach(transactionEntry -> {
            if(TransactionStatus.ACCEPTED.name().equals(transactionEntry.getTransactionStatus())
            && transactionEntry.getTransactionStatusDetails().contains(TransactionStatusDetails.SUCCESFULL.name()) && PaymentTransactionType
            .CAPTURE.equals(transactionEntry.getType())) {
              isPaymentCaptured.set(Boolean.TRUE);
          }
        });
      }
    }
    return isPaymentCaptured.get();
  }

  /**
   * Cloning the order Notes For Extend Order
   *
   * @param originalOrder
   * @param extendOrderModel
   */
  private void clonedOrderNotesForExtendOrder(final OrderModel originalOrder, final OrderModel extendOrderModel) {
    if(CollectionUtils.isNotEmpty(originalOrder.getOrderNotes())) {
      List<NotesModel> notesModels = new ArrayList<>();
      originalOrder.getOrderNotes().forEach(notesModel -> {
        final NotesModel clonedNotes  = getModelService().clone(notesModel);
        clonedNotes.setConsignment(Collections.EMPTY_SET);
        clonedNotes.setConsignment(extendOrderModel.getConsignments());
        clonedNotes.setOrder(extendOrderModel);
        notesModels.add(clonedNotes);
        getModelService().save(clonedNotes);
        getModelService().refresh(clonedNotes);
      });
      extendOrderModel.setOrderNotes(notesModels);
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


  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }


  public DefaultBlESPEventService getDefaultBlESPEventService() {
    return defaultBlESPEventService;
  }

  public void setDefaultBlESPEventService(final DefaultBlESPEventService defaultBlESPEventService) {
    this.defaultBlESPEventService = defaultBlESPEventService;
  }



}
