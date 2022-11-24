package com.bl.facades.order;

import com.bl.core.data.StockResult;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.impl.DefaultBlCalculationService;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.services.extendorder.impl.DefaultBlExtendOrderService;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.services.tax.DefaultBlExternalTaxesService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlExtendOrderUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.populators.BlExtendRentalOrderDetailsPopulator;
import com.bl.facades.populators.BlOrderAppliedVouchersPopulator;
import com.bl.facades.product.data.AvailabilityMessage;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.impl.DefaultOrderFacade;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.promotions.PromotionsService;
import de.hybris.platform.promotions.jalo.PromotionsManager.AutoApplyMode;
import de.hybris.platform.promotions.model.PromotionGroupModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.time.TimeService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.BaseStoreModel;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.ui.Model;

/**
 * This class is created for My account rent again and extend order functionality
 * @author Manikandan
 */
public class DefaultBlOrderFacade extends DefaultOrderFacade implements BlOrderFacade {

  private static final Logger LOG = Logger.getLogger(DefaultBlOrderFacade.class);

  private BlCartFacade blCartFacade;
  private BlCartService blCartService;
  private CommerceCartService commerceCartService;
  private ModelService modelService;
  private Converter<CommerceCartModification, CartModificationData> cartModificationConverter;
  private PriceDataFactory priceDataFactory;
  private BlCommercePriceService commercePriceService;
  private ProductService productService;
  private SessionService sessionService;
  private DefaultBlExtendOrderService defaultBlExtendOrderService;
  private DefaultBlCalculationService defaultBlCalculationService;
  private PromotionsService promotionsService;
  private BaseSiteService baseSiteService;
  private TimeService timeService;
  private BlCommerceStockService blCommerceStockService;
  private BlExtendRentalOrderDetailsPopulator blExtendRentalOrderDetailsPopulator;
  private BlDatePickerService blDatePickerService;
  private BlOrderAppliedVouchersPopulator blOrderAppliedVouchersPopulator;
  private DefaultBlExternalTaxesService defaultBlExternalTaxesService;
  private BlOrderService blOrderService;

  /**
   * This method created to add all the products from existing order
   */
  @Override
  public boolean addToCartAllOrderEnrties(final String orderCode , final Model model) throws CommerceCartModificationException
  {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode, baseStoreModel);

    final CartModel cartModel = getBlCartService().getSessionCart();

    if(null != cartModel && CollectionUtils.isNotEmpty(cartModel.getEntries()) && BooleanUtils.isFalse(cartModel.getIsRentalOrder())) {
      return false;
    }
    final List<AbstractOrderEntryModel> entries = orderModel.getEntries().stream().filter(entry -> !entry.isBundleEntry()).collect(
        Collectors.toList());
    for (final AbstractOrderEntryModel lEntryModel : entries)
    {
        final ProductModel lProductModel = lEntryModel.getProduct();
        addToCart(lProductModel, lEntryModel.getQuantity().intValue(), lEntryModel);
    }

    return true;
  }


  /**
   * This method created for adding product to cart when user renting again from previous order
   * @param blProductModel the SKU product
   * @param quantity quantity
   * @param abstractOrderEntryModel the order model
   * @return CartModificationData
   * @throws CommerceCartModificationException CommerceCartModificationException
   */
  public CartModificationData addToCart(final ProductModel blProductModel, final long quantity ,
      final AbstractOrderEntryModel abstractOrderEntryModel) throws CommerceCartModificationException {

    CartModel cartModel = blCartService.getSessionCart();
    final CommerceCartParameter parameter = new CommerceCartParameter();

    try {
        //For rental product rental products
        parameter.setProduct(blProductModel);
        parameter.setUnit(blProductModel.getUnit());
        parameter.setCreateNewEntry(false);
    } catch (final Exception exception) {
      BlLogger.logMessage(LOG, Level.ERROR , "Error while adding products from rent again" + blProductModel.getCode() , exception);
    }

    parameter.setEnableHooks(true);
    parameter.setCart(cartModel);
    parameter.setQuantity(quantity);

    // To update damage waiver price same as previous order
    updateDamegeWaiverSelectedFromOrder(abstractOrderEntryModel , parameter);
    final CommerceCartModification commerceCartModification = getCommerceCartService()
        .addToCart(parameter);
    setCartType(null, cartModel, commerceCartModification);

    return getCartModificationConverter().convert(commerceCartModification);
  }

  /**
   * To set the cart type while adding produdts to cart
   */

  private void setCartType(final BlSerialProductModel blSerialProductModel,
      final CartModel cartModel, final CommerceCartModification commerceCartModification) {
    if (commerceCartModification != null && commerceCartModification.getStatusCode()
        .equals(BlFacadesConstants.SUCCESS)) {
      if (null == blSerialProductModel) {
        cartModel.setIsRentalOrder(Boolean.TRUE);
      } else {
        cartModel.setIsRentalOrder(Boolean.FALSE);
        //Added code for serial status changes
        blSerialProductModel.setSerialStatus(SerialStatusEnum.ADDED_TO_CART);
        getModelService().save(blSerialProductModel);
      }
    }
    getModelService().save(cartModel);
  }


  /*
   * To set damage waiver in parameter from order
   */
  private void updateDamegeWaiverSelectedFromOrder(final AbstractOrderEntryModel abstractOrderEntryModel , final CommerceCartParameter parameter) {
    parameter.setIsFromRentAgainPage(true);
    parameter.setIsDamageWaiverProSelected(abstractOrderEntryModel.getGearGuardProFullWaiverSelected());
    parameter.setIsDamageWaiverSelected(abstractOrderEntryModel.getGearGuardWaiverSelected());
    parameter.setIsNoDamageWaiverSelected(abstractOrderEntryModel.getNoDamageWaiverSelected());
  }

  /**
   * This method created to calculate extend order price based on extend rental date
   */

  @Override
  public OrderData calculatePriceForExtendOrders(final OrderModel orderModel , final OrderData orderData, final String orderEndDate,
      final String selectedDate) {

    Date startDate = BlDateTimeUtils.convertStringDateToDate(orderEndDate, BlFacadesConstants.START_DATE_PATTERN);
    final Date endDate = BlDateTimeUtils.convertStringDateToDate(selectedDate, BlFacadesConstants.END_DATE_PATTERN);
    startDate = setAdditionalDaysForStartDate(startDate);
    final Date stockEndDate = setAdditionalDaysForStock(endDate);
    final List<StockResult> stockResults = new ArrayList<>();
    final AtomicReference<Date> optimizedRentalEndDateForExtendOrder = new AtomicReference<>();
    final Map<String, Date> stringStringMap = new HashMap<>();
    checkStockAvailablity(orderModel , orderData ,  stockResults , endDate ,  optimizedRentalEndDateForExtendOrder , stringStringMap );
    if (CollectionUtils.isEmpty(stockResults)) {
      BlLogger.logFormattedMessage(LOG , Level.DEBUG , "optimizedRentalEndDateForExtendOrder : New optimized shipping end date as {} for extend order {} " ,
          String.valueOf(optimizedRentalEndDateForExtendOrder.get()) , orderModel.getCode());
      populateExtendOrderDetails(startDate , endDate , selectedDate , orderModel , orderData , stockEndDate , stringStringMap);
    }
    return orderData;
  }

  /**
   * This method created to check the stock availability for extend order
   */
  private void checkStockAvailablity(final OrderModel orderModel, final OrderData orderData, final List<StockResult> stockResults,
      final Date extendRentalEndDate,
      final AtomicReference<Date> optimizedRentalEndDateForExtendOrder,
      final Map<String, Date> stringStringMap) {
      if(CollectionUtils.isNotEmpty(orderModel.getConsignments())) {
          for (final ConsignmentModel consignmentModel : orderModel.getConsignments()) {
              for (final ConsignmentEntryModel consignmentEntryModel : consignmentModel.getConsignmentEntries()) {
                  for (final BlProductModel blProductModel : consignmentEntryModel
                          .getSerialProducts()) {
                      final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(
                              BlackoutDateTypeEnum.HOLIDAY);
                      final int numberOfDaysToAdd = getNumberOfDaysToAdd(orderModel, consignmentModel);
                      final Calendar cal = Calendar.getInstance();
                      cal.setTime(extendRentalEndDate);
                      final LocalDate localDate = LocalDate.of(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH) + 1, cal.get(Calendar.DAY_OF_MONTH));
                      optimizedRentalEndDateForExtendOrder.set(BlDateTimeUtils.addDaysInRentalDates(numberOfDaysToAdd, localDate, blackOutDates));
                      checkProductForAvailablity(blProductModel, getNewStockStartDate(orderModel, consignmentModel), optimizedRentalEndDateForExtendOrder.get(), stockResults, orderData);
                      if (CollectionUtils.isEmpty(stockResults)) {
                          stringStringMap.put(consignmentModel.getCode(), optimizedRentalEndDateForExtendOrder.get());
                      }
                  }

              }
          }
      }
    if(CollectionUtils.isEmpty(orderModel.getConsignments())){
      orderData.setIsAllProductExtendabe(false);
      final StockResult stockResult = new StockResult();
      stockResult.setStockLevelStatus(StockLevelStatus.OUTOFSTOCK);
      stockResult.setAvailableCount(0);
      stockResult.setTotalCount(0);
      stockResults.add(stockResult);
      orderData.setExtendErrorMessage("One or more of your items is unavailable to be extended. Please contact us"
          + "if you are unable to return your order by its scheduled return date.");

    }
  }



  /**
   * This method created to check for product availability bases on serial product of existing order
   */
  private void checkProductForAvailablity(final BlProductModel blProductModel , final Date stockStartDate ,
      final Date stockEndDate , final List<StockResult> stockResults , final OrderData orderData)
    {
      if (blProductModel instanceof BlSerialProductModel && !blProductModel.getProductType().equals(
          ProductTypeEnum.SUBPARTS)) {
        final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) blProductModel;
        final StockResult stockResult = getBlCommerceStockService()
            .getStockForEntireExtendDuration(blSerialProductModel.getCode(),
                Collections.singleton(blSerialProductModel.getWarehouseLocation())
                , stockStartDate, stockEndDate);
        if (stockResult.getStockLevelStatus().getCode()
            .equalsIgnoreCase(StockLevelStatus.OUTOFSTOCK.getCode())) {
          stockResults.add(stockResult);
          orderData.setIsAllProductExtendabe(false);
          orderData
              .setExtendErrorMessage("One or more of your items is unavailable to be extended. Please contact us if you are unable to return your order by its scheduled return date.");
          BlLogger.logMessage(LOG, Level.INFO, "product cannot be extend",
              blSerialProductModel.getCode());
        }
      }
    }


  /**
   * This method created to populate the extend order details
   */
  private void populateExtendOrderDetails(final Date startDate, final Date endDate,
      final String selectedDate,
      final OrderModel orderModel, final OrderData orderData, final Date stockEndDate,
      final Map<String, Date> stringStringMap) {
    long defaultAddedTimeForExtendRental = BlDateTimeUtils
        .getDaysBetweenDates(startDate, endDate) + 1;
    if (StringUtils.isEmpty(selectedDate)) {
      defaultAddedTimeForExtendRental = 1;
    }
    orderData.setAddedTimeForExtendRental(
        (int) defaultAddedTimeForExtendRental); // Default value which added for extend order
    final PriceDataType priceType = PriceDataType.BUY;
    final OrderModel extendOrderModel = getDefaultBlExtendOrderService()
        .cloneOrderModelForExtendRental(orderModel , defaultAddedTimeForExtendRental);

    // Start Date will same as existing rental startDate
    extendOrderModel.setTotalExtendDays((int) defaultAddedTimeForExtendRental); // To set total number of days extended
    Calendar extendStartDate = Calendar.getInstance();
    extendStartDate.setTime(startDate);
    extendStartDate.add(Calendar.DAY_OF_MONTH ,1);
    extendOrderModel.setRentalEndDate(endDate);    // End Date will be stored based on customer selection
    extendOrderModel.setActualRentalEndDate(stockEndDate);


    // To set extend startDate and Extend end date on order model .
    extendOrderModel.setExtendRentalStartDate(startDate);
    extendOrderModel.setExtendRentalEndDate(endDate);
    setOptimizedShippingEndDateForConsignment(stringStringMap , extendOrderModel);

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Order with code {} extended from extended rental start date {} to extended rental end date {}.", extendOrderModel.getCode() ,
        extendOrderModel.getExtendRentalStartDate() , extendOrderModel.getExtendRentalEndDate());
    try {
      getDefaultBlCalculationService()
          .recalculateForExtendOrder(extendOrderModel, (int) defaultAddedTimeForExtendRental);
      getPromotionsService()
          .updatePromotions(getPromotionGroups(), extendOrderModel, true, AutoApplyMode.APPLY_ALL,
              AutoApplyMode.APPLY_ALL, getTimeService().getCurrentTime());
    } catch (final CalculationException e) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR ,
          "Error while Calculating promotion for Order with code {} extended from extended rental start date {} to extended rental end date {}."
          , extendOrderModel.getCode() ,  extendOrderModel.getExtendRentalStartDate() , extendOrderModel.getExtendRentalEndDate());
    }

    orderData.setSubTotalTaxForExtendRental(
        getPriceDataFactory()
            .create(priceType, BigDecimal.valueOf(extendOrderModel.getSubtotal()),
                extendOrderModel.getCurrency().getIsocode()));
    orderData.setTotalDamageWaiverCostForExtendRental(getPriceDataFactory()
        .create(priceType, BigDecimal.valueOf(extendOrderModel.getTotalDamageWaiverCost()),
            extendOrderModel.getCurrency().getIsocode()));
    orderData.setTotalTaxForExtendRental(
        getPriceDataFactory()
            .create(priceType, BigDecimal.valueOf(extendOrderModel.getTotalTax()),
                extendOrderModel.getCurrency().getIsocode()));

    orderData.setExtendOrderDiscount( getPriceDataFactory()
        .create(priceType, BigDecimal.valueOf(extendOrderModel.getTotalDiscounts()),
            extendOrderModel.getCurrency().getIsocode()));

    orderData.setOrderTotalWithTaxForExtendRental(getPriceDataFactory()
        .create(priceType, BigDecimal.valueOf(extendOrderModel.getTotalPrice()), extendOrderModel.getCurrency().getIsocode()));

    getBlOrderAppliedVouchersPopulator().populate(extendOrderModel , orderData);

    // To set current extendOrderModel to session
    BlExtendOrderUtils.setCurrentExtendOrderToSession(extendOrderModel);
  }



  /**
   * This method created to set the extend order details
   */
  @Override
  public OrderData setRentalExtendOrderDetails(final String orderCode , final String rentalEndDate ,final String selectedDate) {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
   final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode,
        baseStoreModel);
   // Calculate order details based on new return date for extend rental order
    return calculatePriceForExtendOrders(orderModel , getOrderConverter().convert(orderModel), rentalEndDate , selectedDate);
  }


  /**
   * This method created to get the promotion group
   */
  private Collection<PromotionGroupModel> getPromotionGroups()
  {
    final Collection<PromotionGroupModel> promotionGroupModels = new ArrayList<>();
    if (getBaseSiteService().getCurrentBaseSite() != null
        && getBaseSiteService().getCurrentBaseSite().getDefaultPromotionGroup() != null)
    {
      promotionGroupModels.add(getBaseSiteService().getCurrentBaseSite().getDefaultPromotionGroup());
    }
    return promotionGroupModels;
  }

  /**
   * To set additional dated to check status
   */
 private Date setAdditionalDaysForStock(final Date dateToAdd) {
   return BlDateTimeUtils.addingNoOfDaysInGivenDate(dateToAdd,2);
 }

  /**
   * To set additional date for startDate
   */
  private Date setAdditionalDaysForStartDate(final Date dateToAdd) {
    return BlDateTimeUtils.addingNoOfDaysInGivenDate(dateToAdd,1);
  }

  /**
   * This method created to get the extend order data
   */
  @Override
  public OrderData getExtendedOrderDetailsFromOrderCode(final String orderCode) {

    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode,
        baseStoreModel);
    final OrderData orderData = new OrderData();
    getBlExtendRentalOrderDetailsPopulator().populate(getExtendOrderAfterPlaceingOrder(orderModel), orderData);
    return orderData;
  }

  /**
   * This method created to get extend order once extend order is extended successfully
   */
  @Override
  public AbstractOrderModel getExtendOrderAfterPlaceingOrder(final OrderModel orderModel) {

    if(BooleanUtils.isFalse(orderModel.getIsExtendedOrder()) && null == orderModel.getExtendedOrderCopy() &&
        CollectionUtils.isNotEmpty(orderModel.getExtendedOrderCopyList())) {
     final  List<AbstractOrderModel> orderModelList = orderModel.getExtendedOrderCopyList();
      if (CollectionUtils.isNotEmpty(orderModelList)) {
        final int size = orderModelList.size();
        for (final AbstractOrderModel extendOrder :orderModelList) {
          if (BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) && extendOrder
              .getExtendOrderStatus().getCode()
              .equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())
              && orderModelList.get(size - 1).getPk()
              .equals(extendOrder.getPk())) {
            return extendOrder;
          }
        }
      }
    }

    return orderModel;
  }


  /**
   * This method created to get extend order from order
   */
  @Override
  public OrderModel getExtendOrderFromOrderModel(final OrderModel orderModel) {

    if(null != orderModel.getExtendedOrderCopy() && StringUtils.endsWithIgnoreCase(orderModel.getExtendedOrderCopy().getExtendOrderStatus().getCode() ,
        ExtendOrderStatusEnum.PROCESSING.getCode())) {
      return orderModel.getExtendedOrderCopy();
    }
    return orderModel;
  }

  /**
   * This method created to get extend order from order code
   */
  @Override
  public OrderModel getExtendedOrderModelFromCode(final String orderCode) {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode,
        baseStoreModel);
    return getExtendOrderFromOrderModel(orderModel);
  }

  /**
   * This method created to update the extend order details
   */
  @Override
  public void updateOrderExtendDetails(final OrderModel orderModel) {
    getDefaultBlExtendOrderService().updateExtendOrder(orderModel);
  }

  /**
   * This method created to store the po number to extend order
   */
  @Override
  public boolean savePoPaymentForExtendOrder(final String poNumber , final String poNotes, final String orderCode) {
    return getDefaultBlExtendOrderService().savePoPayment(poNumber , poNotes , getExtendedOrderModelFromCode(orderCode));
  }

  /**
   * This method created to store the po number to payBill order
   */
  @Override
  public boolean savePoPaymentForPayBillOrder(final String poNumber , final String poNotes, final String orderCode) {
	  final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
	    final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode,
	        baseStoreModel);
    return getBlCartService().savePoPaymentDetailsForPayBill(poNumber , poNotes , orderModel);
  }

  /**
   * This method created to get order model from order code
   */
  @Override
  public OrderModel getOrderModelFromOrderCode(final String orderCode){
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    return getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode, baseStoreModel);
  }

  /**
   * {@inheritDoc}
   * @param target order data
   */
  @Override
  public void setPayBillAttributes(final OrderData target) {
      final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
      OrderModel source = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService()
              .getCurrentUser(), target.getCode(), baseStoreModel);

      final AtomicDouble totalAmt = new AtomicDouble(0.0);
      applyTaxOnPayBillCharges(source);
      final AtomicDouble currentTax = new AtomicDouble(0.0);

      source.setUnPaidBillPresent(false);
      getModelService().save(source);
      source.getConsignments()
              .forEach(consignment -> consignment.getConsignmentEntries().forEach(consignmentEntry -> consignmentEntry
                      .getBillingCharges().forEach((serialCode, listOfCharges) -> {
                          final List<AvailabilityMessage> messages = Lists.newArrayList();
                          listOfCharges.forEach(billing -> {
                              if (BooleanUtils.isFalse(billing.isBillPaid()))
                              {
                                  final AvailabilityMessage messageForBillingType = getMessage(billing.getUnPaidBillNotes());
                                  messages.add(messageForBillingType);
                                  totalAmt.addAndGet(billing.getChargedAmount().doubleValue());
                                  currentTax.addAndGet(billing.getTaxAmount().doubleValue());
                              }
                          });
                          target.getEntries().forEach(entry -> {
                              if (entry.getProduct().getCode().equals(getSkuCode(consignmentEntry, serialCode)))
                              {
                                  final List<AvailabilityMessage> entryMessages = Lists
                                          .newArrayList(CollectionUtils.emptyIfNull(entry.getMessages()));
                                  entryMessages.addAll(messages);
                                  entry.setMessages(entryMessages);
                              }
                          });})));

      target.setExtensionBillingCost(convertDoubleToPriceData(totalAmt.get(), source));
      target.setTotalPayBillTax(convertDoubleToPriceData(currentTax.get(), source ));
      target.setOrderTotalWithTaxForPayBill(convertDoubleToPriceData(totalAmt.get() + currentTax.get() , source));
  }

  /**
   * This method created to get SKU product code.
   * @param consignmentEntry consignment entry
   * @param serialCode serial product code
   */
  private String getSkuCode(final ConsignmentEntryModel consignmentEntry, final String serialCode)
  {
    final StringBuilder sb = new StringBuilder();
    consignmentEntry.getSerialProducts().forEach(serial -> {
      if(serial instanceof BlSerialProductModel && ((BlSerialProductModel)serial).getCode().equals(serialCode))
      {
        final BlSerialProductModel serialProduct = ((BlSerialProductModel) serial);
        sb.append(serialProduct.getBlProduct().getCode());
      }
    });
    return sb.toString();
  }

  /**
   * It applies tax on billing charges
   * @param source
   */
  private void applyTaxOnPayBillCharges(OrderModel source) {
    final AtomicBoolean isTaxBeApplied = new AtomicBoolean(Boolean.FALSE);
    isTaxApplicableOnPayBillCharges(isTaxBeApplied, source);
    if(isTaxBeApplied.get()) {
      source.setUnPaidBillPresent(true);
      getDefaultBlExternalTaxesService().calculateExternalTaxes(source);
    }
  }

  /**
   * It checks whether tax should be applied on billing charges or not
   * @param isTaxBeApplied
   * @param source
   */
  private void isTaxApplicableOnPayBillCharges(AtomicBoolean isTaxBeApplied, OrderModel source) {
    source.getConsignments()
        .forEach(consignment ->
          consignment.getConsignmentEntries().forEach(consignmentEntry -> consignmentEntry
              .getBillingCharges().forEach((serialCode, listOfCharges) -> {
                final boolean result = listOfCharges.stream().anyMatch(billing ->
                    BooleanUtils.isFalse(billing.isBillPaid()));
                if(result) {
                  isTaxBeApplied.set(Boolean.TRUE);
                  return;
                }
              })));
  }

  /**
   * This method converts double to price data
   */
  private PriceData convertDoubleToPriceData(final Double price , OrderModel orderModel) {
    return getPriceDataFactory().create(PriceDataType.BUY ,BigDecimal.valueOf(price),orderModel.getCurrency());
  }

  /**
   * This method created pay bill messages separation.
   * @param billChargeType
   * @return AvailabilityMessage
   */
  private AvailabilityMessage getMessageForBillingType(final ItemBillingChargeTypeEnum billChargeType)
  {
    switch (billChargeType.getCode())
    {
      case "MISSING_CHARGE":
        return getMessage("pay.bill.missing.charge");

      case "LATE_CHARGE":
        return getMessage("pay.bill.late.charge");

      case "REPAIR_CHARGE":
        return getMessage("pay.bill.repair.charge");

      default:
        return null;
    }
  }

  /**
   * This method created pay bill messages.
   * @param messageCode
   * @return AvailabilityMessage
   */
  private AvailabilityMessage getMessage(final String messageCode)
  {
    final AvailabilityMessage am = new AvailabilityMessage();
    am.setMessageCode(messageCode);
    return am;
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void setResolvedStatusOnRepairLog(final String orderCode)
  {
	  getBlOrderService().setResolvedStatusOnRepairLog(orderCode);
  }


  /**
   * This method created to map the Optimized shipping date
   * @return Map<String, Integer>
   */
  private Map<String, Integer> getNumberOfDaysForDeliveryMethod(){
    final Map<String, Integer> numberOfPostDeliveryDays = new LinkedHashMap<>();
    numberOfPostDeliveryDays.put(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode() , 3);
    numberOfPostDeliveryDays.put(OptimizedShippingMethodEnum.TWO_DAY_AIR.getCode() , 2);
    numberOfPostDeliveryDays.put(OptimizedShippingMethodEnum.TWO_DAY_AIR_AM.getCode() , 2);
    numberOfPostDeliveryDays.put(OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode() , 2);
    numberOfPostDeliveryDays.put(OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode() , 1);
    numberOfPostDeliveryDays.put(OptimizedShippingMethodEnum.NEXT_DAY_AIR.getCode() , 1);
    numberOfPostDeliveryDays.put(OptimizedShippingMethodEnum.NEXT_DAY_AIR_AM.getCode() , 1);
    numberOfPostDeliveryDays.put(OptimizedShippingMethodEnum.NEXT_DAY_AIR_SAT.getCode() , 1);
    return numberOfPostDeliveryDays;
  }

  /**
   * This method create to set the optimizedShippingEnd on consignment
   * @param stringStringMap response from UPS Scrape service
   * @param extendOrderModel order model to get update
   */
  private void setOptimizedShippingEndDateForConsignment(final Map<String, Date> stringStringMap,
      final OrderModel extendOrderModel) {
    if(CollectionUtils.isNotEmpty(extendOrderModel.getConsignments()) && MapUtils.isNotEmpty(stringStringMap)){
      extendOrderModel.getConsignments().forEach(consignmentModel -> {
        if(Objects.nonNull(stringStringMap.get(consignmentModel.getCode()))){
          consignmentModel.setOptimizedShippingEndDate(stringStringMap.get(consignmentModel.getCode()));
          getModelService().save(consignmentModel);
          getModelService().refresh(consignmentModel);
          BlLogger.logFormattedMessage(LOG , Level.DEBUG , "New OptimizedShippingEndDate  as {} for extend order {}" ,
              String.valueOf(consignmentModel.getOptimizedShippingEndDate()) , extendOrderModel.getCode());
        }
      });
    }
  }

  /**
   * This method create to get the stock start date for extend order
   * @param abstractOrderModel model to get the extend order list
   * @param consignmentModel consignment to get the optimizedShippingEndDate
   * @return date from consignment
   */
  private Date getNewStockStartDate(final OrderModel abstractOrderModel, final ConsignmentModel consignmentModel) {
    final AtomicReference<Date> dateAtomicReference = new AtomicReference<>(additonalDaysForStock(consignmentModel));
    if(CollectionUtils.isNotEmpty(abstractOrderModel.getExtendedOrderCopyList())){
      final  List<AbstractOrderModel> orderModelList = abstractOrderModel.getExtendedOrderCopyList();
      final int size = orderModelList.size();
      for (final AbstractOrderModel extendOrder :orderModelList) {
        if (BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) && extendOrder
            .getExtendOrderStatus().getCode()
            .equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())
            && orderModelList.get(size - 1).getPk()
            .equals(extendOrder.getPk())) {
          extendOrder.getConsignments().forEach(extendOrderconsignmentModel -> {
            if(consignmentModel.getCode().equalsIgnoreCase(extendOrderconsignmentModel.getCode())){
              dateAtomicReference.set(additonalDaysForStock(extendOrderconsignmentModel));
            }
          });
        }
      }
    }
    return dateAtomicReference.get();
  }

  /**
   * This method created to add the number of days for stock
   * @param consignmentModel to get the optimized shipping end date
   * @return date from consignment
   */
  private Date additonalDaysForStock(final ConsignmentModel consignmentModel){
    return BlDateTimeUtils.addingNoOfDaysInGivenDate(consignmentModel.getOptimizedShippingEndDate(),1);
  }

    /**
     * Gets the number of days to add.
     *
     * @param order the order
     * @param consignment the consignment
     * @return the number of days to add
     */
    private int getNumberOfDaysToAdd(final OrderModel order, final ConsignmentModel consignment)
    {
        if (Objects.nonNull(consignment) && Objects.nonNull(consignment.getOptimizedShippingType()))
        {
            final String optimizedShippingType = consignment.getOptimizedShippingType().getCode();
            return getNumberOfDaysForDeliveryMethod().containsKey(optimizedShippingType)
                    ? getNumberOfDaysForDeliveryMethod().get(optimizedShippingType)
                    : 0;
        }
        if (Objects.nonNull(order) && order.getDeliveryMode() instanceof ZoneDeliveryModeModel)
        {
            final ZoneDeliveryModeModel deliveryMode = ((ZoneDeliveryModeModel) order.getDeliveryMode());
            final String postReservedDays = deliveryMode.getPostReservedDays();
            return StringUtils.isNotBlank(postReservedDays) && NumberUtils.isCreatable(postReservedDays)
                    ? Integer.parseInt(postReservedDays)
                    : 0;
        }
        return 0;
    }

  public BlCartService getBlCartService() {
    return blCartService;
  }

  public void setBlCartService(BlCartService blCartService) {
    this.blCartService = blCartService;
  }

  public CommerceCartService getCommerceCartService() {
    return commerceCartService;
  }

  public void setCommerceCartService(
      CommerceCartService commerceCartService) {
    this.commerceCartService = commerceCartService;
  }

  public BlCartFacade getBlCartFacade() {
    return blCartFacade;
  }

  public void setBlCartFacade(BlCartFacade blCartFacade) {
    this.blCartFacade = blCartFacade;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public Converter<CommerceCartModification, CartModificationData> getCartModificationConverter() {
    return cartModificationConverter;
  }

  public void setCartModificationConverter(
      Converter<CommerceCartModification, CartModificationData> cartModificationConverter) {
    this.cartModificationConverter = cartModificationConverter;
  }

  public PriceDataFactory getPriceDataFactory() {
    return priceDataFactory;
  }

  public void setPriceDataFactory(
      PriceDataFactory priceDataFactory) {
    this.priceDataFactory = priceDataFactory;
  }

  public BlCommercePriceService getCommercePriceService() {
    return commercePriceService;
  }

  public void setCommercePriceService(
      BlCommercePriceService commercePriceService) {
    this.commercePriceService = commercePriceService;
  }

  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }

  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(SessionService sessionService) {
    this.sessionService = sessionService;
  }


  public DefaultBlExtendOrderService getDefaultBlExtendOrderService() {
    return defaultBlExtendOrderService;
  }

  public void setDefaultBlExtendOrderService(
      DefaultBlExtendOrderService defaultBlExtendOrderService) {
    this.defaultBlExtendOrderService = defaultBlExtendOrderService;
  }

  public DefaultBlCalculationService getDefaultBlCalculationService() {
    return defaultBlCalculationService;
  }

  public void setDefaultBlCalculationService(
      DefaultBlCalculationService defaultBlCalculationService) {
    this.defaultBlCalculationService = defaultBlCalculationService;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getPendingDate(final String orderCode) {
    final OrderModel orderModel = getOrderModelFromOrderCode(orderCode);
    final Date date =(BooleanUtils.isTrue(orderModel.isGiftCardOrder()) || BooleanUtils.isFalse(orderModel.getIsRentalOrder())) ? orderModel.getDate() : orderModel.getRentalStartDate();
    return BlDateTimeUtils.convertDateToStringDate(BlDateTimeUtils.addingNoOfDaysInGivenDate(date, 7), "MM/dd/yy");
  }

  public PromotionsService getPromotionsService() {
    return promotionsService;
  }

  public void setPromotionsService(PromotionsService promotionsService) {
    this.promotionsService = promotionsService;
  }

  public BaseSiteService getBaseSiteService() {
    return baseSiteService;
  }

  public void setBaseSiteService(BaseSiteService baseSiteService) {
    this.baseSiteService = baseSiteService;
  }

  public TimeService getTimeService() {
    return timeService;
  }

  public void setTimeService(TimeService timeService) {
    this.timeService = timeService;
  }


  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }

  public BlExtendRentalOrderDetailsPopulator getBlExtendRentalOrderDetailsPopulator() {
    return blExtendRentalOrderDetailsPopulator;
  }

  public void setBlExtendRentalOrderDetailsPopulator(
      BlExtendRentalOrderDetailsPopulator blExtendRentalOrderDetailsPopulator) {
    this.blExtendRentalOrderDetailsPopulator = blExtendRentalOrderDetailsPopulator;
  }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }


  public BlOrderAppliedVouchersPopulator getBlOrderAppliedVouchersPopulator() {
    return blOrderAppliedVouchersPopulator;
  }

  public void setBlOrderAppliedVouchersPopulator(
      BlOrderAppliedVouchersPopulator blOrderAppliedVouchersPopulator) {
    this.blOrderAppliedVouchersPopulator = blOrderAppliedVouchersPopulator;
  }

  public DefaultBlExternalTaxesService getDefaultBlExternalTaxesService() {
    return defaultBlExternalTaxesService;
  }

  public void setDefaultBlExternalTaxesService(
      DefaultBlExternalTaxesService defaultBlExternalTaxesService) {
    this.defaultBlExternalTaxesService = defaultBlExternalTaxesService;
  }

/**
 * @return the blOrderService
 */
public BlOrderService getBlOrderService()
{
	return blOrderService;
}


/**
 * @param blOrderService the blOrderService to set
 */
public void setBlOrderService(BlOrderService blOrderService)
{
	this.blOrderService = blOrderService;
}
}
