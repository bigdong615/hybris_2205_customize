package com.bl.facades.order;

import com.bl.core.data.StockResult;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.impl.DefaultBlCalculationService;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.services.extendorder.impl.DefaultBlExtendOrderService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlExtendOrderUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.populators.BlExtendRentalOrderDetailsPopulator;
import com.bl.logging.BlLogger;
import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.impl.DefaultOrderFacade;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
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

  /**
   * This method created to add all the products from existing order
   */
  @Override
  public boolean addToCartAllOrderEnrties(final String orderCode , final Model model) throws CommerceCartModificationException
  {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode, baseStoreModel);

    final CartModel cartModel = getBlCartService().getSessionCart();

    if(null != cartModel && CollectionUtils.isNotEmpty(cartModel.getEntries()) && BooleanUtils.isFalse(cartModel.getIsRentalCart())) {
      return false;
    }

    for (final AbstractOrderEntryModel lEntryModel : orderModel.getEntries())
    {
        final ProductModel lProductModel = lEntryModel.getProduct();
        addToCart(lProductModel, lEntryModel.getQuantity().intValue(), lEntryModel);
    }

    return true;
  }


  /**
   * This method created for adding product to cart when user renting again from previous order
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
    } catch (Exception exception) {
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
        cartModel.setIsRentalCart(Boolean.TRUE);
      } else {
        cartModel.setIsRentalCart(Boolean.FALSE);
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
    parameter.setIsNODamageWaiverSelected(abstractOrderEntryModel.getNoDamageWaiverSelected());
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
    final Date stockStartDate = setAdditionalDaysForStock(startDate);
    final Date stockEndDate = setAdditionalDaysForStock(endDate);
    final List<StockResult> stockResults = new ArrayList<>();
    checkStockAvailablity(orderModel , orderData , stockStartDate , stockEndDate , stockResults);
    if (CollectionUtils.isEmpty(stockResults)) {
      populateExtendOrderDetails(startDate , endDate , selectedDate , orderModel , orderData , stockEndDate);
    }
    return orderData;
  }

  private void checkStockAvailablity(final OrderModel orderModel , final OrderData orderData , final Date stockStartDate ,
      final Date stockEndDate , List<StockResult> stockResults) {
    for (final ConsignmentModel consignmentModel : orderModel.getConsignments()) {
      for (final ConsignmentEntryModel consignmentEntryModel : consignmentModel.getConsignmentEntries()) {
        for (final BlProductModel blProductModel : consignmentEntryModel
            .getSerialProducts()) {
          checkProductForAvailablity(blProductModel , stockStartDate , stockEndDate , stockResults , orderData);
        }

      }
    }
  }

  private void checkProductForAvailablity(final BlProductModel blProductModel , final Date stockStartDate ,
      final Date stockEndDate , final List<StockResult> stockResults , final OrderData orderData )
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
              .setExtendErrorMessage("This item is no longer available for your Extend Order.");
          BlLogger.logMessage(LOG, Level.INFO, "product cannot be extend",
              blSerialProductModel.getCode());
        }
      }
    }


  private void populateExtendOrderDetails(final Date startDate ,final Date endDate , final String selectedDate ,
      final OrderModel orderModel , final OrderData orderData , final Date stockEndDate) {
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
    extendOrderModel.setTotaExtendDays((int) defaultAddedTimeForExtendRental); // To set total number of days extended
    Calendar extendStartDate = Calendar.getInstance();
    extendStartDate.setTime(startDate);
    extendStartDate.add(Calendar.DAY_OF_MONTH ,1);
    extendOrderModel.setExtendStartEndDate(extendStartDate.getTime());
    extendOrderModel.setRentalEndDate(endDate);    // End Date will be stored based on customer selection
    extendOrderModel.setActualRentalEndDate(stockEndDate);
    try {
      getDefaultBlCalculationService()
          .recalculateForExtendOrder(extendOrderModel, (int) defaultAddedTimeForExtendRental);
      getPromotionsService()
          .updatePromotions(getPromotionGroups(), extendOrderModel, true, AutoApplyMode.APPLY_ALL,
              AutoApplyMode.APPLY_ALL, getTimeService().getCurrentTime());
    } catch (CalculationException e) {
      e.printStackTrace();
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
    final BigDecimal orderTotalWithTax = BigDecimal.valueOf(extendOrderModel.getSubtotal())
        .add(BigDecimal.valueOf(extendOrderModel.getTotalDamageWaiverCost())).
            add(BigDecimal.valueOf(extendOrderModel.getTotalTax()));

    orderData.setOrderTotalWithTaxForExtendRental(getPriceDataFactory()
        .create(priceType, orderTotalWithTax, extendOrderModel.getCurrency().getIsocode()));

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
   final Calendar calendar = Calendar.getInstance();
   calendar.setTime(dateToAdd);
   calendar.add(Calendar.DAY_OF_MONTH ,2);
   return calendar.getTime();
 }

  /**
   * To set additional date for startDate
   */
  private Date setAdditionalDaysForStartDate(final Date dateToAdd) {
    final Calendar calendar = Calendar.getInstance();
    calendar.setTime(dateToAdd);
    calendar.add(Calendar.DAY_OF_MONTH ,1);
    return calendar.getTime();
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
   * This method created to get order model from order code
   */
  @Override
  public OrderModel getOrderModelFromOrderCode(final String orderCode){
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    return getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode, baseStoreModel);
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


}
