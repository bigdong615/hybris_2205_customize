package com.bl.facades.order;

import com.bl.core.data.StockResult;
import com.bl.core.enums.SerialStatusEnum;
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
import com.bl.facades.product.data.AvailabilityMessage;
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
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is created for My account rent again and extend order functionality
 * @author Manikandan
 */
public class DefaultBlOrderFacade extends DefaultOrderFacade implements BlOrderFacade {

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

  private static final Logger LOG = Logger.getLogger(DefaultBlOrderFacade.class);
  /**
   * This method created to add all the products from existing order
   */
  @Override
  public void addToCartAllOrderEnrties(final String orderCode) throws CommerceCartModificationException
  {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode, baseStoreModel);
    for (final AbstractOrderEntryModel lEntryModel : orderModel.getEntries())
    {
      final ProductModel lProductModel = lEntryModel.getProduct();
      addToCart(lProductModel , lProductModel.getCode() ,  lEntryModel.getQuantity().intValue());
    }
  }

  /**
   * This method created to add product to cart
   */
  public void addToCart(final ProductModel lProductModel , String orderCode , final int quantity)
      throws CommerceCartModificationException {
    if (lProductModel != null)
    {
      addToCart(lProductModel, quantity);
    }

  }

  public CartModificationData addToCart(final ProductModel blProductModel, final long quantity)
      throws CommerceCartModificationException {

    CartModel cartModel = blCartService.getSessionCart();
    final CommerceCartParameter parameter = new CommerceCartParameter();

    try {
        //For rental product
        parameter.setProduct(blProductModel);
        parameter.setUnit(blProductModel.getUnit());
        parameter.setCreateNewEntry(false);
    } catch (Exception exception) {
     //
    }

    parameter.setEnableHooks(true);
    parameter.setCart(cartModel);
    parameter.setQuantity(quantity);

    final CommerceCartModification commerceCartModification = getCommerceCartService()
        .addToCart(parameter);
    setCartType(null, cartModel, commerceCartModification);

    return getCartModificationConverter().convert(commerceCartModification);
  }

  private void setCartType(final BlSerialProductModel blSerialProductModel,
      final CartModel cartModel,
      final CommerceCartModification commerceCartModification) {
    if (commerceCartModification != null && commerceCartModification.getStatusCode()
        .equals(BlFacadesConstants.SUCCESS)) {
      if (blSerialProductModel == null) {
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




  @Override
  public OrderData calculatePriceForExtendOrders(final OrderModel orderModel , final OrderData orderData, final String orderEndDate,
      final String selectedDate) {

    Date startDate = BlDateTimeUtils.convertStringDateToDate(orderEndDate, "MM/dd/yyyy");
    final Date endDate = BlDateTimeUtils.convertStringDateToDate(selectedDate, "EE MMM dd yyyy");

    Calendar calendar = Calendar.getInstance();
    calendar.setTime(startDate);
    calendar.add(Calendar.DAY_OF_MONTH ,2);
    startDate = calendar.getTime();


    List<StockResult> stockResults = new ArrayList<>();

    for (ConsignmentModel consignmentModel : orderModel.getConsignments()) {
      for (ConsignmentEntryModel consignmentEntryModel : consignmentModel.getConsignmentEntries()) {
        for (BlSerialProductModel blSerialProductModel : consignmentEntryModel
            .getSerialProducts()) {
          StockResult stockResult = getBlCommerceStockService()
              .getStockForEntireExtendDuration(blSerialProductModel.getCode(),
                  Collections.singleton(blSerialProductModel.getWarehouseLocation())
                  , startDate, endDate);
          if (stockResult.getStockLevelStatus().getCode()
              .equalsIgnoreCase(StockLevelStatus.OUTOFSTOCK.getCode())) {
            stockResults.add(stockResult);
            orderData.setIsAllProductExtendabe(false);
            orderData.setExtendErrorMessage("This item is no longer available for your Extend Order.");
            BlLogger.logMessage(LOG , Level.INFO , "product cannot be extend" , blSerialProductModel.getCode());
          }
        }
      }
    }

    if (CollectionUtils.isEmpty(stockResults)) {
      long defaultAddedTimeForExtendRental = BlDateTimeUtils
          .getDaysBetweenDates(startDate, endDate) + 1;
      if (StringUtils.isEmpty(selectedDate)) {
        defaultAddedTimeForExtendRental = 1;
      }
      orderData.setAddedTimeForExtendRental(
          (int) defaultAddedTimeForExtendRental); // Default value which added for extend order
      PriceDataType priceType = PriceDataType.BUY;
      final OrderModel extendOrderModel = getDefaultBlExtendOrderService()
          .cloneOrderModelForExtendRental(orderModel);
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
    return orderData;
  }

  @Override
  public OrderData setRentalExtendOrderDetails(final String orderCode , final String rentalEndDate ,final  String selectedDate)
      throws CommerceCartModificationException {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
   final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode,
        baseStoreModel);
   // Calculate order details based on new return date for extend rental order
    return calculatePriceForExtendOrders(orderModel , getOrderConverter().convert(orderModel), rentalEndDate , selectedDate);
  }

  protected Collection<PromotionGroupModel> getPromotionGroups()
  {
    final Collection<PromotionGroupModel> promotionGroupModels = new ArrayList<>();
    if (getBaseSiteService().getCurrentBaseSite() != null
        && getBaseSiteService().getCurrentBaseSite().getDefaultPromotionGroup() != null)
    {
      promotionGroupModels.add(getBaseSiteService().getCurrentBaseSite().getDefaultPromotionGroup());
    }
    return promotionGroupModels;
  }

  private AvailabilityMessage getMessage(final String messageCode, final List<String> arguments)
  {
    final AvailabilityMessage message = new AvailabilityMessage();
    message.setMessageCode(messageCode);
    message.setArguments(arguments);
    return message;
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

}
