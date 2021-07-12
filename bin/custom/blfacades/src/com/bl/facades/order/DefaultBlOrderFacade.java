package com.bl.facades.order;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.constants.BlFacadesConstants;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
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
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.store.BaseStoreModel;
import java.math.BigDecimal;
import java.util.Date;
import org.apache.commons.lang.StringUtils;

public class DefaultBlOrderFacade extends DefaultOrderFacade implements BlOrderFacade {

  private BlCartFacade blCartFacade;
  private BlCartService blCartService;
  private CommerceCartService commerceCartService;
  private ModelService modelService;
  private Converter<CommerceCartModification, CartModificationData> cartModificationConverter;
  private PriceDataFactory priceDataFactory;
  private BlCommercePriceService commercePriceService;
  private ProductService productService;
  protected SessionService sessionService;



  @Override
  public void addToCartAllOrderEnrties(String orderCode) throws CommerceCartModificationException {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode, baseStoreModel);
    for (final AbstractOrderEntryModel lEntryModel : orderModel.getEntries())
    {
      final ProductModel lProductModel = lEntryModel.getProduct();
      addToCart(lProductModel, lEntryModel.getProduct().getCode(), lEntryModel.getQuantity().intValue());
    }
  }

  public void addToCart(ProductModel lProductModel, String productCode, int quantity)
      throws CommerceCartModificationException {

    if (lProductModel != null)
    {
     // getBlCartFacade().addToCart(productCode, quantity ,"");
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
  public void calculatePriceForExtendOrders(final OrderData orderData, final String orderEndDate,
      final String selectedDate) throws CommerceCartModificationException {

    final Date startDate =BlDateTimeUtils.convertStringDateToDate(orderEndDate, "MM/dd/yyyy");
     final Date endDate =  BlDateTimeUtils.convertStringDateToDate(selectedDate ,"EE MMM dd yyyy");
    long defaultAddedTimeForExtendRental = BlDateTimeUtils
        .getDaysBetweenDates(startDate, endDate) + 1;
    if(StringUtils.isEmpty(selectedDate)) {
       defaultAddedTimeForExtendRental = 1;
    }
    sessionService.getCurrentSession().setAttribute("extendedRentalDuration", (int) defaultAddedTimeForExtendRental);

    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();

    OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderData.getCode(),
        baseStoreModel);

    orderData.setAddedTimeForExtendRental((int) defaultAddedTimeForExtendRental); // Default value which added for extend order
    PriceDataType priceType = PriceDataType.BUY;
    PriceInformation info;
    BigDecimal subTotal = BigDecimal.valueOf(0.0);

    for(OrderEntryData entries : orderData.getEntries()) {
      final ProductModel productModel = getProductService().getProductForCode(entries.getProduct().getCode());
      info = getCommercePriceService().getWebPriceForExtendProduct(productModel, defaultAddedTimeForExtendRental);
      if (info != null) {
        subTotal = subTotal.add(BigDecimal.valueOf(info.getPriceValue().getValue()).setScale(
            BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
      }

    }
    orderData.setTotalCostForExtendRental(getPriceDataFactory().create(priceType, subTotal , "USD"));

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


}
