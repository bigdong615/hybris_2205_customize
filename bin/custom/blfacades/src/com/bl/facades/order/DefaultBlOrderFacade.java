package com.bl.facades.order;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.impl.DefaultOrderFacade;
import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.store.BaseStoreModel;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;

public class DefaultBlOrderFacade extends DefaultOrderFacade implements BlOrderFacade {

  private BlCartFacade blCartFacade;
  private BlCartService blCartService;
  private CommerceCartService commerceCartService;
  private ModelService modelService;
  private Converter<CommerceCartModification, CartModificationData> cartModificationConverter;


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



}
