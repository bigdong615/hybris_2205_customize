package com.bl.facades.order;

import com.bl.facades.cart.BlCartFacade;
import de.hybris.platform.commercefacades.order.impl.DefaultOrderFacade;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.store.BaseStoreModel;

public class DefaultBlOrderFacade extends DefaultOrderFacade implements BlOrderFacade {



  private BlCartFacade blCartFacade;


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
      getBlCartFacade().addToCart(productCode, quantity ,"");
    }

  }


  public BlCartFacade getBlCartFacade() {
    return blCartFacade;
  }

  public void setBlCartFacade(BlCartFacade blCartFacade) {
    this.blCartFacade = blCartFacade;
  }


}
