package com.bl.facades.order;

import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.core.model.product.ProductModel;

public interface BlOrderFacade extends OrderFacade {

  void addToCartAllOrderEnrties(final String orderCode) throws CommerceCartModificationException;

  void addToCart(ProductModel lProductModel, final String productCode, final int quantity) throws CommerceCartModificationException;

}
