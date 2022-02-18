package com.bl.core.usedGearCart.clean.impl;

import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.usedGearCart.clean.BlUsedGearCartCleanService;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;


/**
 * It is to clear the abandoned used gear carts
 * @author Moumita
 */
public class DefaultBlUsedGearCartCleanService implements BlUsedGearCartCleanService
{
  private BlOrderDao orderDao;
  private BlCartService blCartService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void cleanUsedGearAbandonedCart() {
    final List<CartEntryModel> cartEntries  = orderDao.getAllUsedGearAbandonedCarts();
    final Set<CartModel> carts = cartEntries.stream().map(CartEntryModel::getOrder).collect(Collectors.toSet());
    carts.stream().forEach(cartModel ->
      blCartService.clearCartEntries(cartModel)
    );
  }

  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }

  public BlCartService getBlCartService() {
    return blCartService;
  }

  public void setBlCartService(BlCartService blCartService) {
    this.blCartService = blCartService;
  }
}
