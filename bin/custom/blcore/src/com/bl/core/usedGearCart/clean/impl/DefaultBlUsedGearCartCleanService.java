package com.bl.core.usedGearCart.clean.impl;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.usedGearCart.clean.BlUsedGearCartCleanService;
import com.bl.core.utils.BlUpdateStagedProductUtils;
import de.hybris.platform.catalog.daos.CatalogVersionDao;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.daos.ProductDao;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.beans.factory.annotation.Autowired;


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
    final List<CartModel> carts  = orderDao.getAllUsedGearAbandonedCarts();
    carts.stream().forEach(cartModel -> {
      blCartService.clearCartEntries(cartModel);
    });
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
