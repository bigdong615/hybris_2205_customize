package com.bl.core.services.order.impl;

import com.bl.core.product.service.BlProductService;
import com.bl.core.services.order.BlOrderService;
import de.hybris.platform.core.model.order.AbstractOrderModel;

/**
 * This class is used to implement various order related functionalities.
 *
 * @author Sunil Sahu
 */
public class DefaultBlOrderService implements BlOrderService {

  private BlProductService productService;

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAquatechProductsPresentInOrder(final AbstractOrderModel orderModel) {

    return orderModel.getEntries().stream().anyMatch(
        entry -> productService.isAquatechProduct(entry.getProduct()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAquatechProductOrder(final AbstractOrderModel orderModel) {

    return orderModel.getEntries().stream().allMatch(
        entry -> productService.isAquatechProduct(entry.getProduct()));
  }


  public BlProductService getProductService() {
    return productService;
  }

  public void setProductService(BlProductService productService) {
    this.productService = productService;
  }
}
