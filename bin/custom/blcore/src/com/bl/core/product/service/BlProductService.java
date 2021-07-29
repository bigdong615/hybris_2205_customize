package com.bl.core.product.service;

import de.hybris.platform.product.ProductService;

import com.bl.core.model.BlSerialProductModel;


/**
 * Extended Custom Product Service to derive custom business logic
 *
 * @author Ravikumar
 */
public interface BlProductService extends ProductService {

  /**
   * Checks if functional condition and cosmetic condition is available on serial.
   *
   * @param blSerialProductModel the bl serial product model
   * @return true, if is functional and cosmetic is available
   */
  boolean isFunctionalAndCosmeticIsAvailable(final BlSerialProductModel blSerialProductModel);
}
