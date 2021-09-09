package com.bl.core.product.service;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.ProductService;


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

  /**
   * It checks the product is aquatech product or not
   * @param productModel
   * @return true if the product is aquatech product
   */
  boolean isAquatechProduct(final ProductModel productModel);
}
