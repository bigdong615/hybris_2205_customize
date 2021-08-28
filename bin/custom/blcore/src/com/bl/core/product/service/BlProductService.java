package com.bl.core.product.service;

import com.bl.core.enums.SerialStatusEnum;
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

  /**
   * It checks the serial product status
   * @param serialStatusEnum serial status enum
   * @return true if the serial product status is active
   */
  boolean isActiveSerialProduct(final SerialStatusEnum serialStatusEnum);
}
