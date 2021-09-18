package com.bl.core.product.service;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.ProductService;
import java.util.List;


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
  
  /**
   * Sets the last user changed condition rating.
   *
   * @param blSerialProduct the new last user changed condition rating
   */
  void setLastUserChangedConditionRating(final BlSerialProductModel blSerialProduct);

  /**
   * It changes buffer inventory flag in staged version of product model
   * @param productCode the product code
   * @param isBufferInventory is buffer inventory flag
   */
  void changeBufferInvFlagInStagedVersion(final String productCode, final Boolean isBufferInventory);

  /**
   * It gets serialProductModel of staged version
   *
   * @param productCode the product code
   * @param catalogVersionModel the catalog version model
   * @return List<BlSerialProductModel> the blSerialProducts
   */
  public List<BlSerialProductModel> getProductsOfStagedVersion(final String productCode,
      final CatalogVersionModel catalogVersionModel);
}
