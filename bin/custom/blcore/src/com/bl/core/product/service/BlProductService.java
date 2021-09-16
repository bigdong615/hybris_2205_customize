package com.bl.core.product.service;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
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
   * @param productModel productModel
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
   *  This method use to get all bundle entry from given parent bundle entry.
   * @param parentBundleEntry parent bundle entry.
   * @return list of bundle product reference.
   */
  public List<ProductReferenceModel> getBundleProductReferenceModelFromEntry(final AbstractOrderEntryModel parentBundleEntry);

  /**
   *  This method use to get all bundle entry from given product.
   * @param product main bundle product.
   * @return list of bundle product reference.
   */
  List<ProductReferenceModel>  getBundleProductReferenceModel(final ProductModel product);
}
