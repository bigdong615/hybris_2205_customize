package com.bl.core.product.dao;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.product.daos.ProductDao;

import java.util.Collection;

import com.bl.core.model.BlProductModel;
import java.util.Set;


/**
 * It is used to fetch the products
 *
 * @author Moumita
 */
public interface BlProductDao extends ProductDao {

  /**
   * It fetches all the active sku products present in system
   *
   * @return Collection<BlProductModel> the list of sku products
   */
  public Collection<BlProductModel> getAllActiveSkuProducts();

  /**
   * It fetches all the serial products for given set of codes.
   *
   * @param serialProductCodes the product codes.
   * @return Collection<BlSerialProductModel> the list of sku products
   */
  public Collection<BlSerialProductModel> getBlSerialProductsForCodes(
      Set<String> serialProductCodes);

}
