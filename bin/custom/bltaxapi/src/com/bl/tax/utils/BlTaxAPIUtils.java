package com.bl.tax.utils;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.lang3.StringUtils;

/**
 * This Util class created for common methods in avalara tax
 */
public class BlTaxAPIUtils {

  private BlTaxAPIUtils()
  {
    //empty to avoid instantiating utils class
  }

  /**
   * This method created to get product id from entry
   * @param productModel productModel
   * @return String
   */
  public static String getProductId(final ProductModel productModel) {
    final AtomicReference<String> stringAtomicReference = new AtomicReference<>(StringUtils.EMPTY);
    if(productModel instanceof BlSerialProductModel) {
      final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) productModel;
      stringAtomicReference.set(StringUtils.isBlank(blSerialProductModel.getProductId()) ? StringUtils.EMPTY : blSerialProductModel.getProductId());
    }
    else {
      final BlProductModel blProductModel = (BlProductModel) productModel;
      stringAtomicReference.set(StringUtils.isBlank(blProductModel.getProductId()) ? StringUtils.EMPTY : blProductModel.getProductId());
    }
    return stringAtomicReference.get();
  }

}
