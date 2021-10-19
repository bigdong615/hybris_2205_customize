package com.bl.tax.populators;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.tax.TaxResponse;
import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.service.impl.BlAvalaraTaxCalculationService;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * This populator created for adding line item and order tax to order
 * @author Manikandan
 */
public class BlAvalaraTaxPopulator implements Populator<TaxResponse, AbstractOrderModel> {

  private BlAvalaraTaxCalculationService blAvalaraTaxCalculationService;


  /**
   * this method created for setting line item tax and order level tax to abstractOrderModel
   * @param taxResponse tax response from avalara
   * @param abstractOrderModel abstractOrderModel
   */

  @Override
  public void populate(final TaxResponse taxResponse, final AbstractOrderModel abstractOrderModel) {
    if (BooleanUtils.isFalse(abstractOrderModel.isUnPaidBillPresent())) {
      for (int i = 0; i < taxResponse.getTaxLines().size(); i++) {
        final AtomicReference<String> responseProductCode = new AtomicReference<>(taxResponse.getTaxLines().get(i).getItemCode());
         String[] split = new String[]{};
        if(StringUtils.isNotBlank(taxResponse.getTaxLines().get(i).getDescription()) && taxResponse.getTaxLines().get(i).getDescription().contains(BltaxapiConstants.PRODUCT_ID)) {
          split =taxResponse.getTaxLines().get(i).getDescription().split(BltaxapiConstants.PRODUCT_ID);
        }
        for (int j = 0; j < abstractOrderModel.getEntries().size(); j++) {
          final String productCode = abstractOrderModel.getEntries().get(j).getProduct().getCode();
          if (StringUtils.equalsIgnoreCase(productCode , responseProductCode.get()) ||
              StringUtils.equalsIgnoreCase(getProductIDFromProduct(abstractOrderModel.getEntries().get(j).getProduct()),
                 split.length > 1 && Objects.nonNull(split[1]) ? split[1] : StringUtils.EMPTY)) {
            abstractOrderModel.getEntries().get(j).setAvalaraLineTax(taxResponse.getTaxLines().get(i).getTax());
          }
        }
      }
    }
    getBlAvalaraTaxCalculationService().calculateTaxWithOrderTotal(abstractOrderModel , taxResponse);
    }

  /**
   * This method created to ger product id from product
   * @param product product
   * @return String
   */
  private String getProductIDFromProduct(final ProductModel product) {
    final AtomicReference<String> stringAtomicReference = new AtomicReference<>(StringUtils.EMPTY);
    if(product instanceof BlSerialProductModel) {
      final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) product;
      stringAtomicReference.set(blSerialProductModel.getProductId());
    }
    else {
      final BlProductModel blProductModel = (BlProductModel) product;
      stringAtomicReference.set(blProductModel.getProductId());
    }
    return stringAtomicReference.get();
  }


  public BlAvalaraTaxCalculationService getBlAvalaraTaxCalculationService() {
    return blAvalaraTaxCalculationService;
  }

  public void setBlAvalaraTaxCalculationService(
      BlAvalaraTaxCalculationService blAvalaraTaxCalculationService) {
    this.blAvalaraTaxCalculationService = blAvalaraTaxCalculationService;
  }


}

