package com.bl.tax.populators;

import com.bl.tax.TaxResponse;
import com.bl.tax.service.impl.BlAvalaraTaxCalculationService;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import org.apache.commons.lang3.BooleanUtils;

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
        final String responseProductCode = taxResponse.getTaxLines().get(i).getItemCode();
        for (int j = 0; j < abstractOrderModel.getEntries().size(); j++) {
          final String productCode = abstractOrderModel.getEntries().get(j).getProduct().getCode();
          if (productCode.equalsIgnoreCase(responseProductCode)) {
            abstractOrderModel.getEntries().get(j)
                .setAvalaraLineTax(taxResponse.getTaxLines().get(i).getTax());
          }
        }
      }
    }
    getBlAvalaraTaxCalculationService().calculateTaxWithOrderTotal(abstractOrderModel , taxResponse);
    }


  public BlAvalaraTaxCalculationService getBlAvalaraTaxCalculationService() {
    return blAvalaraTaxCalculationService;
  }

  public void setBlAvalaraTaxCalculationService(
      BlAvalaraTaxCalculationService blAvalaraTaxCalculationService) {
    this.blAvalaraTaxCalculationService = blAvalaraTaxCalculationService;
  }


}

