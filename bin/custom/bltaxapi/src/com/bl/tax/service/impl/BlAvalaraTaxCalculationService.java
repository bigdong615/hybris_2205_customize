package com.bl.tax.service.impl;
import com.bl.tax.TaxResponse;
import de.hybris.platform.core.model.order.AbstractOrderModel;


public class BlAvalaraTaxCalculationService {

  public void calculateTaxWithOrderTotal(final AbstractOrderModel abstractOrderModel , final TaxResponse taxResponse)
  {
      abstractOrderModel.setTotalAvalaraTaxCalculated(setTotalTaxToOrder(taxResponse));
}

  private Double setTotalTaxToOrder(final TaxResponse taxResponse) {
    return taxResponse.getTotalTax() > 0.0 ? taxResponse.getTotalTax() : 0.0;
  }

}
