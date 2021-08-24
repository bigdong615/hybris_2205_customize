package com.bl.tax.service.impl;

import com.bl.tax.TaxResponse;
import de.hybris.platform.core.model.order.AbstractOrderModel;

/**
 * This class is created for setting avalara tax with order
 * @author Manikandan
 */

public class BlAvalaraTaxCalculationService {

  /**
   * This method set the order tax exempt bases on condition
   * @param abstractOrderModel the order model
   * @param taxResponse tax response
   */
  public void calculateTaxWithOrderTotal(final AbstractOrderModel abstractOrderModel,
      final TaxResponse taxResponse) {
    abstractOrderModel.setTotalTax(setTotalTaxToOrder(taxResponse));
    abstractOrderModel.setIsOrderTaxExempt(0.0 < taxResponse.getTotalExempt()
        && 0.0 == taxResponse.getTotalTax() ? Boolean.TRUE : Boolean.FALSE);
  }


  /**
   * this method set the total tax return from the avalara
   */
  private Double setTotalTaxToOrder(final TaxResponse taxResponse) {
    return taxResponse.getTotalTax() > 0.0 ? taxResponse.getTotalTax() : 0.0;
  }
}
