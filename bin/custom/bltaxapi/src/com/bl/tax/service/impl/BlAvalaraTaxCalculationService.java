package com.bl.tax.service.impl;

import com.bl.tax.BltaxapiStandalone;
import com.bl.tax.DetailResponse;
import com.bl.tax.TaxLineResponse;
import com.bl.tax.TaxResponse;
import com.bl.tax.constants.BltaxapiConstants;
import com.fedex.ship.stub.Tax;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import java.util.List;

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
    setTaxRateOnOrderModel(taxResponse , abstractOrderModel);
  }

  /**
   * This method created to set the tax rate on order model
   * @param taxResponse taxresponse
   * @param abstractOrderModel abstractOrderModel
   */
  private void setTaxRateOnOrderModel(final TaxResponse taxResponse, final AbstractOrderModel abstractOrderModel) {
    if(CollectionUtils.isNotEmpty(taxResponse.getTaxLines())) {
      final TaxLineResponse taxLineResponse = taxResponse.getTaxLines().iterator().next();
      if(CollectionUtils.isNotEmpty(taxLineResponse.getDetails())) {
       final DetailResponse detailResponse =  taxLineResponse.getDetails().iterator().next();
       if(StringUtils.equalsIgnoreCase(BltaxapiConstants.JURIS_TYPE , detailResponse.getJurisType())) {
         final Double convertIntoDecimal = detailResponse.getRate() * 100;
         abstractOrderModel.setTaxRate(convertIntoDecimal);
       }
      }
    }
  }


  /**
   * this method set the total tax return from the avalara
   */
  private Double setTotalTaxToOrder(final TaxResponse taxResponse) {
    return taxResponse.getTotalTax() > 0.0 ? taxResponse.getTotalTax() : 0.0;
  }
}
