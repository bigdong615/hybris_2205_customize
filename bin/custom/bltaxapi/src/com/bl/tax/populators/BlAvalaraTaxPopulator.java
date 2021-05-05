package com.bl.tax.populators;

import com.bl.tax.TaxResponse;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;

/**
 * This populator created for adding line item and order tax to order
 * @author Manikandan
 */
public class BlAvalaraTaxPopulator implements Populator<TaxResponse, AbstractOrderModel> {

  /**
   * this method created for setting line item tax and order level tax to abstractOrderModel
   * @param taxResponse tax response from avalara
   * @param abstractOrderModel abstractOrderModel
   */

  @Override
  public void populate(final TaxResponse taxResponse, final AbstractOrderModel abstractOrderModel) {
      for(int i = 0 ; i < taxResponse.getTaxLines().size() ; i ++) {
        final String responseProductCode = taxResponse.getTaxLines().get(i).getItemCode();
        for(int  j = 0; j <abstractOrderModel.getEntries().size() ; j++ ) {
         final String productCode =  abstractOrderModel.getEntries().get(j).getProduct().getCode();
         if(productCode.equalsIgnoreCase(responseProductCode)) {
           abstractOrderModel.getEntries().get(j).setAvalaraLineTax(taxResponse.getTaxLines().get(i).getTax());
         }
        }
      }
    final Double prevAvalaraTax = abstractOrderModel.getTotalAvalaraTaxCalculated();
      if(null != prevAvalaraTax && prevAvalaraTax > 0.0) {
        abstractOrderModel.setTotalPrice(abstractOrderModel.getTotalPrice() - prevAvalaraTax);
      }

     final Double totaltax = setTotalTaxToOrder(taxResponse);
      abstractOrderModel.setTotalAvalaraTaxCalculated(totaltax);
     final Double totalPrice = abstractOrderModel.getTotalPrice();
      abstractOrderModel.setTotalPrice(Double.valueOf(totalPrice + totaltax));
    }

  private Double setTotalTaxToOrder(final TaxResponse taxResponse) {
    return taxResponse.getTotalTax() > 0.0 ? taxResponse.getTotalTax() : 0.0;
  }

  }

