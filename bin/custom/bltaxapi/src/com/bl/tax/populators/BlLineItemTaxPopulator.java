package com.bl.tax.populators;

import com.bl.tax.TaxResponse;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;

public class BlLineItemTaxPopulator implements Populator<TaxResponse, AbstractOrderModel> {

  @Override
  public void populate(final TaxResponse taxResponse, final AbstractOrderModel abstractOrderModel) {
      for(int i = 0 ; i < taxResponse.getTaxLines().size() ; i ++) {
        final String responseProductCode = taxResponse.getTaxLines().get(i).getItemCode();
        for(int  j=0; j <abstractOrderModel.getEntries().size() ; j++ ) {
         final String productCode =  abstractOrderModel.getEntries().get(j).getProduct().getCode();
         if(productCode.equalsIgnoreCase(responseProductCode)) {
           abstractOrderModel.getEntries().get(j).setAvalaraLineTax(taxResponse.getTaxLines().get(i).getTax());
         }
        }
      }
    }

  }

