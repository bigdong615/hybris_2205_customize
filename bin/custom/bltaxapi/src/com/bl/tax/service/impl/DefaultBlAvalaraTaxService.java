package com.bl.tax.service.impl;

import com.bl.tax.TaxRequestData;
import com.bl.tax.TaxResponse;
import com.bl.tax.service.BlTaxService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;

public class DefaultBlAvalaraTaxService extends DefaultBlTaxService<AbstractOrderModel, ExternalTaxDocument, TaxRequestData, TaxResponse>
    implements BlTaxService<AbstractOrderModel, ExternalTaxDocument> {

  @Override
  public ExternalTaxDocument process(final AbstractOrderModel orderModel) throws Exception
  {
    final TaxRequestData request = new TaxRequestData();
    getRequestPopulator().populate(orderModel, request);
    final TaxResponse lResponse ;
      lResponse = super.process(createHttpEntity(request), TaxResponse.class);
      final ExternalTaxDocument lExternalTaxDoc = new ExternalTaxDocument();
      if(null != lResponse) {
        getResponsePopulator().populate(lResponse, lExternalTaxDoc);
        orderModel.setTotalAvalaraTaxCalculated(setTotalTaxToOrder(lResponse));
          getBlLineItemTaxPopulator().populate(lResponse , orderModel);
      }
      return lExternalTaxDoc;
  }

  private Double setTotalTaxToOrder(final TaxResponse lResponse) {
   return lResponse.getTotalTax() > 0.0 ? lResponse.getTotalTax() : 0.0;
  }
}
