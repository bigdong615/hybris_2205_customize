package com.bl.tax.service.impl;

import com.bl.tax.data.TaxRequestData;
import com.bl.tax.data.TaxResponseData;
import com.bl.tax.service.BlTaxService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;

public class DefaultBlAvalaraTaxService extends DefaultBlTaxService<AbstractOrderModel, ExternalTaxDocument, TaxRequestData, TaxResponseData>
    implements BlTaxService<AbstractOrderModel, ExternalTaxDocument> {

  @Override
  public ExternalTaxDocument process(final AbstractOrderModel orderModel) throws Exception
  {
    final TaxRequestData request = new TaxRequestData();
    getRequestPopulator().populate(orderModel, request);
    final TaxResponseData lResponse ;
    boolean allowed = false;
    if(allowed) {
      lResponse = super.process(createHttpEntity(request), TaxResponseData.class);
      final ExternalTaxDocument lExternalTaxDoc = new ExternalTaxDocument();
      getResponsePopulator().populate(lResponse, lExternalTaxDoc);
      return lExternalTaxDoc;
    }
    return null;
  }

}
