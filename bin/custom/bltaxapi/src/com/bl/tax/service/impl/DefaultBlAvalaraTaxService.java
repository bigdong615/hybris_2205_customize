package com.bl.tax.service.impl;

import com.bl.tax.TaxRequestData;
import com.bl.tax.TaxResponse;
import com.bl.tax.service.BlTaxService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;

/**
 * This class created to prepare request and response for avalara
 * @author Manikandan
 */
public class DefaultBlAvalaraTaxService extends DefaultBlTaxService<AbstractOrderModel, ExternalTaxDocument, TaxRequestData, TaxResponse>
    implements BlTaxService<AbstractOrderModel, ExternalTaxDocument> {

  /**
   * this method created for processing request and response
   */
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
        getBlAvalaraTaxPopulator().populate(lResponse , orderModel);
      }
      return lExternalTaxDoc;
  }

}
