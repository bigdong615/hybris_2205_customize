package com.bl.tax.service.impl;

import com.bl.tax.ResponseData;
import com.bl.tax.TaxRequestData;
import com.bl.tax.TaxResponse;
import com.bl.tax.service.BlTaxService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import java.net.URISyntaxException;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.web.client.RestClientException;

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
  public ExternalTaxDocument process(final AbstractOrderModel orderModel)
      throws RestClientException, URISyntaxException {
    final TaxRequestData request = new TaxRequestData();
    getRequestPopulator().populate(orderModel, request);
    final ResponseData responseData;
    responseData = super.process(createHttpEntity(request), TaxResponse.class);
      final ExternalTaxDocument lExternalTaxDoc = new ExternalTaxDocument();
      if(null != responseData.getResults() && "201".equalsIgnoreCase(responseData.getStatusCode())) {
        if(BooleanUtils.isTrue(orderModel.isUnPaidBillPresent())) {
          getBlPayBillTaxResponsePopulator().populate(responseData.getResults(), lExternalTaxDoc);
        } else {
          getResponsePopulator().populate(responseData.getResults(), lExternalTaxDoc);
        }
        getBlAvalaraTaxPopulator().populate(responseData.getResults(), orderModel);
      }
      return lExternalTaxDoc;
  }

}
