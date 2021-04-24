package com.bl.core.services.tax;

import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.externaltax.RecalculateExternalTaxesStrategy;
import de.hybris.platform.commerceservices.externaltax.impl.DefaultExternalTaxesService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

public class DefaultBlExternalTaxesService extends DefaultExternalTaxesService {

  private static final Logger LOG = Logger.getLogger(DefaultBlExternalTaxesService.class);

  @Override
  public boolean calculateExternalTaxes(final AbstractOrderModel abstractOrder)
  {
    BlLogger.logMessage(LOG, Level.INFO,"===DefaultBlExternalTaxesService==");

    if (getDecideExternalTaxesStrategy().shouldCalculateExternalTaxes(abstractOrder))
    {
      getModelService().save(abstractOrder);
      getModelService().refresh(abstractOrder);
      if (CollectionUtils.isNotEmpty(abstractOrder.getEntries()))
      {
        final ExternalTaxDocument exTaxDocument = getCalculateExternalTaxesStrategy().calculateExternalTaxes(abstractOrder);
        Assert.notNull(exTaxDocument, "ExternalTaxDocument should not be null");
        // check if external tax calculation was successful
        if (!exTaxDocument.getAllTaxes().isEmpty() && !exTaxDocument.getShippingCostTaxes().isEmpty())
        {
          getApplyExternalTaxesStrategy().applyExternalTaxes(abstractOrder, exTaxDocument);
          getSessionService().setAttribute(SESSION_EXTERNAL_TAX_DOCUMENT, exTaxDocument);
          saveOrder(abstractOrder);
          return true;
        }
        else
        {
          // the external tax calculation failed
          getSessionService().removeAttribute(RecalculateExternalTaxesStrategy.SESSION_ATTIR_ORDER_RECALCULATION_HASH);
          clearSessionTaxDocument();
          clearTaxValues(abstractOrder);
          saveOrder(abstractOrder);
        }
      }
    }
    return false;
  }

}
