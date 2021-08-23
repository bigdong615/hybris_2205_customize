package com.bl.core.services.tax;

import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.externaltax.RecalculateExternalTaxesStrategy;
import de.hybris.platform.commerceservices.externaltax.impl.DefaultExternalTaxesService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

/**
 * @author Manikandan
 * This class created for process of avalara tax and external tax
 */
public class DefaultBlExternalTaxesService extends DefaultExternalTaxesService {

  private static final Logger LOG = Logger.getLogger(DefaultBlExternalTaxesService.class);

  /**
   * This method created to process the avalara tax
   */
  @Override
  public boolean calculateExternalTaxes(final AbstractOrderModel abstractOrder)
  {
    BlLogger.logMessage(LOG, Level.INFO,"====== DefaultBlExternalTaxesService ======");

    if (getDecideExternalTaxesStrategy().shouldCalculateExternalTaxes(abstractOrder))
    {
      if (CollectionUtils.isNotEmpty(abstractOrder.getEntries()))
      {
        final ExternalTaxDocument exTaxDocument = getCalculateExternalTaxesStrategy().calculateExternalTaxes(abstractOrder);
        Assert.notNull(exTaxDocument, "ExternalTaxDocument should not be null");
        // check if external tax calculation was successful
        if (!exTaxDocument.getAllTaxes().isEmpty() && !exTaxDocument.getShippingCostTaxes().isEmpty())
        {
          abstractOrder.setNet(true);
          getApplyExternalTaxesStrategy().applyExternalTaxes(abstractOrder, exTaxDocument);
          getSessionService().setAttribute(SESSION_EXTERNAL_TAX_DOCUMENT, exTaxDocument);
          saveOrder(abstractOrder);
          return true;
        } else if(!exTaxDocument.getAllTaxes().isEmpty() && BooleanUtils.isTrue(abstractOrder.isUnPaidBillPresent())) {
          getApplyExternalTaxesStrategy().applyExternalTaxes(abstractOrder, exTaxDocument);
          return true;
        }
        else
        {
          getSessionService().removeAttribute(RecalculateExternalTaxesStrategy.SESSION_ATTIR_ORDER_RECALCULATION_HASH);
          clearSessionTaxDocument();
          clearTaxValues(abstractOrder);
          saveOrder(abstractOrder);
        }
      }
      abstractOrder.setTotalTax(0.0);
      abstractOrder.setAvalaraTaxCalculated(false);
    }
    abstractOrder.setAvalaraTaxCalculated(false);
    abstractOrder.setTotalTax(0.0);
    return false;
  }

}
