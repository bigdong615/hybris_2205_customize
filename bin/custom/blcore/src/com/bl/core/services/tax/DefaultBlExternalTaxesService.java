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
        BlLogger.logMessage(LOG,Level.INFO,"DefaultBlExternalTaxesService : calculateExternalTaxes : After exTaxDocument " + abstractOrder.getCode() + " exTaxDocument:" + exTaxDocument);
        // check if external tax calculation was successful
        if (!exTaxDocument.getAllTaxes().isEmpty() && !exTaxDocument.getShippingCostTaxes().isEmpty())
        {
          BlLogger.logMessage(LOG,Level.INFO,"DefaultBlExternalTaxesService : calculateExternalTaxes : Shipping taxes present " + abstractOrder.getCode() + " exTaxDocument:" + exTaxDocument);
          abstractOrder.setNet(true);
          getApplyExternalTaxesStrategy().applyExternalTaxes(abstractOrder, exTaxDocument);
          getSessionService().setAttribute(SESSION_EXTERNAL_TAX_DOCUMENT, exTaxDocument);
          saveOrder(abstractOrder);
          return true;
        } else if(!exTaxDocument.getAllTaxes().isEmpty() && BooleanUtils.isTrue(abstractOrder.isUnPaidBillPresent())) {
          BlLogger.logMessage(LOG,Level.INFO,"DefaultBlExternalTaxesService : calculateExternalTaxes : Unpaid bill present " + abstractOrder.getCode() + " exTaxDocument:" + exTaxDocument);
          getApplyExternalTaxesStrategy().applyExternalTaxes(abstractOrder, exTaxDocument);
          return true;
        }
        else
        {
          BlLogger.logMessage(LOG,Level.INFO,"DefaultBlExternalTaxesService : calculateExternalTaxes : All taxes empty-1 " + abstractOrder.getCode() + " exTaxDocument:" + exTaxDocument);
          getSessionService().removeAttribute(RecalculateExternalTaxesStrategy.SESSION_ATTIR_ORDER_RECALCULATION_HASH);
          clearSessionTaxDocument();
          clearTaxValues(abstractOrder);
          saveOrder(abstractOrder);
        }
      }
      BlLogger.logMessage(LOG,Level.INFO,"BlCalculateExternalTaxesStratergy : calculateExternalTaxes : ERROR : All taxes empty-2 " ,abstractOrder.getCode() + " order entries not empty?: " + CollectionUtils.isNotEmpty(abstractOrder.getEntries()));
      abstractOrder.setTotalTax(0.0);
      abstractOrder.setAvalaraTaxCalculated(false);
    }
    BlLogger.logMessage(LOG,Level.INFO,"BlCalculateExternalTaxesStratergy : calculateExternalTaxes : ERROR : All taxes empty-3 " ,abstractOrder.getCode() + " shouldCalculateExternalTaxes: " + getDecideExternalTaxesStrategy().shouldCalculateExternalTaxes);
    abstractOrder.setAvalaraTaxCalculated(false);
    abstractOrder.setTotalTax(0.0);
    return false;
  }

}
