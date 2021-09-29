package com.bl.tax.stratergy;

import com.bl.logging.BlLogger;
import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.service.BlTaxService;
import de.hybris.platform.commerceservices.externaltax.CalculateExternalTaxesStrategy;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.util.Config;
import de.hybris.platform.util.TaxValue;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created for process the avalara tax
 * @author Manikandan
 */
public class BlCalculateExternalTaxesStratergy implements CalculateExternalTaxesStrategy {

  private static final Logger LOG = Logger.getLogger(BlCalculateExternalTaxesStratergy.class);
  private BlTaxService<AbstractOrderModel, ExternalTaxDocument> defaultBlAvalaraTaxService;
  private ModelService modelService;
  private SessionService sessionService;

  /**
   * this method created to process the avalara request
   * @param abstractOrder order to calculcate the taxes for
   * @return ExternalTaxDocument
   */
  @Override
  public ExternalTaxDocument calculateExternalTaxes(final AbstractOrderModel abstractOrder)
  {
    BlLogger.logMessage(LOG, Level.INFO,"==== AbstracrOrder:" + abstractOrder.getCode() +
        " isAvalaraTaxCalculated:-"+abstractOrder.getAvalaraTaxCalculated());
    ExternalTaxDocument taxResponse = null;
    try
    {
      final String resolveCountry = Config.getParameter(BltaxapiConstants.ISO_CODE);
      if (abstractOrder.getDeliveryAddress().getCountry().getIsocode().equalsIgnoreCase(resolveCountry))
      {
        taxResponse = getDefaultBlAvalaraTaxService().process(abstractOrder);
        if(BooleanUtils.isFalse(abstractOrder.isUnPaidBillPresent())) {
          abstractOrder.setAvalaraTaxCalculated(Boolean.TRUE);
          getModelService().save(abstractOrder);
          getModelService().refresh(abstractOrder);
        }
      }
    }
    catch (final Exception e)
    {
      BlLogger.logMessage(LOG,Level.INFO,"BlCalculateExternalTaxesStratergy : calculateExternalTaxes : ERROR : Failed to calcualte Tax " ,abstractOrder.getCode());
      BlLogger.logMessage(LOG,Level.ERROR ,"BlCalculateExternalTaxesStratergy : : calculateExternalTaxes : ERROR : " ,e);
      getSessionService().setAttribute(BltaxapiConstants.IS_AVALARA_EXCEPTION , true);
      abstractOrder.setAvalaraTaxCalculated(Boolean.FALSE);
      abstractOrder.setTotalTax(0.0);
      getModelService().save(abstractOrder);
      getModelService().refresh(abstractOrder);
    }
    finally
    {
      if (taxResponse == null)
      {
        BlLogger.logMessage(LOG,Level.INFO,"BlCalculateExternalTaxesStratergy : calculateExternalTaxes() : Retruning Default tax Document");
        taxResponse = createDefaultExternalTaxDocument(abstractOrder);
        abstractOrder.setAvalaraTaxCalculated(Boolean.FALSE);
        getModelService().save(abstractOrder);
      }
    }
    BlLogger.logMessage(LOG,Level.DEBUG,"BlCalculateExternalTaxesStratergy : : calculateExternalTaxes() : Recieved Tax:{} for Order:{} and is tax exempted:{}"
       , abstractOrder.getTotalTax() +"code="+ abstractOrder.getCode()  /*null != abstractOrder.getTaxCertificatefile()*/);
    return taxResponse;
  }

  /**
   * To Created External tax if response is null
   */

  private ExternalTaxDocument createDefaultExternalTaxDocument(final AbstractOrderModel abstractOrder)
  {
    final ExternalTaxDocument lExternalTaxDoc = new ExternalTaxDocument();

    for (final AbstractOrderEntryModel entry : abstractOrder.getEntries())
    {
      lExternalTaxDoc.setTaxesForOrderEntry(entry.getEntryNumber().intValue(), getZeroTaxValue(abstractOrder.getCurrency().getIsocode()));
    }
    lExternalTaxDoc.setShippingCostTaxes(getZeroTaxValue(abstractOrder.getCurrency().getIsocode()));
    return lExternalTaxDoc;
  }


  private List<TaxValue> getZeroTaxValue(final String isocode)
  {
    final List<TaxValue> taxValues = new ArrayList<>();
    taxValues.add(new TaxValue("DEFAULT_TAXCODE", 0.0, true, 0.0, isocode));
    return taxValues;
  }

  public BlTaxService<AbstractOrderModel, ExternalTaxDocument> getDefaultBlAvalaraTaxService() {
    return defaultBlAvalaraTaxService;
  }

  public void setDefaultBlAvalaraTaxService(
      BlTaxService<AbstractOrderModel, ExternalTaxDocument> defaultBlAvalaraTaxService) {
    this.defaultBlAvalaraTaxService = defaultBlAvalaraTaxService;
  }


  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }


  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(SessionService sessionService) {
    this.sessionService = sessionService;
  }
}
