package com.bl.tax.stratergy;

import com.bl.logging.BlLogger;
import com.bl.tax.service.BlTaxService;
import de.hybris.platform.commerceservices.externaltax.CalculateExternalTaxesStrategy;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.Config;
import de.hybris.platform.util.TaxValue;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class BlCalculateExternalTaxesStratergy implements CalculateExternalTaxesStrategy {

  private static final Logger LOG = Logger.getLogger(BlCalculateExternalTaxesStratergy.class);
  private BlTaxService<AbstractOrderModel, ExternalTaxDocument> defaultBlAvalaraTaxService;
  private ModelService modelService;

  @Override
  public ExternalTaxDocument calculateExternalTaxes(final AbstractOrderModel abstractOrder)
  {
    BlLogger.logMessage(LOG, Level.INFO,"==== AbstracrOrder:" + abstractOrder.getCode() +
        " isAvalaraTaxCalculated:-"+abstractOrder.getAvalaraTaxCalculated());
    ExternalTaxDocument taxResponse = null;
    try
    {
      // Do TAX Calculation for allowed countries only
      // get allowed Iso-codes from properties file
      final Set<String> resolveCountry =
          org.springframework.util.StringUtils.commaDelimitedListToSet(Config.getParameter("tax.calcualtion.country.isocode"));
      if (resolveCountry.contains(abstractOrder.getDeliveryAddress().getCountry().getIsocode()))
      {
        taxResponse = getDefaultBlAvalaraTaxService().process(abstractOrder);
        abstractOrder.setAvalaraTaxCalculated(Boolean.TRUE);
        getModelService().save(abstractOrder);
      }
    }
    catch (final Exception e)
    {
      BlLogger.logMessage(LOG,Level.INFO,"BlCalculateExternalTaxesStratergy : calculateExternalTaxes : ERROR : Failed to calcualte Tax " ,abstractOrder.getCode());
      BlLogger.logMessage(LOG,Level.ERROR,"BlCalculateExternalTaxesStratergy : : calculateExternalTaxes : ERROR : " ,e);
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
}
