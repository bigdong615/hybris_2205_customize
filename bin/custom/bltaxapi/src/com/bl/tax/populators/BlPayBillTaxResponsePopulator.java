package com.bl.tax.populators;

import com.bl.logging.BlLogger;
import com.bl.tax.TaxLineResponse;
import com.bl.tax.TaxResponse;
import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.service.impl.DefaultBlTaxValueConversionService;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import de.hybris.platform.util.TaxValue;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This populator created to populate response from avalara
 * @author Moumita
 */
public class BlPayBillTaxResponsePopulator implements Populator<TaxResponse, ExternalTaxDocument> {

  private static final Logger LOG = Logger.getLogger(BlPayBillTaxResponsePopulator.class);

  private DefaultBlTaxValueConversionService defaultBlTaxValueConversionService;

  /**
   * this method created to populate response from  for pay bill charges
   */
  @Override
  public void populate(final TaxResponse source, final ExternalTaxDocument target) {
    if (null != source && CollectionUtils.isNotEmpty(source.getTaxLines()))
    {
      for (final TaxLineResponse taxLineResp : source.getTaxLines())
      {
          final int entryNumber = Integer.parseInt(taxLineResp.getLineNumber());
          target.setTaxesForOrderEntry(entryNumber,
              getLineTaxValues(taxLineResp, BltaxapiConstants.USD));
      }
    }
    else
    {
      BlLogger.logMessage(LOG , Level.ERROR , "BlPayBillTaxResponsePopulator: Error in TaxResponse source.getTaxLines()" , new RuntimeException());
    }
  }

  /**
   * this method created for get line item tax values
   */
  private List<TaxValue> getLineTaxValues(final TaxLineResponse taxLine, final String currencyCode)
  {
    return getDefaultBlTaxValueConversionService().getLineTaxValues(taxLine, currencyCode);
  }


  public DefaultBlTaxValueConversionService getDefaultBlTaxValueConversionService() {
    return defaultBlTaxValueConversionService;
  }

  public void setDefaultBlTaxValueConversionService(
      DefaultBlTaxValueConversionService defaultBlTaxValueConversionService) {
    this.defaultBlTaxValueConversionService = defaultBlTaxValueConversionService;
  }

}


