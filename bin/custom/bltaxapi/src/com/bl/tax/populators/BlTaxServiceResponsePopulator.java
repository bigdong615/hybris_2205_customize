package com.bl.tax.populators;

import com.bl.tax.TaxLineResponse;
import com.bl.tax.TaxResponse;
import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.service.impl.DefaultBlTaxValueConversionService;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.util.TaxValue;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;

public class BlTaxServiceResponsePopulator implements Populator<TaxResponse, ExternalTaxDocument> {

  private DefaultBlTaxValueConversionService defaultBlTaxValueConversionService;

  @Override
  public void populate(final TaxResponse source, final ExternalTaxDocument target)
      throws ConversionException {
    if (null != source && CollectionUtils.isNotEmpty(source.getTaxLines()))
    {
      // US and CA country
      final String currencyCode = BltaxapiConstants.CA.equals(source.getCountry()) ? BltaxapiConstants.CAD : BltaxapiConstants.USD;
      for (final TaxLineResponse taxLineResp : source.getTaxLines())
      {
        if (shouldConvertLine(source.getTaxLines(), Integer.parseInt(taxLineResp.getLineNumber()), true))
        {
          final int entryNumber = Integer.parseInt(taxLineResp.getLineNumber());
          target.setTaxesForOrderEntry(entryNumber,
              getLineTaxValues(taxLineResp, currencyCode));
        }
      }
      // Set default tax 0 for shipping cost
      target.setShippingCostTaxes(
          getDefaultBlTaxValueConversionService().getShippingTaxes(source.getTaxLines(), currencyCode, true));
    }
    else
    {
      throw new RuntimeException("BlTaxServiceResponsePopulator: Error in TaxResponse source.getTaxLines()");
    }
  }

  private boolean shouldConvertLine(final List<TaxLineResponse> taxLines, final int index, final boolean shippingIncluded)
  {
    if (!shippingIncluded)
    {
      return true;
    }

    return index < (taxLines.size() - 1);
  }

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


