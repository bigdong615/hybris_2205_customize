package com.bl.tax.service.impl;

import com.bl.tax.TaxLineResponse;
import com.bl.tax.service.BlTaxValueConversionService;
import de.hybris.platform.util.TaxValue;
import java.util.ArrayList;
import java.util.List;

public class DefaultBlTaxValueConversionService implements BlTaxValueConversionService {

  @Override
  public List<TaxValue> getLineTaxValues(TaxLineResponse taxLine, String currencyCode) {
    final List<TaxValue> taxValues = new ArrayList<>();
    taxValues.add(new TaxValue(taxLine.getTaxCode(), taxLine.getTax(), true, taxLine.getTax(), currencyCode));
    return taxValues;
  }

  @Override
  public List<TaxValue> getShippingTaxes(List<TaxLineResponse> taxLines,
      String currencyCode, boolean shippingIncluded) {
    List<TaxValue> shippingTaxes = null;
    if (shippingIncluded)
    {
      shippingTaxes = getLineTaxValues(taxLines.get(taxLines.size() - 1), currencyCode);
    }
    return shippingTaxes;
  }
}
