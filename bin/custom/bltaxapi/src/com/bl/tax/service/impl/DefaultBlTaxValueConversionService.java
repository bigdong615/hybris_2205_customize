package com.bl.tax.service.impl;

import com.bl.tax.TaxLineResponse;
import com.bl.tax.service.BlTaxValueConversionService;
import de.hybris.platform.util.TaxValue;
import java.util.ArrayList;
import java.util.List;

/**
 * This class created for Tax conversion
 * @author Manikandan
 */
public class DefaultBlTaxValueConversionService implements BlTaxValueConversionService {

  /**
   * this method created to get line item tax
   */
  @Override
  public List<TaxValue> getLineTaxValues(final TaxLineResponse taxLine, final String currencyCode) {
    final List<TaxValue> taxValues = new ArrayList<>();
    taxValues.add(new TaxValue(taxLine.getTaxCode(), taxLine.getTax(), true, taxLine.getTax(), currencyCode));
    return taxValues;
  }

  /**
   * this method created to get shipping tax
   */
  @Override
  public List<TaxValue> getShippingTaxes(final List<TaxLineResponse> taxLines,
     final String currencyCode, final  boolean shippingIncluded) {

    List<TaxValue> shippingTaxes = null;

    if (shippingIncluded)
    {
      shippingTaxes = getLineTaxValues(taxLines.get(taxLines.size() - 1), currencyCode);
    }
    return shippingTaxes;
  }
}
