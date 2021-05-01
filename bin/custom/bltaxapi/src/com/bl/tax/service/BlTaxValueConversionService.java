package com.bl.tax.service;

import com.bl.tax.TaxLineResponse;
import de.hybris.platform.util.TaxValue;
import java.util.List;

public interface BlTaxValueConversionService {

  List<TaxValue> getLineTaxValues(final TaxLineResponse taxLine, final String currencyCode);

  List<TaxValue> getShippingTaxes(final List<TaxLineResponse> taxLines, final String currencyCode,
      final boolean shippingIncluded);
}
