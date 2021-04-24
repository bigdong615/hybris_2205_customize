package com.bl.tax.populators;

import com.bl.tax.data.TaxResponseData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

public class BlTaxServiceResponsePopulator implements Populator<TaxResponseData, ExternalTaxDocument> {


  @Override
  public void populate(final TaxResponseData source, final ExternalTaxDocument target) throws ConversionException
  {
    //
  }

}
