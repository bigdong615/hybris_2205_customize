package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

public class BlExtendOrderPopulator <SOURCE extends AbstractOrderModel, TARGET extends AbstractOrderData> implements
    Populator<SOURCE, TARGET> {


  @Override
  public void populate(AbstractOrderModel source, AbstractOrderData target) throws ConversionException {

    //
  }
}
