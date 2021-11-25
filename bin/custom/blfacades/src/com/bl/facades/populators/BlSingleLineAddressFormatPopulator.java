package com.bl.facades.populators;

import de.hybris.platform.commercefacades.address.converters.populator.SingleLineAddressFormatPopulator;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.util.List;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.log4j.Logger;
import org.apache.commons.lang.StringUtils;

public class BlSingleLineAddressFormatPopulator extends SingleLineAddressFormatPopulator {

  private static final Logger LOG = Logger.getLogger(BlSingleLineAddressFormatPopulator.class);

  private List<String> addressFormatList;

  /**
   * This method to check the field values are not empty
   * @param addressModel
   * @param addressLine
   */
  @Override
  public void populate(final AddressModel addressModel, final StringBuilder addressLine) {

    for (final String field : addressFormatList)
    {
      try
      {
        final String fieldValue = (String) PropertyUtils.getProperty(addressModel, field);
        if (StringUtils.isNotBlank(fieldValue))
        {
          addressLine.append(fieldValue);
          addressLine.append(", ");
        }
      }
      catch (final NestedNullException e)
      {
        if (LOG.isDebugEnabled())
        {
          LOG.debug(e.getLocalizedMessage(), e);
        }
      }
      catch (final Exception e)
      {
        throw new ConversionException(e.getLocalizedMessage(), e);
      }
    }

    if (addressLine.length() > 2)
    {
      // Trim last ", "
      addressLine.setLength(addressLine.length() - 2);
    }
  }

  @Override
  public List<String> getAddressFormatList() {
    return addressFormatList;
  }

  @Override
  public void setAddressFormatList(List<String> addressFormatList) {
    this.addressFormatList = addressFormatList;
  }

}
