package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;

public class BlProductTagPopulator implements Populator<BlProductModel, ProductData> {

  @Override
  public void populate(final BlProductModel source, final ProductData target) {
    setProductTagValues(target,
        BooleanUtils.isTrue(source.getIsNew()) ? BlCoreConstants.NEW : StringUtils.EMPTY);
    setProductTagValues(target,
        BooleanUtils.isTrue(source.getMostPopular()) ? BlCoreConstants.POPULAR : StringUtils.EMPTY);
    if (BooleanUtils.isTrue(source.getForRent())) {
      setProductTagValues(target,
          BooleanUtils.isTrue(source.getGreatValue()) ? BlCoreConstants.GREAT_VALUE_STRING
              : StringUtils.EMPTY);
      setProductTagValues(target,
          BooleanUtils.isTrue(source.getStaffPick()) ? BlCoreConstants.STAFF_PICK_STRING
              : StringUtils.EMPTY);
    }
  }

  private void setProductTagValues(final ProductData target, final String value) {
    if (StringUtils.isBlank(target.getProductTagValues())) {
      target.setProductTagValues(value);
    }
  }
}