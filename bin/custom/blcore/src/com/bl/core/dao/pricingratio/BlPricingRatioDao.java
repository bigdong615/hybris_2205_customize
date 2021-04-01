package com.bl.core.dao.pricingratio;

import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import java.util.List;

/**
 * The interface Bl standard pricing ratio dao.
 */
public interface BlPricingRatioDao {

  /**
   * Gets standard pricing ratio.
   *
   * @return the standard pricing ratio
   */
  List<BlStandardPricingRatioModel> getStandardPricingRatio();
  /**
   * Gets constrained pricing ratio.
   *
   * @return the constrained pricing ratio
   */
  List<BlConstrainedPricingRatioModel> getConstrainedPricingRatio();

  BlStandardPricingRatioModel getStandardPricingRatioByDuration(final DurationEnum duration);


  BlConstrainedPricingRatioModel getConstrainedPricingRatioByDuration(final DurationEnum duration);



  }
