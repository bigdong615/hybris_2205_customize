package com.bl.core.services.pricingratio;

import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import java.util.List;

/**
 * This interface is added to get standard and constrained price ratio
 * for a fixed duration
 *
 * @author Ritika
 */
public interface BlPricingRatioService{

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

  /**
   * Gets standard pricing ratio by duration.
   *
   * @param duration the duration
   * @return the standard pricing ratio by duration
   */
  BlStandardPricingRatioModel getStandardPricingRatioByDuration(final DurationEnum duration);


  /**
   * Gets constrained pricing ratio.
   *
   * @param duration the duration
   * @return the constrained pricing ratio
   */
  BlConstrainedPricingRatioModel getConstrainedPricingRatioByDuration(final DurationEnum duration);
}
