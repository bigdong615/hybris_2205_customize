package com.bl.core.dao.pricingratio;

import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import java.util.List;
/**
 * This interface is
 * created for retrieving the standard
 * and constrained ratios
 *
 * @author Ritika
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

  /**
   * Get Standard Pricing Ratio by specific duration
   * @param duration
   * @return
   */

  BlStandardPricingRatioModel getStandardPricingRatioByDuration(final DurationEnum duration);


  /**
   * get Constrained Pricing Ratio by Duration
   * @param duration
   * @return
   */
  BlConstrainedPricingRatioModel getConstrainedPricingRatioByDuration(final DurationEnum duration);



  }
