package com.bl.core.dao;

import com.bl.core.model.BlStandardPricingRatioModel;
import java.util.List;

/**
 * BlStandardPricingRatioDao interface is used to get product price ratios
 *
 * @author Kalyan Kumar
 */
public interface BlStandardPricingRatioDao {

  /**
   * getStandardPricingRatio is used to get List of BlStandardPricingRatioModel
   *
   * @return List<BlStandardPricingRatioModel> which returns list of price ratios
   */
  List<BlStandardPricingRatioModel> getStandardPricingRatio();
}
