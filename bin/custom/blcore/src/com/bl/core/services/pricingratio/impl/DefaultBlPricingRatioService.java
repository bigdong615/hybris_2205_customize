package com.bl.core.services.pricingratio.impl;

import com.bl.core.dao.pricingratio.BlPricingRatioDao;
import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.core.services.pricingratio.BlPricingRatioService;

/**
 * This class is for updating the seven day and other prices on change of retail price
 *
 * @author Ritika
 */
public class DefaultBlPricingRatioService  implements BlPricingRatioService {

  private BlPricingRatioDao blPricingRatioDao;

  @Override
  public BlStandardPricingRatioModel getStandardPricingRatioByDuration(DurationEnum duration) {

    return getBlPricingRatioDao().getStandardPricingRatioByDuration(duration);
  }

  @Override
  public BlConstrainedPricingRatioModel getConstrainedPricingRatioByDuration(DurationEnum duration) {

    return getBlPricingRatioDao().getConstrainedPricingRatioByDuration(duration);
  }

  public BlPricingRatioDao getBlPricingRatioDao() {
    return blPricingRatioDao;
  }

  public void setBlPricingRatioDao(BlPricingRatioDao blPricingRatioDao) {
    this.blPricingRatioDao = blPricingRatioDao;
  }
}
