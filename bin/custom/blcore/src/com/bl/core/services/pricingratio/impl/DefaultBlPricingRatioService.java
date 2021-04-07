package com.bl.core.services.pricingratio.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.core.services.pricingratio.BlPricingRatioService;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.internal.dao.GenericDao;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections.CollectionUtils;

/**
 * This class is for updating the
 * seven day and other prices on change of retail price
 *
 * @author Ritika
 */
public class DefaultBlPricingRatioService  implements BlPricingRatioService {

  private GenericDao<BlConstrainedPricingRatioModel> blConstrainedRatioGenericDao;
  private GenericDao<BlStandardPricingRatioModel> blStandardRatioGenericDao;


  @Override
  public List<BlStandardPricingRatioModel> getStandardPricingRatio() {

    final List<BlStandardPricingRatioModel> resultSet = getBlStandardRatioGenericDao().find();
    return CollectionUtils.isNotEmpty(resultSet) ? resultSet : Collections.emptyList();
  }

  @Override
  public List<BlConstrainedPricingRatioModel> getConstrainedPricingRatio() {

    final List<BlConstrainedPricingRatioModel> resultSet = getBlConstrainedRatioGenericDao().find();
    return CollectionUtils.isNotEmpty(resultSet) ? resultSet : Collections.emptyList();
  }

  @Override
  public BlStandardPricingRatioModel getStandardPricingRatioByDuration(final DurationEnum duration) {

    validateParameterNotNullStandardMessage("duration", duration);
    final Map<String, Object> queryParams = Collections.singletonMap(BlStandardPricingRatioModel.DURATION, duration);
    return getBlStandardRatioGenericDao().find(queryParams).stream().findFirst()
        .orElseThrow(() -> new ModelNotFoundException(String.format("Ratio not found for code [%s]", duration)));
  }

  @Override
  public BlConstrainedPricingRatioModel getConstrainedPricingRatioByDuration(final DurationEnum duration) {

    validateParameterNotNullStandardMessage("duration", duration);
    final Map<String, Object> queryParams = Collections.singletonMap(BlConstrainedPricingRatioModel.DURATION, duration);
    return getBlConstrainedRatioGenericDao().find(queryParams).stream().findFirst()
        .orElseThrow(() -> new ModelNotFoundException(String.format("Ratio not found for code [%s]", duration)));
  }



  public GenericDao<BlConstrainedPricingRatioModel> getBlConstrainedRatioGenericDao() {
    return blConstrainedRatioGenericDao;
  }

  public void setBlConstrainedRatioGenericDao(
      GenericDao<BlConstrainedPricingRatioModel> blConstrainedRatioGenericDao) {
    this.blConstrainedRatioGenericDao = blConstrainedRatioGenericDao;
  }

  public GenericDao<BlStandardPricingRatioModel> getBlStandardRatioGenericDao() {
    return blStandardRatioGenericDao;
  }

  public void setBlStandardRatioGenericDao(
      GenericDao<BlStandardPricingRatioModel> blStandardRatioGenericDao) {
    this.blStandardRatioGenericDao = blStandardRatioGenericDao;
  }
}
