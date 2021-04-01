package com.bl.core.dao.pricingratio.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.core.dao.pricingratio.BlPricingRatioDao;
import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.util.Collections;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;

/**
 * This class is
 * created for retrieving all the standard and constrained ratios
 * based on duration(s)
 *
 * @author Ritika
 */
public class DefaultBlPricingRatioDao implements BlPricingRatioDao
{

  public static final String SELECT = "SELECT {";
  public static final String FROM = "} FROM {";
  public static final String FIND_STANDARD_PRICING_RATIOS = SELECT + BlStandardPricingRatioModel.PK + FROM
      + BlStandardPricingRatioModel._TYPECODE + "}";
  public static final String FIND_CONSTRAINED_PRICING_RATIOS = SELECT + BlConstrainedPricingRatioModel.PK + FROM
      + BlConstrainedPricingRatioModel._TYPECODE + "}";
  public static final String FIND_STANDARD_PRICING_RATIO_BY_DURATION = SELECT + BlStandardPricingRatioModel.PK + FROM
      + BlStandardPricingRatioModel._TYPECODE + "} WHERE {" + BlStandardPricingRatioModel.DURATION+ "} =?duration";
  public static final String FIND_CONSTRAINED_PRICING_RATIO_BY_DURATION = SELECT + BlConstrainedPricingRatioModel.PK + FROM
      + BlConstrainedPricingRatioModel._TYPECODE + "} WHERE {" + BlConstrainedPricingRatioModel.DURATION+ "} =?duration";

  private FlexibleSearchService flexibleSearchService;

  @Override
  public List<BlStandardPricingRatioModel> getStandardPricingRatio()
  {
    FlexibleSearchQuery query = new FlexibleSearchQuery(FIND_STANDARD_PRICING_RATIOS);
    SearchResult<BlStandardPricingRatioModel> result = getFlexibleSearchService().search(query);
    return CollectionUtils.isNotEmpty(result.getResult()) ? result.getResult() : Collections.emptyList();
  }

  @Override
  public List<BlConstrainedPricingRatioModel> getConstrainedPricingRatio() {
    FlexibleSearchQuery query = new FlexibleSearchQuery(FIND_CONSTRAINED_PRICING_RATIOS);
    SearchResult<BlConstrainedPricingRatioModel> result = getFlexibleSearchService().search(query);
    return CollectionUtils.isNotEmpty(result.getResult()) ? result.getResult() : Collections.emptyList();
  }

  @Override
  public BlStandardPricingRatioModel getStandardPricingRatioByDuration(final DurationEnum duration)
  {
    validateParameterNotNull(duration, "Duration must not be null");

    FlexibleSearchQuery query = new FlexibleSearchQuery(FIND_STANDARD_PRICING_RATIO_BY_DURATION);
    query.addQueryParameter("duration",duration);
    SearchResult<BlStandardPricingRatioModel> result = getFlexibleSearchService().search(query);
    return result.getCount()> 0 && result.getResult() != null ? result.getResult().get(0) : null ;
  }

  @Override
  public BlConstrainedPricingRatioModel getConstrainedPricingRatioByDuration(final DurationEnum duration) {
    validateParameterNotNull(duration, "Duration must not be null");

    FlexibleSearchQuery query = new FlexibleSearchQuery(FIND_CONSTRAINED_PRICING_RATIO_BY_DURATION);
    query.addQueryParameter("duration",duration);
    SearchResult<BlConstrainedPricingRatioModel> result = getFlexibleSearchService().search(query);
    return result.getCount()> 0 && result.getResult() != null ? result.getResult().get(0) : null ;
  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }
}
