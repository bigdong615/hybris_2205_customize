package com.bl.core.dao.impl;

import com.bl.core.dao.BlStandardPricingRatioDao;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.util.Collections;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * DefaultBlStandardPricingRatioDao class is used to get product price ratios
 *
 * @author Kalyan Kumar
 */

public class DefaultBlStandardPricingRatioDao implements BlStandardPricingRatioDao {


  private FlexibleSearchService flexibleSearchService;
  private static final Logger LOG = Logger.getLogger(DefaultBlStandardPricingRatioDao.class);


  /**
   *  {@inheritDoc}  
   */
  @Override
  public List<BlStandardPricingRatioModel> getStandardPricingRatio()
  {
    final String query = new StringBuilder("SELECT {PK} FROM {")
        .append(BlStandardPricingRatioModel._TYPECODE).append("}").toString();
    final FlexibleSearchQuery flexibleSearchQuery = new FlexibleSearchQuery(query);
    final SearchResult<BlStandardPricingRatioModel> result = getFlexibleSearchService().search(flexibleSearchQuery);
    if(CollectionUtils.isEmpty(result.getResult())) {
      BlLogger.logMessage(LOG, Level.DEBUG, "! No results found for BL standard pricing ratios");
      return Collections.emptyList();
    }
    return result.getResult();
  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }
}
