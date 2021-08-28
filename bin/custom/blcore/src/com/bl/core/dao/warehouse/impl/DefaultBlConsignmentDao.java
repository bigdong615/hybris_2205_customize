package com.bl.core.dao.warehouse.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is used to get consignments.
 *
 * @author Sunil
 */
public class DefaultBlConsignmentDao implements BlConsignmentDao {

  private static final Logger LOG = Logger.getLogger(DefaultBlConsignmentDao.class);
  private FlexibleSearchService flexibleSearchService;

  private static final String DATE_PARAM = "} BETWEEN ?startDate AND ?endDate ";
  private static final String FIND_READY_TO_SHIP_CONSIGNMENTS_FOR_DATE = "SELECT {pk} FROM {Consignment as con} WHERE {con:STATUS} NOT IN ({{SELECT {cs:PK} FROM {ConsignmentStatus as cs} WHERE {cs:CODE} = ?status1 OR {cs:CODE} = ?status2}})" +
      " AND {con:optimizedShippingStartDate"+ DATE_PARAM;

  /**
   * Get consignments
   *
   * @return ConsignmentModels
   */
  @Override
  public List<ConsignmentModel> getReadyToShipConsignmentsForDate(final Date shipDate) {

    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(FIND_READY_TO_SHIP_CONSIGNMENTS_FOR_DATE);

    final List<ConsignmentStatus> statusList = new ArrayList<>();
    statusList.add(ConsignmentStatus.CANCELLED);
    statusList.add(ConsignmentStatus.SHIPPED);

    addQueryParameter(shipDate, statusList, fQuery);

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Flexible query for getting consignments to ship for date {} is  - {}", shipDate, fQuery.toString());

    final SearchResult<ConsignmentModel> result = getFlexibleSearchService().search(fQuery);
    final List<ConsignmentModel> consignmentModels = result.getResult();


    if (CollectionUtils.isEmpty(consignmentModels))
    {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "No Consignments available to ship for date {}", shipDate);
      return Collections.emptyList();
    }

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "No of consignments available = {} to ship for date {}", consignmentModels.size(), shipDate);

    return consignmentModels;
  }

  /**
   * It adds the parameters value into query
   * @param shipDate
   * @param statusList
   * @param fQuery
   */
  private void addQueryParameter(final Date shipDate, final List<ConsignmentStatus> statusList,
      final FlexibleSearchQuery fQuery) {

    SimpleDateFormat simpleformat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    final Calendar startDate = BlDateTimeUtils.getFormattedStartDay(shipDate);
    fQuery.addQueryParameter(BlCoreConstants.START_DATE, simpleformat.format(startDate.getTime()));

    final Calendar endDate = BlDateTimeUtils.getFormattedEndDay(shipDate);
    fQuery.addQueryParameter(BlCoreConstants.END_DATE, simpleformat.format(endDate.getTime()));

    fQuery.addQueryParameter(BlCoreConstants.STATUS1, statusList.get(0).getCode());
    fQuery.addQueryParameter(BlCoreConstants.STATUS2, statusList.get(1).getCode());

  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(
      final FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }

}
