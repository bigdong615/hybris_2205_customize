package com.bl.core.dao.warehouse.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.util.Config;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
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

  /**
   * Get consignments
   *
   * @return ConsignmentModels
   */
  @Override
  public List<ConsignmentModel> getReadyToShipConsignmentsForDate(final Date shipDate) {

    final StringBuilder readyToShipConsignmentsQuery = new StringBuilder();
    readyToShipConsignmentsQuery.append("SELECT {pk} FROM {Consignment as con} WHERE {con:STATUS} NOT IN ({{SELECT {cs:PK} FROM {ConsignmentStatus as cs} WHERE {cs:CODE} IN (?status)}})");
    readyToShipConsignmentsQuery.append(Config.isSQLServerUsed() ? " AND CONVERT(VARCHAR,{con:optimizedShippingStartDate},110) = ?startDate"
        : " AND to_char({con:optimizedShippingStartDate},'MM-dd-yyyy') = ?startDate");

    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(readyToShipConsignmentsQuery.toString());

    final List<ConsignmentStatus> statusList = new ArrayList<>();
    statusList.add(ConsignmentStatus.CANCELLED);
    statusList.add(ConsignmentStatus.SHIPPED);

    addQueryParameter(shipDate, statusList, fQuery);

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

    final DateFormat dateFormat = new SimpleDateFormat(BlCoreConstants.SQL_DATE_FORMAT);

    fQuery.addQueryParameter(BlCoreConstants.START_DATE, dateFormat.format(shipDate));
    fQuery.addQueryParameter(BlCoreConstants.STATUS, statusList);
  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(
      final FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }

}
