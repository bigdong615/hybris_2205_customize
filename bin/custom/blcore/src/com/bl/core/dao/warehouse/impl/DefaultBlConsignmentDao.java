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

  private static final String FIND_READY_TO_SHIP_CONSIGNMENTS_FOR_DATE =
      "SELECT {pk} FROM {" + ConsignmentModel._TYPECODE + "} WHERE {"
          + ConsignmentModel.STATUS + "} NOT IN (?status) AND  {"
          + ConsignmentModel.SHIPPINGDATE + "} >= ?shipDate";

  /**
   * Get consignments
   *
   * @return ConsignmentModels
   */
  @Override
  public List<ConsignmentModel> getReadyToShipConsignmentsForDate(final Date shipDate) {

    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(FIND_READY_TO_SHIP_CONSIGNMENTS_FOR_DATE);

    List<ConsignmentStatus> statusList = new ArrayList<>();
    statusList.add(ConsignmentStatus.CANCELLED);
    statusList.add(ConsignmentStatus.SHIPPED);

    addQueryParameter(shipDate, statusList, fQuery);

    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<ConsignmentModel> consignmentModels = result.getResult();
    if (CollectionUtils.isEmpty(consignmentModels))
    {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "No Consignments available to ship.");
      return Collections.emptyList();
    }

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

    final Calendar startDate = BlDateTimeUtils.getFormattedStartDay(shipDate);
    fQuery.addQueryParameter(BlCoreConstants.SHIP_DATE, startDate.getTime());
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
