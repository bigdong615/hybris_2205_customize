package com.bl.integration.dao.impl;

import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.dao.BlOrderDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This Class created to get the order from Data Base
 * @author Manikandan
 */
public class DefaultBlOrderDao implements BlOrderDao {

  private static final Logger LOG = Logger.getLogger(DefaultBlOrderDao.class);

  private static final String GET_ORDERS_BY_CODE_QUERY = "SELECT {" + ItemModel.PK + BlintegrationConstants.FROM
      + OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.CODE + "} = ?code ";

  private static final String ORDERS_TO_BE_UPS_SCRAPE = "SELECT DISTINCT {" + ItemModel.PK + BlintegrationConstants.FROM
      + OrderModel._TYPECODE + " AS o LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} WHERE {con:"
        + ConsignmentModel.OPTIMIZEDSHIPPINGENDDATE + "} BETWEEN ?optimizedShippingStartDate AND ?optimizedShippingEndDate ";

  private static final String PACKAGES_TO_BE_UPS_SCRAPE = "SELECT {" + ItemModel.PK + BlintegrationConstants.FROM
      + PackagingInfoModel._TYPECODE + "}" + "WHERE {" + PackagingInfoModel.PACKAGERETURNEDTOWAREHOUSE + "} = ?packageReturnedToWarehouse AND {"
      + PackagingInfoModel.ISSCRAPESCANCOMPLETED + "} = ?isScrapeScanCompleted AND {"
      + PackagingInfoModel.LATEPACKAGEDATE + "} BETWEEN ?startDate AND ?endDate ";

  protected FlexibleSearchService flexibleSearchService;


  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractOrderModel getOrderByCode(final String orderNumber)
  {
    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_BY_CODE_QUERY);
    fQuery.addQueryParameter(BlintegrationConstants.CODE, orderNumber);
    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<AbstractOrderModel> orders = result.getResult();
    if (CollectionUtils.isEmpty(orders))
    {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No orders with code {} exists", orderNumber);
      return null;
    } else if(orders.size() > 1) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "More than two orders were found with code {} ", orderNumber);
    }
    return orders.get(0);
  }


  /**
   * {@inheritDoc}
   */
  public List<AbstractOrderModel> getOrdersForUPSScrape()
  {
     List<AbstractOrderModel> orders;
      final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(ORDERS_TO_BE_UPS_SCRAPE);
      fQuery.addQueryParameter(BlintegrationConstants.OPTIMIZED_SHIPPING_START_DATE, convertDateIntoSpecificFormat(getFormattedStartDay(new Date())));
      fQuery.addQueryParameter( BlintegrationConstants.OPTIMIZED_SHIPPING_END_DATE, convertDateIntoSpecificFormat(getFormattedEndDay(new Date())));
      final SearchResult result = getFlexibleSearchService().search(fQuery);
      orders = result.getResult();
      if (CollectionUtils.isEmpty(orders)) {
        return Collections.emptyList();
      }
      return orders;
  }

  /**
   * {@inheritDoc}
   */
  public List<PackagingInfoModel> getRescheduledPackagesForUPSScrape()
  {
    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(PACKAGES_TO_BE_UPS_SCRAPE);
    fQuery.addQueryParameter(BlintegrationConstants.PACKAGE_RETURNED_TO_WAREHOUSE , Boolean.FALSE);
    fQuery.addQueryParameter(BlintegrationConstants.IS_SCRAPE_SCAN_COMPLETED , Boolean.TRUE);
    fQuery.addQueryParameter(BlintegrationConstants.START_DATE, convertDateIntoSpecificFormat(getFormattedStartDay(new Date())));
    fQuery.addQueryParameter(BlintegrationConstants.END_DATE, convertDateIntoSpecificFormat(getFormattedEndDay(new Date())));
    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<PackagingInfoModel> packagingInfoModels = result.getResult();
    if (CollectionUtils.isEmpty(packagingInfoModels))
    {
      return Collections.emptyList();
    }
    return packagingInfoModels;
  }

  public Date getFormattedStartDay(final Date day) {
    final Calendar startDate = Calendar.getInstance();
    startDate.setTime(day);
    startDate.set(Calendar.HOUR_OF_DAY, BlintegrationConstants.START_HOURS);
    startDate.set(Calendar.MINUTE, BlintegrationConstants.START_MINUTES);
    startDate.set(Calendar.SECOND, BlintegrationConstants.START_SECONDS);
    return startDate.getTime();
  }



  /**
   * To get the formatted date
   * @param day the date
   * @return Calendar
   */
  public static Date getFormattedEndDay(final Date day) {
    final Calendar startDate = new GregorianCalendar();
    startDate.setTime(day);
    startDate.set(Calendar.HOUR_OF_DAY, BlintegrationConstants.END_HOURS);
    startDate.set(Calendar.MINUTE, BlintegrationConstants.END_MINUTES);
    startDate.set(Calendar.SECOND, BlintegrationConstants.END_SECONDS);
    return startDate.getTime();
  }

  /**
   * This method created to convert date into specific format
   * @param dateToConvert date
   * @return String
   */
  private String convertDateIntoSpecificFormat(final Date dateToConvert) {
    final SimpleDateFormat sdf = new SimpleDateFormat(BlintegrationConstants.DATE_FORMATTER);
    return  sdf.format(dateToConvert);
  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(
      FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }



}
