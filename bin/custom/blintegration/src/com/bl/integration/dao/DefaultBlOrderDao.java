package com.bl.integration.dao;

import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class DefaultBlOrderDao

{

  public static final int END_HOURS = 23;
  public static final int END_MINUTES = 59;
  public static final int END_SECONDS = 59;
  public static final int END_MILLI_SECONDS = 999;
  public static final int START_HOURS = 0;
  public static final int START_MINUTES = 0;
  public static final int START_SECONDS = 0;

  private static final Logger LOG = Logger.getLogger(DefaultBlOrderDao.class);
  private static final String GET_ORDERS_BY_CODE_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
      + OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.CODE + "} = ?code ";
  public static final String CODE = "code";
  public static final String DATE = "endDate";

  private static final String ORDERS_TO_BE_UPS_SCRAPE = "SELECT {" + ItemModel.PK + "} FROM {"
      + OrderModel._TYPECODE + " AS o LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} WHERE {con:"
      + ConsignmentModel.OPTIMIZEDSHIPPINGENDDATE + "} = ?startDate" /*AND {con:"
      + ConsignmentModel.OPTIMIZEDSHIPPINGENDDATE + "} <= ?endDate"*/;/* OR {con:" + ConsignmentModel.OPTIMIZEDSHIPPINGENDDATE +"} = ?startDate OR {con:"
      + ConsignmentModel.OPTIMIZEDSHIPPINGENDDATE +"} = ?endDate";*/


  protected FlexibleSearchService flexibleSearchService;

  public AbstractOrderModel getOrderByCode(final String orderNumber)
  {
    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_BY_CODE_QUERY);
    fQuery.addQueryParameter(CODE, orderNumber);
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




  public List<AbstractOrderModel> getOrdersForUPSScrape()
  {

    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(ORDERS_TO_BE_UPS_SCRAPE);
    fQuery.addQueryParameter("startDate" , getFormattedStartDay(new Date()));
   // fQuery.addQueryParameter( DATE, getFormattedEndDay(new Date()));
    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<AbstractOrderModel> orders = result.getResult();
    if (CollectionUtils.isEmpty(orders))
    {
      return null;
    }
    return orders;
  }

  public static Date getFormattedStartDay(final Date day) {
    final Calendar startDate = new GregorianCalendar();
    startDate.setTime(day);
    startDate.set(Calendar.HOUR_OF_DAY, START_HOURS);
    startDate.set(Calendar.MINUTE, START_MINUTES);
    startDate.set(Calendar.SECOND, START_SECONDS);
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
    startDate.set(Calendar.HOUR_OF_DAY, END_HOURS);
    startDate.set(Calendar.MINUTE, END_MINUTES);
    startDate.set(Calendar.SECOND, END_SECONDS);
    return startDate.getTime();
  }


  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(
      FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }

}
