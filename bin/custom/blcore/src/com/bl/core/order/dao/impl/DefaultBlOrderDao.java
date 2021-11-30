package com.bl.core.order.dao.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.suggestion.dao.SimpleSuggestionDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.daos.impl.DefaultOrderDao;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Default implementation of {@link SimpleSuggestionDao}.
 * @author Moumita
 */
public class DefaultBlOrderDao extends DefaultOrderDao implements BlOrderDao
{
	private UserService userService;
	private static final Logger LOG = Logger.getLogger(DefaultBlOrderDao.class);
	private static final String MANUAL_REVIEW_STATUS_BY_RESHUFFLER = "manualReviewStatusByReshuffler";
	private static final String ORDER_COMPLETED_DATE = "orderCompletedDate";
	private static final String GET_ORDERS_FOR_AUTHORIZATION_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} WHERE {con:"
			+ ConsignmentModel.OPTIMIZEDSHIPPINGSTARTDATE + "} BETWEEN ?startDate AND ?endDate AND {o:status} NOT IN "
			+ "({{select {os:pk} from {OrderStatus as os} where {os:code} = 'RECEIVED_MANUAL_REVIEW'}}) AND {o:" + AbstractOrderModel.ISAUTHORISED
			+ "} = ?isAuthorized ";

	private static final String GET_ORDERS_BY_CODE_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.CODE + "} = ?code ";

	private static final String GET_ORDERS_BY_CUSTOMER_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.USER + "} = ?user and {o:status} IN ({{select {se:pk} from {OrderStatus as se} where {se:code} IN (?orderStatuses) }})";

	private static final String GET_INCOMPLETE_ORDERS_TO_BE_PROCESSED_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} WHERE ({con:"
			+ ConsignmentModel.OPTIMIZEDSHIPPINGSTARTDATE + "} BETWEEN ?startDate AND ?endDate OR {o:" + OrderModel.ACTUALRENTALSTARTDATE
			+ "} BETWEEN ?startDate AND ?endDate) AND {o:status} IN "
			+ "({{select {os:pk} from {OrderStatus as os} where {os:code} = 'RECEIVED_MANUAL_REVIEW'}}) AND ({o:" + OrderModel.MANUALREVIEWSTATUSBYRESHUFFLER
			+ "} =?manualReviewStatusByReshuffler OR {o:" + OrderModel.MANUALREVIEWSTATUSBYRESHUFFLER + "}  is null)";

	private static final String GET_ORDERS_OF_UNAVAILABLE_SOFT_ASSIGNED_SERIALS = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o JOIN " + OrderEntryModel._TYPECODE + " AS oe ON {oe:order} = {o:pk} JOIN " + BlProductModel._TYPECODE
			+ " AS p ON {oe:product}={p:pk} LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} WHERE {p:"
			+ BlProductModel.CODE + "} IN (?productCodes) AND {con:" + ConsignmentModel.OPTIMIZEDSHIPPINGSTARTDATE + "} BETWEEN ?startDate AND ?endDate"
			+ " AND {o:status} IN ({{select {os:pk} from {OrderStatus as os} where {os:code} = 'RECEIVED'}})";

	private static final String GET_COMPLETED_RENTAL_ORDERS_FOR_SHARE_A_SALE = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + OrderModel.ISRENTALCART + "} = ?isRentalCart and {o:" + OrderModel.SHAREASALESENT + "} = ?shareASaleSent and {o:" + OrderModel.STATUS + "} = ({{select {type:" + ItemModel.PK + "} from {" + OrderStatus._TYPECODE
			+ " as type} where {type:code} = ?code}})";

	private static final String GET_ONE_YEAR_OLD_COMPLETED_ORDERS = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + OrderModel.ORDERCOMPLETEDDATE + "} > ?orderCompletedDate AND {o:"
			+ OrderModel.USER + "} IN ({{SELECT {" + ItemModel.PK + "} FROM {" + CustomerModel._TYPECODE + "} WHERE {"
			+ CustomerModel.UID + "} = ?uid}})";

	private static final String ORDERS_TO_BE_UPS_SCRAPE = "SELECT DISTINCT {" + ItemModel.PK + BlintegrationConstants.FROM
			+ OrderModel._TYPECODE + " AS o LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} WHERE {con:"
			+ ConsignmentModel.OPTIMIZEDSHIPPINGENDDATE + "} BETWEEN ?optimizedShippingStartDate AND ?optimizedShippingEndDate ";

	private static final String PACKAGES_TO_BE_UPS_SCRAPE = "SELECT {" + ItemModel.PK + BlintegrationConstants.FROM
			+ PackagingInfoModel._TYPECODE + "}" + "WHERE {" + PackagingInfoModel.PACKAGERETURNEDTOWAREHOUSE + "} = ?packageReturnedToWarehouse AND {"
			+ PackagingInfoModel.ISSCRAPESCANCOMPLETED + "} = ?isScrapeScanCompleted AND {"
			+ PackagingInfoModel.LATEPACKAGEDATE + "} BETWEEN ?startDate AND ?endDate ";

	private static final String DELAYED_OR_UPDATED_PACKAGES_TO_BE_UPS_SCRAPE = "SELECT {" + ItemModel.PK + BlintegrationConstants.FROM
			+ PackagingInfoModel._TYPECODE + "}" + "WHERE  {" + PackagingInfoModel.RETURNINGDATE + "} BETWEEN ?startDate AND ?endDate OR {"
			+ PackagingInfoModel.DELAYEDDATE +"} BETWEEN ?startDate AND ?endDate";

	private static final String GET_ORDERS_TO_OPTIMIZE_SHIP_FROM_WH_QUERY = "SELECT o.PK FROM ({{SELECT{" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} GROUP BY {"
	    + ItemModel.PK + "} HAVING COUNT(*) > 1}} INTERSECT {{SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} WHERE ({con:"
			+ ConsignmentModel.OPTIMIZEDSHIPPINGSTARTDATE + "} BETWEEN ?startDate AND ?endDate OR {o:" + OrderModel.ACTUALRENTALSTARTDATE
			+ "} BETWEEN ?startDate AND ?endDate) AND {" + OrderModel.STATUS + "} IN ({{select {os:pk} from {OrderStatus as os} where {os:code} = 'RECEIVED'}})}}) o";

	/**
 	* {@inheritDoc}
 	*/
	@Override
	public List<AbstractOrderModel> getOrdersForAuthorization()
	{
		final Date currentDate = Date
				.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
		final Date endDate = DateUtils.addDays(currentDate, 1);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_FOR_AUTHORIZATION_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.START_DATE, BlDateTimeUtils.getFormattedStartDay(currentDate).getTime());
		fQuery.addQueryParameter(BlCoreConstants.END_DATE, BlDateTimeUtils.getFormattedEndDay(endDate).getTime());
		fQuery.addQueryParameter(BlCoreConstants.IS_AUTHORISED, Boolean.FALSE);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<AbstractOrderModel> ordersToAuthorizePayment = result.getResult();
		if (CollectionUtils.isEmpty(ordersToAuthorizePayment))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No orders found to authorize the payment");
			return Collections.emptyList();
		}
		return ordersToAuthorizePayment;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public AbstractOrderModel getOrderByCode(final String orderNumber)
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_BY_CODE_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.CODE, orderNumber);
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
	@Override
	public List<AbstractOrderModel> getUnPaidBillOrderByCustomer()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_BY_CUSTOMER_QUERY);
			fQuery.addQueryParameter("user", getUserService().getCurrentUser());
			fQuery.addQueryParameter("orderStatuses", getOrderStatuses());
			final SearchResult result = getFlexibleSearchService().search(fQuery);
			if (CollectionUtils.isEmpty(result.getResult()))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "There are no orders for current user with incomplete status {} ", getUserService().getCurrentUser().getUid());
				
			}
			return result.getResult();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<AbstractOrderModel> getIncompleteOrdersToBeProcessed(final Date currentDate) {
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_INCOMPLETE_ORDERS_TO_BE_PROCESSED_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.START_DATE, BlDateTimeUtils.getFormattedStartDay(currentDate).getTime());
		fQuery.addQueryParameter(BlCoreConstants.END_DATE, BlDateTimeUtils.getFormattedEndDay(currentDate).getTime());
		fQuery.addQueryParameter(MANUAL_REVIEW_STATUS_BY_RESHUFFLER, false);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		if (CollectionUtils.isEmpty(result.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"There are no orders to be processed via reshuffler job with manual review status for the day {} ", currentDate);
		}
		return result.getResult();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<AbstractOrderModel> getOrdersOfUnavailableSoftAssignedSerials(final Date currentDate,
			final List<String> productCodes) {
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_OF_UNAVAILABLE_SOFT_ASSIGNED_SERIALS);
		fQuery.addQueryParameter(BlCoreConstants.START_DATE, BlDateTimeUtils.getFormattedStartDay(currentDate).getTime());
		fQuery.addQueryParameter(BlCoreConstants.END_DATE, BlDateTimeUtils.getFormattedEndDay(currentDate).getTime());
		fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CODES, productCodes);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		if (CollectionUtils.isEmpty(result.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"There are no other orders for the unavailable products {} for the day {} ", productCodes, currentDate);
		}
		return result.getResult();
	}

	/**
	 * @return list of order statuses
	 */
	private List<String> getOrderStatuses(){
		return Lists.newArrayList(OrderStatus.INCOMPLETE.getCode(),OrderStatus.INCOMPLETE_ITEMS_IN_REPAIR.getCode(),
				OrderStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS.getCode(),OrderStatus.INCOMPLETE_MISSING_ITEMS.getCode());
		
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<AbstractOrderModel> getCompletedRentalOrderForShareASale() {
		final FlexibleSearchQuery query = new FlexibleSearchQuery(
				GET_COMPLETED_RENTAL_ORDERS_FOR_SHARE_A_SALE);
		query.addQueryParameter(BlCoreConstants.RENTAL_ORDER, Boolean.TRUE);
		query.addQueryParameter(BlCoreConstants.SHARE_A_SALE, Boolean.FALSE);
		query.addQueryParameter(BlCoreConstants.ORDER_STATUS, OrderStatus.COMPLETED.getCode());
		final SearchResult<AbstractOrderModel> result = getFlexibleSearchService().search(query);
		final List<AbstractOrderModel> abstractOrderModelList = result.getResult();
		if (CollectionUtils.isEmpty(abstractOrderModelList)) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					BlCoreConstants.SHARE_A_SALE_ORDERS_NOT_EXIST);
			return Collections.emptyList();
		}
		return abstractOrderModelList;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<AbstractOrderModel> getOneYearOldCompletedOrders(final Date oneYearPastDate,
			final CustomerModel customerModel) {
		final FlexibleSearchQuery query = new FlexibleSearchQuery(
				GET_ONE_YEAR_OLD_COMPLETED_ORDERS);
		query.addQueryParameter(ORDER_COMPLETED_DATE, oneYearPastDate);
		query.addQueryParameter(BlCoreConstants.UID, customerModel.getUid());
		final SearchResult<AbstractOrderModel> result = getFlexibleSearchService().search(query);
		final List<AbstractOrderModel> abstractOrderModelList = result.getResult();
		if (CollectionUtils.isEmpty(abstractOrderModelList)) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No Orders Found in past one year as per order completed date");
			return Collections.emptyList();
		}
		return abstractOrderModelList;
	}


	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<AbstractOrderModel> getOrdersForUPSScrape()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(ORDERS_TO_BE_UPS_SCRAPE);
		fQuery.addQueryParameter(BlintegrationConstants.OPTIMIZED_SHIPPING_START_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
		fQuery.addQueryParameter( BlintegrationConstants.OPTIMIZED_SHIPPING_END_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(new Date()).getTime()));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		List<AbstractOrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders)) {
			BlLogger.logMessage(LOG , Level.INFO , "No Results found for UPS Scrape service which optimizedShippingEndDate has ",
					convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
			return Collections.emptyList();
		}
		return orders;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<PackagingInfoModel> getRescheduledPackagesForUPSScrape()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(PACKAGES_TO_BE_UPS_SCRAPE);
		fQuery.addQueryParameter(BlintegrationConstants.PACKAGE_RETURNED_TO_WAREHOUSE , Boolean.FALSE);
		fQuery.addQueryParameter(BlintegrationConstants.IS_SCRAPE_SCAN_COMPLETED , Boolean.TRUE);
		fQuery.addQueryParameter(BlintegrationConstants.START_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
		fQuery.addQueryParameter(BlintegrationConstants.END_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(new Date()).getTime()));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<PackagingInfoModel> packagingInfoModels = result.getResult();
		if (CollectionUtils.isEmpty(packagingInfoModels))
		{
			BlLogger.logMessage(LOG , Level.INFO , "No Results found for getRescheduledPackagesForUPSScrape which latePackage has ",
					convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
			return Collections.emptyList();
		}
		return packagingInfoModels;
	}


	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<PackagingInfoModel> getDelayedOrUpdatedPackagesForUPSScrape()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(DELAYED_OR_UPDATED_PACKAGES_TO_BE_UPS_SCRAPE);
		fQuery.addQueryParameter(BlintegrationConstants.PACKAGE_RETURNED_TO_WAREHOUSE , Boolean.FALSE);
		fQuery.addQueryParameter(BlintegrationConstants.START_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
		fQuery.addQueryParameter(BlintegrationConstants.END_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(new Date()).getTime()));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<PackagingInfoModel> packagingInfoModels = result.getResult();
		if (CollectionUtils.isEmpty(packagingInfoModels))
		{
			BlLogger.logMessage(LOG , Level.INFO , "No Results found for getRescheduledPackagesForUPSScrape which latePackage has ",
					convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
			return Collections.emptyList();
		}
		return packagingInfoModels;
	}

	/**
	 * {@inheritDoc}
	 */
	public List<AbstractOrderModel> getOrdersToOptimizeShipFromWH(final Date currentDate) {
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_TO_OPTIMIZE_SHIP_FROM_WH_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.START_DATE, BlDateTimeUtils.getFormattedStartDay(currentDate).getTime());
		fQuery.addQueryParameter(BlCoreConstants.END_DATE, BlDateTimeUtils.getFormattedEndDay(currentDate).getTime());
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		if (CollectionUtils.isEmpty(result.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"There are no orders to be processed via reshuffler job to optimize ship from warehouse for the day {} ", currentDate);
		}
		return result.getResult();
	}

	/**
	 * This method created to convert date into specific format
	 * @param dateToConvert the date which required to convert
	 * @return String after conversion into specific format
	 */
	private String convertDateIntoSpecificFormat(final Date dateToConvert) {
		final SimpleDateFormat sdf = new SimpleDateFormat(BlintegrationConstants.DATE_FORMATTER);
		return sdf.format(dateToConvert);
	}

	/**
	 * @return the userService
	 */
	public UserService getUserService()
	{
		return userService;
	}

	/**
	 * @param userService the userService to set
	 */
	public void setUserService(UserService userService)
	{
		this.userService = userService;
	}
	
	

}
