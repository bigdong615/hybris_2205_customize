package com.bl.core.order.dao.impl;

import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.daos.impl.DefaultOrderDao;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.task.TaskConditionModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.suggestion.dao.SimpleSuggestionDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

/**
 * Default implementation of {@link SimpleSuggestionDao}.
 * @author Moumita
 */
public class DefaultBlOrderDao extends DefaultOrderDao implements BlOrderDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlOrderDao.class);
	private UserService userService;
	private ConfigurationService configurationService;
	private BaseStoreService baseStoreService;
	private static final String MANUAL_REVIEW_STATUS_BY_RESHUFFLER = "manualReviewStatusByReshuffler";
	private static final String ORDER_COMPLETED_DATE = "orderCompletedDate";
	private static final String TIMER = "timer";
	private static final Integer BUFFER_TO_CLEAR_ABANDONED_USEDGEAR_CARTS = 8;
	private static final String IS_EXTENDED_ORDER ="isExtendedOrder";
	private static final String IS_REPLACEMENT_ORDER ="isReplacementOrder";
	private static final String IS_AUTHORIZATION_VOIDED ="isAuthorizationVoided";
	private static final String DELAY_VOID_TRANSACTION_BY_TIME = "delay.void.transaction.time";
	private static final String IS_GIFT_CARD_ORDER = "isGiftCardOrder";
	private static final String IS_NEW_GEAR_ORDER = "isNewGearOrder";
	private static final String IS_AUTHORIZATION_ATTEMPTED = "isAuthorizationAttempted";

	private static final String GET_ORDERS_FOR_AUTHORIZATION_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o LEFT JOIN " + ConsignmentModel._TYPECODE + " AS con ON {con:order} = {o:pk}} WHERE {con:"
			+ ConsignmentModel.OPTIMIZEDSHIPPINGSTARTDATE + "} BETWEEN ?startDate AND ?endDate AND {o:status} NOT IN "
			+ "({{select {os:pk} from {OrderStatus as os} where {os:code} = 'RECEIVED_MANUAL_REVIEW' OR {os:code} = 'CANCELLED'}}) AND {o:" + AbstractOrderModel.ISAUTHORISED
			+ "} = ?isAuthorized AND ({o:" + AbstractOrderModel.ISAUTHORIZATIONATTEMPTED + "} = ?isAuthorizationAttempted OR {o:"
			+ AbstractOrderModel.ISAUTHORIZATIONATTEMPTED + "} is null)" ;

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
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + OrderModel.ISRENTALORDER + "} = ?isRentalCart and {o:" + OrderModel.SHAREASALESENT + "} = ?shareASaleSent and {o:"+OrderModel.RENTALENDDATE + "} >= ?previousYearEndDate  and {o:" + OrderModel.STATUS + "} = ({{select {type:" + ItemModel.PK + "} from {" + OrderStatus._TYPECODE
			+ " as type} where {type:code} = ?code}})";

	private static final String GET_ONE_YEAR_OLD_COMPLETED_ORDERS = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + OrderModel.ORDERCOMPLETEDDATE + "} > ?orderCompletedDate AND {o:"
			+ OrderModel.USER + "} IN ({{SELECT {" + ItemModel.PK + "} FROM {" + CustomerModel._TYPECODE + "} WHERE {"
			+ CustomerModel.UID + "} = ?uid}})";

	private static final String ORDERS_TO_BE_UPS_SCRAPE =  "SELECT distinct{" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o } WHERE {o:" + OrderModel.RENTALENDDATE + "} <= ?endDate AND {o:"
			+ OrderModel.ISLATESTORDER	+ "} = 1  AND {o:" + OrderModel.ISSAPORDER+ "} = 1  AND {" + OrderModel.STATUS + "} IN "
			+ "({{select {os:pk} from {OrderStatus as os} where {os:code} = 'SHIPPED'}})";
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

	private static final String ORDERS_TO_FEED_FTP  = "SELECT DISTINCT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.ORDERMODIFIEDDATE + "} BETWEEN ?orderModifiedDate AND ?orderModifiedEndDate ";

	private static final String ORDERS_BILL_TO_FEED_FTP  = "SELECT DISTINCT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.ORDERBILLMODIFIEDDATE + "} BETWEEN ?orderBillModifiedDate AND ?orderBillModifiedEndDate";

	private static final String ORDERS_TO_FEED_FTP_BY_DATE  = "SELECT DISTINCT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.ORDERMODIFIEDDATE +
			"} BETWEEN ?orderModifiedDate AND ?orderModifiedEndDate AND {o.sentOrderFeedToSalesforce} IN "
			+ "({{select {es:pk} from {ExportStatus as es} where {es:code} = 'NOTEXPORTED'}})";

	private static final String ORDERS_BILL_TO_FEED_FTP_BY_DATE  = "SELECT DISTINCT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.ORDERBILLMODIFIEDDATE +
			"} BETWEEN ?orderBillModifiedDate AND ?orderBillModifiedEndDate AND {o.sentOrderFeedToSalesforce} IN "
			+ "({{select {es:pk} from {ExportStatus as es} where {es:code} = 'NOTEXPORTED'}})";

	private static final String USED_GEAR_ABANDONED_CARTS  = "SELECT {" + ItemModel.PK + "} FROM {"
			+ CartEntryModel._TYPECODE + " AS c} WHERE  datediff(ss,{c:" + CartEntryModel.CREATIONTIME + "},current_timestamp) > ?timer";

	private static final String GET_ORDERS_TO_VOID_TRANSACTION  = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + OrderModel.ISAUTHORIZATIONVOIDED +
			"} =?isAuthorizationVoided AND {o:" + OrderModel.ISEXTENDEDORDER + "} =?isExtendedOrder AND "
			+ "{o:" + OrderModel.ISREPLACEMENTORDER + "} =?isReplacementOrder AND "
			+ "{o:" + OrderModel.GIFTCARDORDER + "} =?isGiftCardOrder AND "
			+ "{o:" + OrderModel.ISRETAILGEARORDER + "} =?isNewGearOrder AND "
			+ "{o:" + OrderModel.ORIGINALVERSION + "} is null AND datediff(mi,{o:" + OrderModel.CREATIONTIME + "},current_timestamp) > ?timer";

	private static final String GET_ALL_RENTAL_LEGACY_ORDERS_QUERY = "SELECT {o:" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE "
					+ "{o:" + AbstractOrderModel.STATUS + "} = ({{SELECT {os:" + ItemModel.PK + "} from {"+ OrderStatus._TYPECODE + " AS os} where {os:code} = ?orderStatus}}) AND "
					+ "({o:" + AbstractOrderModel.ISSAPORDER + "} IS NULL OR "
					+ "{o:" + AbstractOrderModel.ISSAPORDER + "} = ?isSAPOrder ) AND "
					+ "{o:" + AbstractOrderModel.ISRENTALORDER + "} = ?isRentalOrder AND "
					+ "{o:" + AbstractOrderModel.GIFTCARDORDER + "} = ?isGiftCardOrder AND "
					+ "{o:" + AbstractOrderModel.ISRETAILGEARORDER + "} = ?isRetailGearOrder AND "
					+ "{o:" + AbstractOrderModel.ISREPLACEMENTORDER + "} = ?isReplacementOrder AND "
					+ "{o:" + AbstractOrderModel.ISEXTENDEDORDER + "} = ?isExtendedOrder AND "
					+ "{o:" + AbstractOrderModel.INTERNALTRANSFERORDER + "} = ?internalTransferOrder" ;

	private static final String RETURN_ORDERS_FEED_QUERY = "SELECT DISTINCT {" + ItemModel.PK + "} FROM {" + OrderModel._TYPECODE
			+ " AS o} WHERE {o:" + OrderModel.RENTALENDDATE
			+ "} BETWEEN ?returnOrderBefore AND ?returnOrderAfter and {o:status} NOT IN ({{select {se:pk} from {OrderStatus as se} where {se:code} IN (?orderStatuses)}})";

	private static final String ORIGINAL_ORDER_BY_CODE = "SELECT {" + ItemModel.PK + "} FROM {" + OrderModel._TYPECODE + " AS o } WHERE {o:" + OrderModel.VERSIONID  + "} IS NULL AND {" + OrderModel.CODE+ "} = ?code";

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
		fQuery.addQueryParameter(IS_AUTHORIZATION_ATTEMPTED, Boolean.FALSE);
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
	
	private List<String> getOrderStatusesForReturnOrderFeed(){
		return Lists.newArrayList(OrderStatus.CANCELLED.getCode(),OrderStatus.UNBOXED_COMPLETELY.getCode(),
				OrderStatus.UNBOXED_PARTIALLY.getCode(),OrderStatus.COMPLETED.getCode());

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
		final Calendar cal = Calendar.getInstance();
		cal.add(Calendar.YEAR, -1);
		final Date previousYear = cal.getTime();
    query.addQueryParameter(BlCoreConstants.PREVIOUS_YEAR_END_DATE,previousYear);
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
		fQuery.addQueryParameter(BlintegrationConstants.END_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(new Date()).getTime()));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<AbstractOrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders)) {
			BlLogger.logMessage(LOG , Level.INFO , "No Results found for UPS Scrape service which is same or before rental end date has ",
					convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(new Date()).getTime()));
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
	 * This method created to get Order to Feed FTP
	 * @return list of orders
	 */
	@Override
	public List<AbstractOrderModel> getOrdersForOrderFeedToFTP() {
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(ORDERS_TO_FEED_FTP);
		fQuery.addQueryParameter(BlCoreConstants.ORDER_MODIFIED_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
		fQuery.addQueryParameter(BlCoreConstants.ORDER_MODIFIED_END_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(new Date()).getTime()));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<AbstractOrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders)) {
			BlLogger.logFormattedMessage(LOG , Level.INFO , "No orders found for Order feed with date {}",
					convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
			return Collections.emptyList();
		}
		return orders;
	}

	/**
	 * This method created to get OrderBill to Feed FTP
	 * @return list of orders
	 */
	@Override
	public List<AbstractOrderModel> getOrdersForOrderBillFeedToFTP() {
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(ORDERS_BILL_TO_FEED_FTP);
		fQuery.addQueryParameter(BlCoreConstants.ORDER_BILL_MODIFIED_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
		fQuery.addQueryParameter(BlCoreConstants.ORDER_BILL_MODIFIED_END_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(new Date()).getTime()));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<AbstractOrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders)) {
			BlLogger.logFormattedMessage(LOG , Level.INFO , "No orders found for Order bill feed with date {} ",
					convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(new Date()).getTime()));
			return Collections.emptyList();
		}
		return orders;
	}


  /**
   * This method created to get list of orders based on specified date
   * @param orderFeedDate date to get orders
   * @return list of order models
   */
	@Override
	public List<AbstractOrderModel> getOrdersForOrderFeedToFTPBasedOnSpecificDate(final Date orderFeedDate) {
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(ORDERS_TO_FEED_FTP_BY_DATE);
		fQuery.addQueryParameter(BlCoreConstants.ORDER_MODIFIED_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(orderFeedDate).getTime()));
		fQuery.addQueryParameter(BlCoreConstants.ORDER_MODIFIED_END_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(orderFeedDate).getTime()));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<AbstractOrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders)) {
			BlLogger.logFormattedMessage(LOG , Level.INFO , "No orders found for Order feed with date {}",
					convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(orderFeedDate).getTime()));
			return Collections.emptyList();
		}
		return orders;
	}

	/**
	 * This method created to get list of orders based on specified date
	 * @param orderBillFeedDate date to get orders
	 * @return list of order models
	 */
	@Override
	public List<AbstractOrderModel> getOrdersForOrderBillFeedToFTPBasedOnSpecificDate(final Date orderBillFeedDate) {
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(ORDERS_BILL_TO_FEED_FTP_BY_DATE);
		fQuery.addQueryParameter(BlCoreConstants.ORDER_BILL_MODIFIED_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(orderBillFeedDate).getTime()));
		fQuery.addQueryParameter(BlCoreConstants.ORDER_BILL_MODIFIED_END_DATE, convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedEndDay(orderBillFeedDate).getTime()));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<AbstractOrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders)) {
			BlLogger.logFormattedMessage(LOG , Level.INFO , "No orders found for Order bill feed with date {}",
					convertDateIntoSpecificFormat(BlDateTimeUtils.getFormattedStartDay(orderBillFeedDate).getTime()));
			return Collections.emptyList();
		}
		return orders;
	}

	/**
	 * {@inheritDoc}
	 */
	public List<CartEntryModel> getAllUsedGearAbandonedCarts() {
		final BaseStoreModel baseStore = getBaseStoreService()
				.getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(USED_GEAR_ABANDONED_CARTS);
		// Added 8 seconds buffer, so that cron job will never clear the carts before it gets cleared from front end
		fQuery.addQueryParameter(TIMER, Integer.valueOf(baseStore.getUsedGearCartTimer())
				+ BUFFER_TO_CLEAR_ABANDONED_USEDGEAR_CARTS);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<CartEntryModel> cartEntries = result.getResult();
		if (CollectionUtils.isEmpty(cartEntries)) {
			BlLogger.logMessage(LOG, Level.INFO, "No abandoned carts found for for used gear products");
			return Collections.emptyList();
		}
		return cartEntries;
	}

		/**
		 * {@inheritDoc}
		 */
	@Override
	public List<OrderModel> getOrdersToVoidTransactions() {
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_TO_VOID_TRANSACTION);
		fQuery.addQueryParameter(IS_AUTHORIZATION_VOIDED, Boolean.FALSE);
		fQuery.addQueryParameter(IS_EXTENDED_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(IS_REPLACEMENT_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(IS_GIFT_CARD_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(IS_NEW_GEAR_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(TIMER, getConfigurationService().getConfiguration().getInt(DELAY_VOID_TRANSACTION_BY_TIME));
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<OrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders)) {
			BlLogger.logMessage(LOG , Level.INFO , "No orders found to void $1 authorization transactions");
			return Collections.emptyList();
		}
		return orders;
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
	public void setUserService(final UserService userService)
	{
		this.userService = userService;
	}

	public ConfigurationService getConfigurationService() {
		return configurationService;
	}

	public void setConfigurationService(final ConfigurationService configurationService) {
		this.configurationService = configurationService;
	}

	public BaseStoreService getBaseStoreService() {
		return baseStoreService;
	}

	public void setBaseStoreService(final BaseStoreService baseStoreService) {
		this.baseStoreService = baseStoreService;
	}

	@Override
	public List<OrderModel> getAllLegacyOrders()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ALL_RENTAL_LEGACY_ORDERS_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.QRY_ORDER_STATUS, OrderStatus.PENDING.getCode());
		fQuery.addQueryParameter(BlCoreConstants.IS_SAP_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(BlCoreConstants.IS_RENTAL_ORDER, Boolean.TRUE);
		fQuery.addQueryParameter(IS_GIFT_CARD_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(BlCoreConstants.IS_RETAIL_GEAR_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(BlCoreConstants.IS_REPLACEMENT_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(BlCoreConstants.IS_EXTENDED_ORDER, Boolean.FALSE);
		fQuery.addQueryParameter(BlCoreConstants.INTERNAL_TRANSFER_ORDER, Boolean.FALSE);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<OrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders)) {
			BlLogger.logMessage(LOG , Level.INFO , "No Legacy rental orders found");
			return Collections.emptyList();
		}
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Number of Legacy rental orders found : {}", orders.size());
		return orders;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<TaskConditionModel> getTaskCondition(final String code) {
		final StringBuilder sql = new StringBuilder();
		sql.append("SELECT {tm:pk} ");
		sql.append("FROM {").append(TaskConditionModel._TYPECODE).append(" AS tm} ");
		sql.append("WHERE {tm:uniqueID} LIKE '%").append(code).append("%'");
		final FlexibleSearchQuery query = new FlexibleSearchQuery(sql.toString());
		final SearchResult result = getFlexibleSearchService().search(query);
		final List<TaskConditionModel> taskConditions = result.getResult();
		if (CollectionUtils.isEmpty(taskConditions)) {
			BlLogger.logMessage(LOG , Level.INFO , "No associated task conditions found");
			return Collections.emptyList();
		}
		return taskConditions;
	}

	@Override
	public List<OrderModel> getOrdersReadyForReturn() throws ParseException
	{
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat("MM-dd-yyyy");
		String date = simpleDateFormat.format(new Date());
		Date currentDate = simpleDateFormat.parse(date);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(RETURN_ORDERS_FEED_QUERY);
		fQuery.addQueryParameter("returnOrderBefore", BlDateTimeUtils.getFormattedStartDay(currentDate).getTime());
		fQuery.addQueryParameter("returnOrderAfter", BlDateTimeUtils.getFormattedEndDay(currentDate).getTime());
		fQuery.addQueryParameter("orderStatuses", getOrderStatusesForReturnOrderFeed());
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<OrderModel> orders = result.getResult();
		if (CollectionUtils.isEmpty(orders))
		{
			BlLogger.logMessage(LOG, Level.INFO, "No orders found for Return Order feed with date {}");
			return Collections.emptyList();
		}
		return orders;
	}

	/**
	 * get Original order from extended order
	 *
	 * @param code
	 * @return
	 */
	@Override
	public OrderModel getOriginalOrderFromExtendedOrderCode(String code) {
		validateParameterNotNull(code, "code must not be null");

		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(ORIGINAL_ORDER_BY_CODE);
		fQuery.addQueryParameter(OrderModel.CODE, code);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<OrderModel> orders = result.getResult();
		if (orders.isEmpty()) {
			BlLogger.logMessage(LOG , Level.ERROR , "Cannot find order with code: " + code);
		}
		return orders.size() == 1 ? orders.get(0) : null;
	}
}
