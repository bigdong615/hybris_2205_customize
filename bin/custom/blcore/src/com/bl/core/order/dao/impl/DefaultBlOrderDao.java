package com.bl.core.order.dao.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.suggestion.dao.SimpleSuggestionDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.daos.impl.DefaultOrderDao;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.user.UserService;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;
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
	private static final String GET_ORDERS_FOR_AUTHORIZATION_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.ISAUTHORISED + "} = ?isAuthorized ";

	private static final String GET_ORDERS_BY_CODE_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.CODE + "} = ?code ";

	private static final String GET_ORDERS_BY_CUSTOMER_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.USER + "} = ?user and {o:status} IN ({{select {se:pk} from {OrderStatus as se} where {se:code} IN (?orderStatuses) }})";

	private static final String GET_COMPLETED_RENTAL_ORDERS_FOR_SHARE_A_SALE = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + OrderModel.ISRENTALCART + "} = ?isRentalCart and {o:" + OrderModel.SHAREASALESENT + "} = ?shareASaleSent and {o:" + OrderModel.STATUS + "} = ({{select {type:" + ItemModel.PK + "} from {" + OrderStatus._TYPECODE
			+ " as type} where {type:code} = ?code}})";

	/**
 	* {@inheritDoc}
 	*/
	@Override
	public List<AbstractOrderModel> getOrdersForAuthorization()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_FOR_AUTHORIZATION_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.IS_AUTHORISED, Boolean.FALSE);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<AbstractOrderModel> ordersToAuthorizePayment = result.getResult();
		final List<AbstractOrderModel> ordersToAuthPayment = new ArrayList<>();
		if (CollectionUtils.isEmpty(ordersToAuthorizePayment))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No orders found to authorize the payment");
			return Collections.emptyList();
		} else {
			ordersToAuthorizePayment.stream().forEach(order -> {
				if(checkDifferenceBetweenShippingAndCurrentDate(order)) {
					ordersToAuthPayment.add(order);
				}
			});
		}
		return ordersToAuthPayment;
		
	}

	/**
	 * It checks the difference between shipping date and current date
	 * @param order
	 * @return true if difference between shipping date and current date is 0 or 1
	 */
	private boolean checkDifferenceBetweenShippingAndCurrentDate(final AbstractOrderModel order) {
		final Set<ConsignmentModel> consignments = order.getConsignments();
		final LocalDateTime currentDate = BlDateTimeUtils.getFormattedDateTime(Date
				.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant()));
		final Optional<ConsignmentModel> orderToAuthPayment = consignments.stream().filter(consignmentModel ->
				isEligibleForPaymentAuthorization(consignmentModel.getOptimizedShippingStartDate(), currentDate)).findFirst();
				if(orderToAuthPayment.isPresent()) {
					return true;
				}
		return false;
	}

	/**
	 * It checks the difference between shipping date and current date
	 * @param shippingStartDate
	 * @param currentDate
	 * @return true if difference between shipping date and current date is 0 or 1
	 */
	private boolean isEligibleForPaymentAuthorization(final Date shippingStartDate, final LocalDateTime currentDate) {
		if(null != shippingStartDate) {
			final LocalDateTime optimizedShippingStartDate =BlDateTimeUtils.getFormattedDateTime(shippingStartDate);
			final long DifferenceInDays = ChronoUnit.DAYS.between(currentDate, optimizedShippingStartDate);
			return DifferenceInDays ==0 || DifferenceInDays == 1;
		}
		return false;
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
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "There are no orders for current user with incomplted status {} ", getUserService().getCurrentUser().getUid());
				
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
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					BlCoreConstants.SHARE_A_SALE_ORDERS_NOT_EXIST);
			return Collections.emptyList();
		}
		return abstractOrderModelList;
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
