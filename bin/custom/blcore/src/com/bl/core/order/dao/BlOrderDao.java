package com.bl.core.order.dao;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.daos.OrderDao;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Date;
import java.util.List;


/**
 * Implementation of {@link OrderDao}.
 * @author Moumita
 */
public interface BlOrderDao extends OrderDao
{
	/**
 	* It gets all the orders for which authorization needs to be created
	* @return list of orders
 	*/
	public List<AbstractOrderModel> getOrdersForAuthorization();

	/**
	 * It gets the order by order number
	 * @return list of orders
	 */
	public AbstractOrderModel getOrderByCode(final String orderNumber);
	
	/**
	 * It gets the order by customer
	 * @return list of orders
	 */
	public List<AbstractOrderModel> getUnPaidBillOrderByCustomer();

	/**
	 * It gets the incomplete orders
	 * @return list of orders
   * @param currentDate the current date
	 */
	public List<AbstractOrderModel> getIncompleteOrdersToBeProcessed(final Date currentDate);
	/**
	 * It gets the rental orders which are completed and shareASale value is false.
	 * @return list of orders
	 */
	public List<AbstractOrderModel> getCompletedRentalOrderForShareASale();

	/**
	 * It fetches the orders which have the products which does not have enough stocks to get fulfilled
	 * @param currentDate the current date
	 * @param productCodes list of product code
	 * @return list of order model
	 */
	public List<AbstractOrderModel> getOrdersOfUnavailableSoftAssignedSerials(final Date currentDate,
			final List<String> productCodes);

	/**
	 * It loads the value of rolling spend attribute
	 * @param oneYearPastDate the past date before one year
	 * @param customerModel the customer model
	 * @return list of order model
	 */
	public List<AbstractOrderModel> getOneYearOldCompletedOrders(final Date oneYearPastDate,
			final CustomerModel customerModel);

	/**
	 * This method created to get the list of order
	 * @return List<AbstractOrderModel> list of orders to be send to UPS scrape
	 */
	List<AbstractOrderModel> getOrdersForUPSScrape();

	/**
	 * This method created to get the list of packages
	 * @return List<PackagingInfoModel> list of packages to be send to UPS scrape
	 */
	List<PackagingInfoModel> getRescheduledPackagesForUPSScrape();

	/**
	 * This method created to get the list of packages which delayed or updated
	 * @return List<PackagingInfoModel> list of packages to be send to UPS scrape
	 */
	List<PackagingInfoModel> getDelayedOrUpdatedPackagesForUPSScrape();

	/**
	 * It gets the orders to fulfill from one warehouse
	 * @param currentDate the current date
	 * @return list of orders
	 */
	public List<AbstractOrderModel> getOrdersToOptimizeShipFromWH(final Date currentDate);
}
