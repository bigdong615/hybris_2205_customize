package com.bl.core.order.dao;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.order.daos.OrderDao;
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
   * @param date the date
	 */
	public List<AbstractOrderModel> getIncompleteOrdersToBeProcessed(Date currentDate,
      Date date);
	/**
	 * It gets the rental orders which are completed and shareASale value is false.
	 * @return list of orders
	 */
	public List<AbstractOrderModel> getCompletedRentalOrderForShareASale();

}
