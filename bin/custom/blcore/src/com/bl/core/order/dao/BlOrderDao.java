package com.bl.core.order.dao;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.order.daos.OrderDao;
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
}
