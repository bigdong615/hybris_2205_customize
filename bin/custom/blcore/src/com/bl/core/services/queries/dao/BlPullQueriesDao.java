/**
 *
 */
package com.bl.core.services.queries.dao;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.util.List;


public interface BlPullQueriesDao
{
	List<ConsignmentModel> pullOrdersAwaitingQueue();

	List<ConsignmentModel> pulledOrdersQueue();
}
