/**
 *
 */
package com.bl.core.services.queries.dao;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.util.List;

import com.bl.core.model.BlPullOrdersQueueModel;
import com.bl.core.model.BlPulledOrdersAwaitingQueueModel;


public interface BlPullQueriesDao
{
	List<ConsignmentModel> pullOrdersAwaitingQueue();

	List<BlPulledOrdersAwaitingQueueModel> ordersAwaitingQueue();

	List<ConsignmentModel> pulledOrdersQueue();

	List<BlPullOrdersQueueModel> ordersQueue();
}
