/**
 *
 */
package com.bl.core.services.queries.dao.impl;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.List;


public class BlPulledQueriesDaoImpl
{
	private FlexibleSearchService flexibleSearchService;

	private static final String pullOrdersAwaitingQueueQuery = "Select {c.pk} from {consignment as c join ConsignmentStatus as cs on {c.status} = {cs.pk}} where {cs.code} = 'RECEIVED_READY_TO_SHIP'";

	private static final String pulledOrdersQueueQuery = "Select {c.pk} from {consignment as c join ConsignmentStatus as cs on {c.status} = {cs.pk}} where {cs.code} != 'RECEIVED_READY_TO_SHIP'";

	public List<ConsignmentModel> pullOrdersAwaitingQueue()
	{
		final SearchResult<ConsignmentModel> result = getFlexibleSearchService().search(pullOrdersAwaitingQueueQuery);
		return result.getResult();
	}

	public List<ConsignmentModel> pulledOrdersQueue()
	{
		final SearchResult<ConsignmentModel> result = getFlexibleSearchService().search(pulledOrdersQueueQuery);
		return result.getResult();
	}

	public FlexibleSearchService getFlexibleSearchService()
	{
		return flexibleSearchService;
	}


	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
	{
		this.flexibleSearchService = flexibleSearchService;
	}
}
