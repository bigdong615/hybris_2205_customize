/**
 *
 */
package com.bl.facades.consignment;

import de.hybris.platform.commercefacades.order.data.ConsignmentData;
import de.hybris.platform.commercefacades.order.data.ConsignmentEntryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;

import java.util.Date;


public interface BLConsignmentFacade
{
	/**
	 * To fetch ConsignmentEntries for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of ConsignmentEntries
	 */
	SearchPageData<ConsignmentEntryData> getConsignmentEntries(final PageableData pageableData, final Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<ConsignmentData> getConsignments(PageableData pageableData, Date date);
}
