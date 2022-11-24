/**
 *
 */
package com.bl.facades.domo;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;


/**
 * @author ravi
 *
 */
public interface BlDomoFacade
{
	/**
	 * To fetch PackagingInfos for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of PackagingInfos
	 */
	SearchPageData<PackagingInfoData> getPackagingInfos(final PageableData pageableData);
}
