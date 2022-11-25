/**
 *
 */
package com.bl.core.domo.impl;

import de.hybris.platform.commerceservices.search.flexiblesearch.PagedFlexibleSearchService;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import com.bl.core.domo.BlDomoDao;


public class DefaultBlDomoDao implements BlDomoDao
{
	private PagedFlexibleSearchService pagedFlexibleSearchService;


	@Override
	public SearchPageData<PackagingInfoModel> getPackagingInfos(final PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {pi.pk} FROM {PackagingInfo as pi}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}

	public PagedFlexibleSearchService getPagedFlexibleSearchService()
	{
		return pagedFlexibleSearchService;
	}

	public void setPagedFlexibleSearchService(final PagedFlexibleSearchService pagedFlexibleSearchService)
	{
		this.pagedFlexibleSearchService = pagedFlexibleSearchService;
	}


}
