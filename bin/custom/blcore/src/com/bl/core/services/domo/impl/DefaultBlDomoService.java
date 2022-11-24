/**
 *
 */
package com.bl.core.services.domo.impl;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import com.bl.core.domo.BlDomoDao;
import com.bl.core.services.domo.BlDomoService;


/**
 * @author ravi
 *
 */
public class DefaultBlDomoService implements BlDomoService
{
	private BlDomoDao blDomoDao;

	@Override
	public SearchPageData<PackagingInfoModel> getPackagingInfos(final PageableData pageableData)
	{

		return getBlDomoDao().getPackagingInfos(pageableData);
	}

	/**
	 * @return the blDomoDao
	 */
	public BlDomoDao getBlDomoDao()
	{
		return blDomoDao;
	}

	/**
	 * @param blDomoDao
	 *           the blDomoDao to set
	 */
	public void setBlDomoDao(final BlDomoDao blDomoDao)
	{
		this.blDomoDao = blDomoDao;
	}



}
