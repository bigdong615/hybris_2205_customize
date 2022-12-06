/**
 *
 */
package com.bl.core.services.domo.impl;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import com.bl.core.domo.BlDomoDao;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.domo.BlDomoService;

public class DefaultBlDomoService implements BlDomoService
{
	private BlDomoDao blDomoDao;

	@Override
	public SearchPageData<PackagingInfoModel> getPackagingInfos(final PageableData pageableData)
	{

		return getBlDomoDao().getPackagingInfos(pageableData);
	}

	@Override
	public SearchPageData<PaymentTransactionModel> getPaymentTransactions(final PageableData pageableData)
	{

		return getBlDomoDao().getPaymentTransactions(pageableData);
	}

	@Override
	public SearchPageData<PaymentTransactionEntryModel> getPaymentTransactionEntries(final PageableData pageableData)
	{

		return getBlDomoDao().getPaymentTransactionEntries(pageableData);
	}

	@Override
	public SearchPageData<GiftCardModel> getGiftCards(final PageableData pageableData)
	{
		return getBlDomoDao().getGiftCards(pageableData);
	}

	@Override
	public SearchPageData<GiftCardMovementModel> getGiftCardMovements(final PageableData pageableData)
	{
		return getBlDomoDao().getGiftCardMovements(pageableData);
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
