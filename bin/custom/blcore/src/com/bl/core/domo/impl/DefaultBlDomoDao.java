/**
 *
 */
package com.bl.core.domo.impl;

import de.hybris.platform.commerceservices.search.flexiblesearch.PagedFlexibleSearchService;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import com.bl.core.domo.BlDomoDao;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;


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

	@Override
	public SearchPageData<PaymentTransactionModel> getPaymentTransactions(final PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {p.pk} FROM {PaymentTransaction as p}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}

	@Override
	public SearchPageData<PaymentTransactionEntryModel> getPaymentTransactionEntries(final PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {p.pk} FROM {PaymentTransactionEntry as p}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}

	@Override
	public SearchPageData<GiftCardModel> getGiftCards(final PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {g.pk} FROM {GiftCard as g}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}

	@Override
	public SearchPageData<GiftCardMovementModel> getGiftCardMovements(final PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {gp.pk} FROM {GiftCardMovement as gp}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}

	@Override
	public SearchPageData<OrderModel> getOrders(final PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {o.pk} FROM {Order as o}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}

	@Override
	public SearchPageData<OrderEntryModel> getOrderEntries(final PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {o.pk} FROM {OrderEntry as o}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}
	
	@Override
	public SearchPageData<BlItemsBillingChargeModel> getBlItemsBillingCharge(PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {b.pk} FROM {BlItemsBillingCharge as b}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}

	@Override
	public SearchPageData<CustomerModel> getCustomers(PageableData pageableData)
	{
		final FlexibleSearchQuery fQ = new FlexibleSearchQuery("SELECT distinct {c.pk} FROM {Customer as c}");
		return getPagedFlexibleSearchService().search(fQ, pageableData);
	}

}
