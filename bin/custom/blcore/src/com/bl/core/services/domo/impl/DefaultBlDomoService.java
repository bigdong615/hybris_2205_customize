/**
 *
 */
package com.bl.core.services.domo.impl;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import com.bl.core.domo.BlDomoDao;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlSerialLogModel;
import com.bl.core.model.CustomerNotesModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.InHouseRepairLogModel;
import com.bl.core.model.PartsNeededRepairLogModel;
import com.bl.core.model.VendorRepairLogModel;
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

	@Override
	public SearchPageData<OrderModel> getOrders(final PageableData pageableData)
	{
		return getBlDomoDao().getOrders(pageableData);
	}

	@Override
	public SearchPageData<OrderEntryModel> getOrderEntries(final PageableData pageableData)
	{
		return getBlDomoDao().getOrderEntries(pageableData);
	}

	@Override
	public SearchPageData<BlItemsBillingChargeModel> getBlItemsBillingCharge(final PageableData pageableData)
	{
		return getBlDomoDao().getBlItemsBillingCharge(pageableData);
	}

	@Override
	public SearchPageData<CustomerModel> getCustomers(final PageableData pageableData)
	{
		return getBlDomoDao().getCustomers(pageableData);
	}

	@Override
	public SearchPageData<BlSerialLogModel> getBlSerialLogs(final PageableData pageableData)
	{
		return getBlDomoDao().getBlSerialLogs(pageableData);
	}

	@Override
	public SearchPageData<CustomerNotesModel> getCustomerNotes(final PageableData pageableData)
	{
		return getBlDomoDao().getCustomerNotes(pageableData);
	}

	@Override
	public SearchPageData<VendorRepairLogModel> getVendorRepairLogs(final PageableData pageableData)
	{
		return getBlDomoDao().getVendorRepairLogs(pageableData);
	}

	@Override
	public SearchPageData<PartsNeededRepairLogModel> getPartsNeededRepairLogs(final PageableData pageableData)
	{
		return getBlDomoDao().getPartsNeededRepairLogs(pageableData);

	}

	@Override
	public SearchPageData<InHouseRepairLogModel> getInHouseRepairLogs(final PageableData pageableData)
	{
		return getBlDomoDao().getInHouseRepairLogs(pageableData);
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
