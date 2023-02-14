/**
 *
 */
package com.bl.core.services.domo.impl;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.Date;

import com.bl.core.domo.BlDomoDao;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlSerialLogModel;
import com.bl.core.model.CustomerNotesModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.InHouseRepairLogModel;
import com.bl.core.model.NotesModel;
import com.bl.core.model.PartsNeededRepairLogModel;
import com.bl.core.model.VendorRepairLogModel;
import com.bl.core.services.domo.BlDomoService;
import com.braintree.model.BrainTreePaymentInfoModel;


public class DefaultBlDomoService implements BlDomoService
{
	private BlDomoDao blDomoDao;

	@Override
	public SearchPageData<PackagingInfoModel> getPackagingInfos(final PageableData pageableData, final Date date)
	{

		return getBlDomoDao().getPackagingInfos(pageableData, date);
	}

	@Override
	public SearchPageData<PaymentTransactionModel> getPaymentTransactions(final PageableData pageableData, final Date date)
	{

		return getBlDomoDao().getPaymentTransactions(pageableData, date);
	}

	@Override
	public SearchPageData<PaymentTransactionEntryModel> getPaymentTransactionEntries(final PageableData pageableData,
			final Date date)
	{

		return getBlDomoDao().getPaymentTransactionEntries(pageableData, date);
	}

	@Override
	public SearchPageData<GiftCardModel> getGiftCards(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getGiftCards(pageableData, date);
	}

	@Override
	public SearchPageData<GiftCardMovementModel> getGiftCardMovements(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getGiftCardMovements(pageableData, date);
	}

	@Override
	public SearchPageData<OrderModel> getOrders(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getOrders(pageableData, date);
	}

	@Override
	public SearchPageData<OrderEntryModel> getOrderEntries(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getOrderEntries(pageableData, date);
	}

	@Override
	public SearchPageData<BlItemsBillingChargeModel> getBlItemsBillingCharge(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getBlItemsBillingCharge(pageableData, date);
	}

	@Override
	public SearchPageData<CustomerModel> getCustomers(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getCustomers(pageableData, date);
	}

	@Override
	public SearchPageData<BlSerialLogModel> getBlSerialLogs(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getBlSerialLogs(pageableData, date);
	}

	@Override
	public SearchPageData<CustomerNotesModel> getCustomerNotes(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getCustomerNotes(pageableData, date);
	}

	@Override
	public SearchPageData<VendorRepairLogModel> getVendorRepairLogs(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getVendorRepairLogs(pageableData, date);
	}

	@Override
	public SearchPageData<PartsNeededRepairLogModel> getPartsNeededRepairLogs(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getPartsNeededRepairLogs(pageableData, date);

	}

	@Override
	public SearchPageData<InHouseRepairLogModel> getInHouseRepairLogs(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getInHouseRepairLogs(pageableData, date);
	}

	@Override
	public SearchPageData<AddressModel> getAddresses(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getAddresses(pageableData, date);
	}

	@Override
	public SearchPageData<BrainTreePaymentInfoModel> getBrainTreePaymentInfos(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getBrainTreePaymentInfos(pageableData, date);
	}

	@Override
	public SearchPageData<StockLevelModel> getStockLevels(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getStockLevels(pageableData, date);
	}

	@Override
	public SearchPageData<NotesModel> getNotes(final PageableData pageableData, final Date date)
	{
		return getBlDomoDao().getNotes(pageableData, date);
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
