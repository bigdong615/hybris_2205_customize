/**
 *
 */
package com.bl.facades.domo.impl;

import de.hybris.platform.commercefacades.BlItemsBillingChargeData;
import de.hybris.platform.commercefacades.giftcard.data.GiftCardData;
import de.hybris.platform.commercefacades.giftcard.movement.data.GiftCardMovementData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.converters.Converters;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionEntryData;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlSerialLogModel;
import com.bl.core.model.CustomerNotesModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.InHouseRepairLogModel;
import com.bl.core.model.PartsNeededRepairLogModel;
import com.bl.core.model.VendorRepairLogModel;
import com.bl.core.services.domo.BlDomoService;
import com.bl.facades.blSerialLog.data.BlSerialLogData;
import com.bl.facades.customerNotes.data.CustomerNotesData;
import com.bl.facades.domo.BlDomoFacade;
import com.bl.facades.inHouseRepairLog.data.InHouseRepairLogData;
import com.bl.facades.partsNeededRepairLog.data.PartsNeededRepairLogData;
import com.bl.facades.vendorRepairLog.data.VendorRepairLogData;

public class DefaultBlDomoFacade implements BlDomoFacade
{
	private BlDomoService blDomoService;

	private Converter<PackagingInfoModel, PackagingInfoData> blpackagingInfoConverter;

	private Converter<PaymentTransactionModel, PaymentTransactionData> blpaymentTransactionConverter;

	private Converter<PaymentTransactionEntryModel, PaymentTransactionEntryData> blpaymentTransactionEntryConverter;

	private Converter<GiftCardModel, GiftCardData> blGiftCardConverter;

	private Converter<GiftCardMovementModel, GiftCardMovementData> blGiftCardMovementConverter;

	private Converter<OrderModel, OrderData> blDomoOrderConverter;

	private Converter<OrderEntryModel, OrderEntryData> blorderEntryConverter;

	private Converter<BlItemsBillingChargeModel, BlItemsBillingChargeData> blItemsBillingChargeConverter;

	private Converter<CustomerModel, CustomerData> blCustomerConverter;

	private Converter<BlSerialLogModel, BlSerialLogData> blSerialLogsConverter;

	private Converter<CustomerNotesModel, CustomerNotesData> blCustomerNotesConverter;

	private Converter<VendorRepairLogModel, VendorRepairLogData> blVendorRepairLogConverter;

	private Converter<PartsNeededRepairLogModel, PartsNeededRepairLogData> blPartsNeededRepairLogConverter;

	private Converter<InHouseRepairLogModel, InHouseRepairLogData> blInHouseRepairLogConverter;


	@Override
	public SearchPageData<PackagingInfoData> getPackagingInfos(final PageableData pageableData)
	{
		final SearchPageData<PackagingInfoModel> packaginginfos = getBlDomoService().getPackagingInfos(pageableData);
		return convertPageData(packaginginfos, getBlpackagingInfoConverter());
	}

	/*
	 * Method to convert Page data
	 */
	protected <S, T> SearchPageData<T> convertPageData(final SearchPageData<S> source, final Converter<S, T> converter)
	{
		final SearchPageData<T> result = new SearchPageData<T>();
		result.setPagination(source.getPagination());
		result.setSorts(source.getSorts());
		result.setResults(Converters.convertAll(source.getResults(), converter));
		return result;
	}

	@Override
	public SearchPageData<PaymentTransactionData> getPaymentTransactions(final PageableData pageableData)
	{
		final SearchPageData<PaymentTransactionModel> paymentTransactions = getBlDomoService().getPaymentTransactions(pageableData);
		return convertPageData(paymentTransactions, getBlpaymentTransactionConverter());
	}

	@Override
	public SearchPageData<PaymentTransactionEntryData> getPaymentTransactionEntries(final PageableData pageableData)
	{
		final SearchPageData<PaymentTransactionEntryModel> paymentTransactionEntries = getBlDomoService()
				.getPaymentTransactionEntries(pageableData);
		return convertPageData(paymentTransactionEntries, getBlpaymentTransactionEntryConverter());
	}

	@Override
	public SearchPageData<GiftCardData> getGiftCards(final PageableData pageableData)
	{
		final SearchPageData<GiftCardModel> giftCards = getBlDomoService().getGiftCards(pageableData);
		return convertPageData(giftCards, getBlGiftCardConverter());
	}

	@Override
	public SearchPageData<GiftCardMovementData> getGiftCardMovements(final PageableData pageableData)
	{
		final SearchPageData<GiftCardMovementModel> giftCardMovements = getBlDomoService().getGiftCardMovements(pageableData);
		return convertPageData(giftCardMovements, getBlGiftCardMovementConverter());
	}

	@Override
	public SearchPageData<OrderData> getOrders(final PageableData pageableData)
	{
		final SearchPageData<OrderModel> orders = getBlDomoService().getOrders(pageableData);
		return convertPageData(orders, getBlDomoOrderConverter());
	}

	@Override
	public SearchPageData<OrderEntryData> getOrderEntries(final PageableData pageableData)
	{
		final SearchPageData<OrderEntryModel> orderEntries = getBlDomoService().getOrderEntries(pageableData);
		return convertPageData(orderEntries, getBlorderEntryConverter());
	}

	@Override
	public SearchPageData<BlItemsBillingChargeData> getBlItemsBillingCharge(final PageableData pageableData)
	{
		final SearchPageData<BlItemsBillingChargeModel> blItemsBillingCharges = getBlDomoService()
				.getBlItemsBillingCharge(pageableData);
		return convertPageData(blItemsBillingCharges, getBlItemsBillingChargeConverter());
	}

	@Override
	public SearchPageData<CustomerData> getCustomers(final PageableData pageableData)
	{
		final SearchPageData<CustomerModel> customers = getBlDomoService().getCustomers(pageableData);
		return convertPageData(customers, getBlCustomerConverter());
	}

	@Override
	public SearchPageData<BlSerialLogData> getBlSerialLogs(final PageableData pageableData)
	{
		final SearchPageData<BlSerialLogModel> seriallogs = getBlDomoService().getBlSerialLogs(pageableData);
		return convertPageData(seriallogs, getBlSerialLogsConverter());
	}

	@Override
	public SearchPageData<CustomerNotesData> getCustomerNotes(final PageableData pageableData)
	{
		final SearchPageData<CustomerNotesModel> customerNotes = getBlDomoService().getCustomerNotes(pageableData);
		return convertPageData(customerNotes, getBlCustomerNotesConverter());
	}

	@Override
	public SearchPageData<VendorRepairLogData> getVendorRepairLogs(final PageableData pageableData)
	{
		final SearchPageData<VendorRepairLogModel> vendorRepairLogs = getBlDomoService().getVendorRepairLogs(pageableData);
		return convertPageData(vendorRepairLogs, getBlVendorRepairLogConverter());
	}

	@Override
	public SearchPageData<PartsNeededRepairLogData> getPartsNeededRepairLogs(final PageableData pageableData)
	{
		final SearchPageData<PartsNeededRepairLogModel> partsNeededRepairLogs = getBlDomoService()
				.getPartsNeededRepairLogs(pageableData);
		return convertPageData(partsNeededRepairLogs, getBlPartsNeededRepairLogConverter());
	}

	@Override
	public SearchPageData<InHouseRepairLogData> getInHouseRepairLogs(final PageableData pageableData)
	{
		final SearchPageData<InHouseRepairLogModel> inHouseRepairLogs = getBlDomoService().getInHouseRepairLogs(pageableData);
		return convertPageData(inHouseRepairLogs, getBlInHouseRepairLogConverter());
	}


	/**
	 * @return the blSerialLogsConverter
	 */
	public Converter<BlSerialLogModel, BlSerialLogData> getBlSerialLogsConverter()
	{
		return blSerialLogsConverter;
	}

	/**
	 * @param blSerialLogsConverter
	 *           the blSerialLogsConverter to set
	 */
	public void setBlSerialLogsConverter(final Converter<BlSerialLogModel, BlSerialLogData> blSerialLogsConverter)
	{
		this.blSerialLogsConverter = blSerialLogsConverter;
	}

	/**
	 * @return the blCustomerNotesConverter
	 */
	public Converter<CustomerNotesModel, CustomerNotesData> getBlCustomerNotesConverter()
	{
		return blCustomerNotesConverter;
	}

	/**
	 * @param blCustomerNotesConverter
	 *           the blCustomerNotesConverter to set
	 */
	public void setBlCustomerNotesConverter(final Converter<CustomerNotesModel, CustomerNotesData> blCustomerNotesConverter)
	{
		this.blCustomerNotesConverter = blCustomerNotesConverter;
	}

	/**
	 * @return the blVendorRepairLogConverter
	 */
	public Converter<VendorRepairLogModel, VendorRepairLogData> getBlVendorRepairLogConverter()
	{
		return blVendorRepairLogConverter;
	}

	/**
	 * @param blVendorRepairLogConverter
	 *           the blVendorRepairLogConverter to set
	 */
	public void setBlVendorRepairLogConverter(
			final Converter<VendorRepairLogModel, VendorRepairLogData> blVendorRepairLogConverter)
	{
		this.blVendorRepairLogConverter = blVendorRepairLogConverter;
	}

	/**
	 * @return the blPartsNeededRepairLogConverter
	 */
	public Converter<PartsNeededRepairLogModel, PartsNeededRepairLogData> getBlPartsNeededRepairLogConverter()
	{
		return blPartsNeededRepairLogConverter;
	}

	/**
	 * @param blPartsNeededRepairLogConverter
	 *           the blPartsNeededRepairLogConverter to set
	 */
	public void setBlPartsNeededRepairLogConverter(
			final Converter<PartsNeededRepairLogModel, PartsNeededRepairLogData> blPartsNeededRepairLogConverter)
	{
		this.blPartsNeededRepairLogConverter = blPartsNeededRepairLogConverter;
	}

	/**
	 * @return the blInHouseRepairLogConverter
	 */
	public Converter<InHouseRepairLogModel, InHouseRepairLogData> getBlInHouseRepairLogConverter()
	{
		return blInHouseRepairLogConverter;
	}

	/**
	 * @param blInHouseRepairLogConverter
	 *           the blInHouseRepairLogConverter to set
	 */
	public void setBlInHouseRepairLogConverter(
			final Converter<InHouseRepairLogModel, InHouseRepairLogData> blInHouseRepairLogConverter)
	{
		this.blInHouseRepairLogConverter = blInHouseRepairLogConverter;
	}


	/**
	 * @return the blorderEntryConverter
	 */
	public Converter<OrderEntryModel, OrderEntryData> getBlorderEntryConverter()
	{
		return blorderEntryConverter;
	}

	/**
	 * @param blorderEntryConverter
	 *           the blorderEntryConverter to set
	 */
	public void setBlorderEntryConverter(final Converter<OrderEntryModel, OrderEntryData> blorderEntryConverter)
	{
		this.blorderEntryConverter = blorderEntryConverter;
	}


	/**
	 * @return the BlDomoOrderConverter
	 */
	public Converter<OrderModel, OrderData> getBlDomoOrderConverter()
	{
		return blDomoOrderConverter;
	}

	/**
	 * @param BlDomoOrderConverter
	 *           the BlDomoOrderConverter to set
	 */
	public void setBlDomoOrderConverter(final Converter<OrderModel, OrderData> blDomoOrderConverter)
	{
		this.blDomoOrderConverter = blDomoOrderConverter;
	}

	/**
	 * @return the blpaymentTransactionConverter
	 */
	public Converter<PaymentTransactionModel, PaymentTransactionData> getBlpaymentTransactionConverter()
	{
		return blpaymentTransactionConverter;
	}

	/**
	 * @param blpaymentTransactionConverter
	 *           the blpaymentTransactionConverter to set
	 */
	public void setBlpaymentTransactionConverter(
			final Converter<PaymentTransactionModel, PaymentTransactionData> blpaymentTransactionConverter)
	{
		this.blpaymentTransactionConverter = blpaymentTransactionConverter;
	}

	/**
	 * @return the blpaymentTransactionEntryConverter
	 */
	public Converter<PaymentTransactionEntryModel, PaymentTransactionEntryData> getBlpaymentTransactionEntryConverter()
	{
		return blpaymentTransactionEntryConverter;
	}

	/**
	 * @param blpaymentTransactionEntryConverter
	 *           the blpaymentTransactionEntryConverter to set
	 */
	public void setBlpaymentTransactionEntryConverter(
			final Converter<PaymentTransactionEntryModel, PaymentTransactionEntryData> blpaymentTransactionEntryConverter)
	{
		this.blpaymentTransactionEntryConverter = blpaymentTransactionEntryConverter;
	}


	/**
	 * @return the blpackagingInfoConverter
	 */
	public Converter<PackagingInfoModel, PackagingInfoData> getBlpackagingInfoConverter()
	{
		return blpackagingInfoConverter;
	}

	/**
	 * @param blpackagingInfoConverter
	 *           the blpackagingInfoConverter to set
	 */
	public void setBlpackagingInfoConverter(final Converter<PackagingInfoModel, PackagingInfoData> blpackagingInfoConverter)
	{
		this.blpackagingInfoConverter = blpackagingInfoConverter;
	}

	/**
	 * @return the blDomoService
	 */
	public BlDomoService getBlDomoService()
	{
		return blDomoService;
	}

	/**
	 * @param blDomoService
	 *           the blDomoService to set
	 */
	public void setBlDomoService(final BlDomoService blDomoService)
	{
		this.blDomoService = blDomoService;
	}

	/**
	 * @return the blGiftCardConverter
	 */
	public Converter<GiftCardModel, GiftCardData> getBlGiftCardConverter()
	{
		return blGiftCardConverter;
	}

	/**
	 * @param blGiftCardConverter the blGiftCardConverter to set
	 */
	public void setBlGiftCardConverter(final Converter<GiftCardModel, GiftCardData> blGiftCardConverter)
	{
		this.blGiftCardConverter = blGiftCardConverter;
	}

	/**
	 * @return the blGiftCardMovementConverter
	 */
	public Converter<GiftCardMovementModel, GiftCardMovementData> getBlGiftCardMovementConverter()
	{
		return blGiftCardMovementConverter;
	}

	/**
	 * @param blGiftCardMovementConverter the blGiftCardMovementConverter to set
	 */
	public void setBlGiftCardMovementConverter(final Converter<GiftCardMovementModel, GiftCardMovementData> blGiftCardMovementConverter)
	{
		this.blGiftCardMovementConverter = blGiftCardMovementConverter;
	}

	/**
	 * @return the blItemsBillingChargeConverter
	 */
	public Converter<BlItemsBillingChargeModel, BlItemsBillingChargeData> getBlItemsBillingChargeConverter()
	{
		return blItemsBillingChargeConverter;
	}

	/**
	 * @param blItemsBillingChargeConverter
	 *           the blItemsBillingChargeConverter to set
	 */
	public void setBlItemsBillingChargeConverter(
			final Converter<BlItemsBillingChargeModel, BlItemsBillingChargeData> blItemsBillingChargeConverter)
	{
		this.blItemsBillingChargeConverter = blItemsBillingChargeConverter;
	}

	/**
	 * @return the blCustomerConverter
	 */
	public Converter<CustomerModel, CustomerData> getBlCustomerConverter()
	{
		return blCustomerConverter;
	}

	/**
	 * @param blCustomerConverter
	 *           the blCustomerConverter to set
	 */
	public void setBlCustomerConverter(final Converter<CustomerModel, CustomerData> blCustomerConverter)
	{
		this.blCustomerConverter = blCustomerConverter;
	}

}
