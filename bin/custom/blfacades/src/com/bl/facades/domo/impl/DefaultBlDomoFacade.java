/**
 *
 */
package com.bl.facades.domo.impl;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.converters.Converters;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionEntryData;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import com.bl.core.services.domo.BlDomoService;
import com.bl.facades.domo.BlDomoFacade;

public class DefaultBlDomoFacade implements BlDomoFacade
{
	private BlDomoService blDomoService;

	private Converter<PackagingInfoModel, PackagingInfoData> blpackagingInfoConverter;

	private Converter<PaymentTransactionModel, PaymentTransactionData> blpaymentTransactionConverter;

	private Converter<PaymentTransactionEntryModel, PaymentTransactionEntryData> blpaymentTransactionEntryConverter;

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



}
