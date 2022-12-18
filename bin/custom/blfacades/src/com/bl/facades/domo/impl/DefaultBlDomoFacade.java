/**
 *
 */
package com.bl.facades.domo.impl;

import de.hybris.platform.commercefacades.giftcard.data.GiftCardData;
import de.hybris.platform.commercefacades.giftcard.movement.data.GiftCardMovementData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.converters.Converters;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionEntryData;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.domo.BlDomoService;
import com.bl.facades.domo.BlDomoFacade;

public class DefaultBlDomoFacade implements BlDomoFacade
{
	private BlDomoService blDomoService;

	private Converter<PackagingInfoModel, PackagingInfoData> blpackagingInfoConverter;

	private Converter<PaymentTransactionModel, PaymentTransactionData> blpaymentTransactionConverter;

	private Converter<PaymentTransactionEntryModel, PaymentTransactionEntryData> blpaymentTransactionEntryConverter;

	private Converter<GiftCardModel, GiftCardData> blGiftCardConverter;

	private Converter<GiftCardMovementModel, GiftCardMovementData> blGiftCardMovementConverter;

	private Converter<OrderModel, OrderData> blDomoOrderConverter;

	private Converter<OrderEntryModel, OrderEntryData> orderEntryConverter;

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
		return convertPageData(orderEntries, getOrderEntryConverter());
	}

	/**
	 * @return the orderEntryConverter
	 */
	public Converter<OrderEntryModel, OrderEntryData> getOrderEntryConverter()
	{
		return orderEntryConverter;
	}

	/**
	 * @param orderEntryConverter
	 *           the orderEntryConverter to set
	 */
	public void setOrderEntryConverter(final Converter<OrderEntryModel, OrderEntryData> orderEntryConverter)
	{
		this.orderEntryConverter = orderEntryConverter;
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

}
