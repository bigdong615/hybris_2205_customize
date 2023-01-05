/**
 *
 */
package com.bl.facades.consignment.impl;

import de.hybris.platform.commercefacades.order.data.ConsignmentData;
import de.hybris.platform.commercefacades.order.data.ConsignmentEntryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.converters.Converters;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.facades.consignment.BLConsignmentFacade;


/**
 * @author
 *
 */
public class DefaultBLConsignmentFacade implements BLConsignmentFacade
{
	private BlConsignmentEntryService blConsignmentEntryService;

	private Converter<ConsignmentEntryModel, ConsignmentEntryData> consignmentEntryConverter;

	private Converter<ConsignmentModel, ConsignmentData> consignmentConverter;

	@Override
	public SearchPageData<ConsignmentEntryData> getConsignmentEntries(final PageableData pageableData)
	{
		final SearchPageData<ConsignmentEntryModel> entries = getBlConsignmentEntryService().getConsignmentEntries(pageableData);
		return convertPageData(entries, getConsignmentEntryConverter());
	}

	@Override
	public SearchPageData<ConsignmentData> getConsignments(final PageableData pageableData)
	{
		final SearchPageData<ConsignmentModel> consignments = getBlConsignmentEntryService().getConsignments(pageableData);
		return convertConsignmentPageData(consignments, getConsignmentConverter());
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

	protected <S, T> SearchPageData<T> convertConsignmentPageData(final SearchPageData<S> source, final Converter<S, T> converter)
	{
		final SearchPageData<T> result = new SearchPageData<T>();
		result.setPagination(source.getPagination());
		result.setSorts(source.getSorts());
		result.setResults(Converters.convertAll(source.getResults(), converter));
		return result;
	}

	public BlConsignmentEntryService getBlConsignmentEntryService()
	{
		return blConsignmentEntryService;
	}

	public void setBlConsignmentEntryService(final BlConsignmentEntryService blConsignmentEntryService)
	{
		this.blConsignmentEntryService = blConsignmentEntryService;
	}

	/**
	 * @return the consignmentEntryConverter
	 */
	public Converter<ConsignmentEntryModel, ConsignmentEntryData> getConsignmentEntryConverter()
	{
		return consignmentEntryConverter;
	}

	/**
	 * @param consignmentEntryConverter
	 *           the consignmentEntryConverter to set
	 */
	public void setConsignmentEntryConverter(
			final Converter<ConsignmentEntryModel, ConsignmentEntryData> consignmentEntryConverter)
	{
		this.consignmentEntryConverter = consignmentEntryConverter;
	}

	/**
	 * @return the consignmentConverter
	 */
	public Converter<ConsignmentModel, ConsignmentData> getConsignmentConverter()
	{
		return consignmentConverter;
	}

	/**
	 * @param consignmentConverter
	 *           the consignmentConverter to set
	 */
	public void setConsignmentConverter(final Converter<ConsignmentModel, ConsignmentData> consignmentConverter)
	{
		this.consignmentConverter = consignmentConverter;
	}

}
