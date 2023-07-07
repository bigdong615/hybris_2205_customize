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

import java.time.Duration;
import java.time.Instant;
import java.util.Date;

import org.apache.log4j.Logger;

import com.bl.core.dao.warehouse.impl.DefaultBlConsignmentDao;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.facades.consignment.BLConsignmentFacade;


/**
 * @author
 *
 */
public class DefaultBLConsignmentFacade implements BLConsignmentFacade
{
	private static final Logger LOG = Logger.getLogger(DefaultBlConsignmentDao.class);

	private BlConsignmentEntryService blConsignmentEntryService;

	private Converter<ConsignmentEntryModel, ConsignmentEntryData> blDomoConsignmentEntryConverter;

	private Converter<ConsignmentModel, ConsignmentData> blDomoConsignmentConverter;

	@Override
	public SearchPageData<ConsignmentEntryData> getConsignmentEntries(final PageableData pageableData, final Date date)
	{
		final SearchPageData<ConsignmentEntryModel> entries = getBlConsignmentEntryService().getConsignmentEntries(pageableData,
				date);
		final Instant inst1 = Instant.now();
		LOG.info("Before calling consignmnet entries populator " + inst1);
		final SearchPageData<ConsignmentEntryData> consignmentEntries = convertPageData(entries,
				getBlDomoConsignmentEntryConverter());
		final Instant inst2 = Instant.now();
		LOG.info("after calling consignmnets populator " + inst2);
		LOG.info("Elapsed Time: " + Duration.between(inst1, inst2).toString());
		return consignmentEntries;
	}


	@Override
	public SearchPageData<ConsignmentData> getConsignments(final PageableData pageableData, final Date date)
	{

		final SearchPageData<ConsignmentModel> consignments = getBlConsignmentEntryService().getConsignments(pageableData, date);
		final Instant inst1 = Instant.now();
		LOG.info("Before calling consignmnets  populator" + inst1);
		final SearchPageData<ConsignmentData> cnmnts = convertConsignmentPageData(consignments, getBlDomoConsignmentConverter());
		final Instant inst2 = Instant.now();
		LOG.info("after calling consignmnets populator" + inst2);
		LOG.info("Elapsed Time: " + Duration.between(inst1, inst2).toString());
		return cnmnts;
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
	 * @return the blDomoConsignmentEntryConverter
	 */
	public Converter<ConsignmentEntryModel, ConsignmentEntryData> getBlDomoConsignmentEntryConverter()
	{
		return blDomoConsignmentEntryConverter;
	}
	
	/**
	 * @param blDomoConsignmentEntryConverter
	 *           the blDomoConsignmentEntryConverter to set
	 */
	public void setBlDomoConsignmentEntryConverter(
			final Converter<ConsignmentEntryModel, ConsignmentEntryData> blDomoConsignmentEntryConverter)
	{
		this.blDomoConsignmentEntryConverter = blDomoConsignmentEntryConverter;
	}

	/**
	 * @return the blDomoConsignmentConverter
	 */
	public Converter<ConsignmentModel, ConsignmentData> getBlDomoConsignmentConverter()
	{
		return blDomoConsignmentConverter;
	}

	/**
	 * @param blDomoConsignmentConverter
	 *           the blDomoConsignmentConverter to set
	 */
	public void setBlDomoConsignmentConverter(final Converter<ConsignmentModel, ConsignmentData> blDomoConsignmentConverter)
	{
		this.blDomoConsignmentConverter = blDomoConsignmentConverter;
	}

}
