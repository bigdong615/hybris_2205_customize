/**
 *
 */
package com.bl.integration.dao.impl;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.bl.integration.dao.BlPrintingShippingLabelDao;


/**
 * @author Admin
 *
 */
public class DefaultBlPrintingShippingLabelDao implements BlPrintingShippingLabelDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlPrintingShippingLabelDao.class);

	@Autowired
	private FlexibleSearchService flexibleSearchService;

	@Override
	public ConsignmentModel getConsignmentByPk(final String code)
	{
		final String barcodeList = "SELECT {pk} FROM {Consignment} WHERE {pk} = ?code";
		final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
		query.addQueryParameter("code", code);
		final List<ConsignmentModel> results = getFlexibleSearchService().<ConsignmentModel> search(query).getResult();
		return CollectionUtils.isNotEmpty(results) ? results.get(0) : null;
	}

	/**
	 * @return the flexibleSearchService
	 */
	public FlexibleSearchService getFlexibleSearchService()
	{
		return flexibleSearchService;
	}

	/**
	 * @param flexibleSearchService
	 *           the flexibleSearchService to set
	 */
	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
	{
		this.flexibleSearchService = flexibleSearchService;
	}

}
