package com.bl.core.dao.warehouse.impl;

import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlStateWarehouseMappingDao;
import com.bl.core.model.BlStateWarehouseMappingModel;
import com.bl.core.model.BoxSizesModel;


/**
 * This class is created for State Warehouse Mapping.
 *
 * @author Sunil
 */
public class DefaultBlStateWarehouseMappingDao implements BlStateWarehouseMappingDao
{

	private FlexibleSearchService flexibleSearchService;
	private static final String FIND_BL_STATE_WAREHOUSE_BY_STATE_CODE = "SELECT {pk} FROM {"
			+ BlStateWarehouseMappingModel._TYPECODE + "} WHERE {" + BlStateWarehouseMappingModel.CODE
			+ "} IN ({{SELECT {reg:PK} FROM {Region as reg} WHERE {reg:isoCodeShort} =  ?isoCodeShort }})";

	private static final String FIND_BOX_DIMENSIONS = "SELECT {pk} FROM {" + BoxSizesModel._TYPECODE + "}";

	/**
	 * {@inheritDoc}
	 */
	@Override
	public BlStateWarehouseMappingModel getStateWarehouseForStateCode(final String isoCodeShort)
	{

		final FlexibleSearchQuery query = new FlexibleSearchQuery(FIND_BL_STATE_WAREHOUSE_BY_STATE_CODE);
		query.addQueryParameter(BlCoreConstants.ISO_CODE_SHORT, isoCodeShort);
		final SearchResult<BlStateWarehouseMappingModel> result = getFlexibleSearchService().search(query);
		if (null != result && CollectionUtils.isNotEmpty(result.getResult()))
		{
			return result.getResult().get(0);
		}

		return null;
	}


	@Override
	public List<BoxSizesModel> getBoxDimensions()
	{
		final FlexibleSearchQuery query = new FlexibleSearchQuery(FIND_BOX_DIMENSIONS);
		final SearchResult<BoxSizesModel> result = getFlexibleSearchService().search(query);
		return result.getResult();
	}

	public FlexibleSearchService getFlexibleSearchService()
	{
		return flexibleSearchService;
	}

	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
	{
		this.flexibleSearchService = flexibleSearchService;
	}


}
