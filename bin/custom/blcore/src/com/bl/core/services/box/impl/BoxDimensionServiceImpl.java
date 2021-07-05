/**
 *
 */
/**
 * @author admin
 *
 */
package com.bl.core.services.box.impl;

import java.util.List;

import com.bl.core.dao.warehouse.BlStateWarehouseMappingDao;
import com.bl.core.model.BoxSizesModel;
import com.bl.core.services.box.BoxDimensionService;


public class BoxDimensionServiceImpl implements BoxDimensionService
{
	private BlStateWarehouseMappingDao blStateWarehouseMappingDao;

	public List<BoxSizesModel> getBoxDimestions()
	{
		return getBlStateWarehouseMappingDao().getBoxDimensions();
	}

	/**
	 * @return the blStateWarehouseMappingDao
	 */
	public BlStateWarehouseMappingDao getBlStateWarehouseMappingDao()
	{
		return blStateWarehouseMappingDao;
	}

	/**
	 * @param blStateWarehouseMappingDao
	 *           the blStateWarehouseMappingDao to set
	 */
	public void setBlStateWarehouseMappingDao(final BlStateWarehouseMappingDao blStateWarehouseMappingDao)
	{
		this.blStateWarehouseMappingDao = blStateWarehouseMappingDao;
	}
}
