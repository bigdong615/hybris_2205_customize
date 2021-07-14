/**
 *
 */
/**
 * @author admin
 *
 */
package com.bl.core.services.box;

import java.util.List;

import com.bl.core.model.BoxSizesModel;


public interface BoxDimensionService
{
	/**
	 * Get Box Sizes available in Database
	 * 
	 * @return All Package Box sizes
	 */
	public List<BoxSizesModel> getBoxDimestions();
}
