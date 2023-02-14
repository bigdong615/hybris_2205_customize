package com.bl.core.dao.warehouse;

import java.util.List;

import com.bl.core.model.BlStateWarehouseMappingModel;
import com.bl.core.model.BoxSizesModel;
import com.bl.core.model.ShippingOptimizationModel;


/**
 * This interface is created for State Warehouse Mapping.
 *
 * @author Sunil
 */
public interface BlStateWarehouseMappingDao
{

	/**
	 * Get StateWarehouse for given state id.
	 *
	 * @param stateCode
	 *           - id of state.
	 * @return StateWarehouseMapping object.
	 */
	BlStateWarehouseMappingModel getStateWarehouseForStateCode(final String stateCode);

	/**
	 * Get all the Box Dimensions
	 *
	 * @return
	 */
	List<BoxSizesModel> getBoxDimensions();


	List<ShippingOptimizationModel> getWarehouseForPostalCode(final String postalCode, final Integer carrierID);

}
