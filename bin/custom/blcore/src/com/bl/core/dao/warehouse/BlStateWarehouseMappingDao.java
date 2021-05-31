package com.bl.core.dao.warehouse;
import com.bl.core.model.BlStateWarehouseMappingModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
/**
 * This interface is
 * created for State Warehouse Mapping.
 *
 * @author Sunil
 */
public interface BlStateWarehouseMappingDao {
    /**
     * Get StateWarehouse for given state id.
     *
     * @param stateId - id of state.
     * @return StateWarehouseMapping object.
     */
   BlStateWarehouseMappingModel getStateWarehouseForStateCode(String stateCode);

}
