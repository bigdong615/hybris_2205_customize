package com.bl.core.dao.warehouse;
import com.bl.core.model.BlStateWarehouseMappingModel;

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
   * @param stateCode - id of state.
   * @return StateWarehouseMapping object.
   */
  BlStateWarehouseMappingModel getStateWarehouseForStateCode(String stateCode);

}
