package com.bl.Ordermanagement.services;

import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;

public interface BlSourcingLocationService {

  /**
   * This is to create and add sourcing location to SourcingContext
   *
   * @param context  the SourcingContext
   * @param location the WarehouseModel
   * @return sourcing location.
   */
  public SourcingLocation createSourcingLocation(SourcingContext context, WarehouseModel location);
}
