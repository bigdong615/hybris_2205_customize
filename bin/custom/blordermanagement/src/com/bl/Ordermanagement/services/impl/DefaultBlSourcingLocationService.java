package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.services.BlSourcingLocationService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.sourcing.context.populator.SourcingLocationPopulator;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Required;

public class DefaultBlSourcingLocationService implements BlSourcingLocationService {

  private Set<SourcingLocationPopulator> sourcingLocationPopulators;

  /**
   * {@inheritDoc}
   */
  @Override
  public SourcingLocation createSourcingLocation(SourcingContext context, WarehouseModel location) {
    SourcingLocation sourcingLocation = new SourcingLocation();
    sourcingLocation.setWarehouse(location);
    sourcingLocation.setContext(context);
    this.getSourcingLocationPopulators().forEach((populator) -> {
      populator.populate(location, sourcingLocation);
    });

    Set<SourcingLocation> sourcingLocations = (Set) context.getSourcingLocations();
    if (CollectionUtils.isNotEmpty(sourcingLocations)) {
      sourcingLocations.add(sourcingLocation);
      context.setSourcingLocations(sourcingLocations);
    } else {
      Set<SourcingLocation> newSourcingLocations =  new HashSet<SourcingLocation>();
      newSourcingLocations.add(sourcingLocation);
      context.setSourcingLocations(newSourcingLocations);
    }

    return sourcingLocation;
  }

  private Set<SourcingLocationPopulator> getSourcingLocationPopulators() {
    return this.sourcingLocationPopulators;
  }

  @Required
  public void setSourcingLocationPopulators(Set<SourcingLocationPopulator> populators) {
    this.sourcingLocationPopulators = populators;
  }
}
