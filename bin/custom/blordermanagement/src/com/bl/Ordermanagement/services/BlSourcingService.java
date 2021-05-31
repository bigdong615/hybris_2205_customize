package com.bl.Ordermanagement.services;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import de.hybris.platform.warehousing.sourcing.SourcingService;

/**
 * This is used to source the order.
 * @author Moumita
 */
public interface BlSourcingService extends SourcingService {
  /**
   * This is to source the order
   *
   * @param order the order
   *
   * @return SourcingResults The SourcingResults
   */
  public SourcingResults sourceOrder(AbstractOrderModel order);
}
