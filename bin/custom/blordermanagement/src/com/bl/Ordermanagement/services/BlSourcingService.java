package com.bl.Ordermanagement.services;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import de.hybris.platform.warehousing.sourcing.SourcingService;

/**
 * This is used to source the order.
 * @author Sunil
 */
public interface BlSourcingService extends SourcingService {
  /**
   * This is to source the order
   *
   * @param order the order
   * @param newOrderEntry the new order entry
   *
   * @return SourcingResults The SourcingResults
   */
  public SourcingResults sourceOrder(final AbstractOrderModel order, final AbstractOrderEntryModel newOrderEntry);

  /**
   * This method updates the actual rental start date for internal transfer cases.
   *
   * @param order the order
   * @param result the result
   */
  public void updateShippingDatesForInternalTransfers(final AbstractOrderModel order,
      final SourcingResults result);
}
