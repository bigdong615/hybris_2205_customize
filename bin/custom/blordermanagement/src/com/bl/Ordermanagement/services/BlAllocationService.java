package com.bl.Ordermanagement.services;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;

/**
 * Allocation service.
 *
 * @author Sunil
 */
public interface BlAllocationService extends AllocationService {

  /**
   * It creates consignment entry
   * @param orderEntry the order entry
   * @param quantity the quantity
   * @param consignment the consignment
   * @param result
   * @return consignment entry
   */
  public ConsignmentEntryModel createConsignmentEntry(final AbstractOrderEntryModel orderEntry,
      Long quantity, final ConsignmentModel consignment, final SourcingResult result);
}
