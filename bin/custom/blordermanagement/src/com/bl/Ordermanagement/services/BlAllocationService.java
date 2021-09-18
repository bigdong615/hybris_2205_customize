package com.bl.Ordermanagement.services;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.util.Set;

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
   * @param result the sourcing result
   * @return consignment entry
   */
  public ConsignmentEntryModel createConsignmentEntry(final AbstractOrderEntryModel orderEntry,
      Long quantity, final ConsignmentModel consignment, final SourcingResult result);

  /**
   * It sets seral codes to billing charges
   * @param consignmentEntry the consignment entry
   * @param serialProductModels the serialproduct instances
   */
  public void setSerialCodesToBillingCharges(final ConsignmentEntryModel consignmentEntry,
      final Set<BlSerialProductModel> serialProductModels);
}
