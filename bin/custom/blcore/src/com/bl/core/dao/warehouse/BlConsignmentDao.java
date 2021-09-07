package com.bl.core.dao.warehouse;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import java.util.Date;
import java.util.List;

/**
 * It is used to get consignments.
 *
 * @author Sunil
 */
public interface BlConsignmentDao {

  /**
   * Get consignments
   * @param shipDate
   * @return ConsignmentModels
   */
  List<ConsignmentModel> getReadyToShipConsignmentsForDate(final Date shipDate);

  /**
   * Gets the consignment for return date.
   *
   * @param returnDate the return date
   * @return the consignment for return date
   */
  List<ConsignmentModel> getConsignmentForReturnDate(final Date returnDate);

}
