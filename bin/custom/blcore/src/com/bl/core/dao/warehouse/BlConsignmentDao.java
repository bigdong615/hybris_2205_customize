package com.bl.core.dao.warehouse;

import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import java.util.Date;
import java.util.List;

import com.bl.core.model.BlSerialProductModel;

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
  
  /**
   * Gets the consignment entries for serial code and from date.
   *
   * @param serial the serial
   * @param fromDate the from date
   * @return the consignment entries for serial code and date
   */
  List<ConsignmentEntryModel> getConsignmentEntriesForSerialCodeAndDate(final BlSerialProductModel serial, final Date fromDate);

  /**
	 * Gets the consignment entries for serial code.
	 *
	 * @param serial
	 *           the serial
	 * @return the consignment entries for serial code and date
	 */
	public ConsignmentModel getConsignmentForSerialCode(final String serialCode, final String orderCode);

}
