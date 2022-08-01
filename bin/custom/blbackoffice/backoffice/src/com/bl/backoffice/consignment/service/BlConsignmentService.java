/**
 *
 */
package com.bl.backoffice.consignment.service;


import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.util.List;
import java.util.Set;

import com.bl.backoffice.widget.controller.order.BlOrderEntryToCancelDto;
import com.bl.core.model.BlSerialProductModel;


/**
 * Interface is created to update the stock for cancelled order
 * @author Aditi Sharma
 *
 */
public interface BlConsignmentService
{
	/**
	 * method will be used to update stock for partially cancelled order
	 *
	 * @param consignments
	 *           as consignment
	 * @param cancelAndRefundEntries as Cancel and Refund Entries
	 */
	public void updateStockForPartialCancelledOrder(final Set<ConsignmentModel> consignments,
			List<BlOrderEntryToCancelDto> cancelAndRefundEntries);

	/**
	 * method will be updated to update stock for cancelled product
	 *
	 * @param consignments
	 *           as Consignment
	 */
	public void updateStockForCancelledOrder(final Set<ConsignmentModel> consignments);
	
	/**
	 * Checks if main item scan remaining in the consignment.
	 *
	 * @param consignment the consignment
	 * @return true, if is main item scan remaining
	 */
	public boolean isMainItemScanRemaining(final ConsignmentModel consignment);
	
	/**
	 * Checks if is subpart scan remaining in the consignment.
	 *
	 * @param consignment the consignment
	 * @return true, if is subpart scan remaining
	 */
	public boolean isSubpartScanRemaining(final ConsignmentModel consignment);
	
	/**
	 * Gets the remaining scan subpart names list.
	 *
	 * @param consignment the consignment
	 * @return the remaining scan subpart names
	 */
	public List<String> getRemainingScanSubpartNames(final ConsignmentModel consignment);
	
	/**
	 * Gets the main items list from consignment.
	 *
	 * @param consignment the consignment
	 * @return the main items list from consignment
	 */
	public List<BlSerialProductModel> getMainItemsListFromConsignment(final ConsignmentModel consignment);

}
