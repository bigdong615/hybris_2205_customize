/**
 *
 */
package com.bl.backoffice.consignment.service;


import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.util.List;
import java.util.Set;

import com.bl.backoffice.widget.controller.order.BlOrderEntryToCancelDto;


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

}
