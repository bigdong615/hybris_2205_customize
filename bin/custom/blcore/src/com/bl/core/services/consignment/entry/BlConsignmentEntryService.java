package com.bl.core.services.consignment.entry;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;

import java.util.Set;

import com.bl.core.model.BlSerialProductModel;


/**
 * This service interface is use to perform custom bussiness logic on consignment entry or consignment
 *
 * @author Ravikumar
 *
 */
public interface BlConsignmentEntryService
{

	/**
	 * Removes the serial from future consignment entry and mark Consignment and Order status to MANUAL_REVIEW.
	 *
	 * @param blSerialProductModel
	 *           the bl serial product model
	 */
	void removeSerialFromFutureConsignmentEntry(final BlSerialProductModel blSerialProductModel);

	/**
	 * Created Map to display Shipper what all items are attached to the consignment. So that agent can verify and scan
	 * the serial. Sub-parts and serials both will be added to this Map. During Sub-parts scanning, please replace
	 * sub-part name with sub-part serial code ex: BEFORE SCANNING ---> 54356 NOT_INCLUDED 46363 NOT_INCLUDED Lens Hood-1
	 * NOT_INCLUDED (Sub-parts Name associated) Lens Hood-2 NOT_INCLUDED (Sub-parts Name associated) Battery NOT_INCLUDED
	 * (Sub-parts Name associated)
	 *
	 * AFTER SCANNING ---> 54356 INCLUDED 46363 INCLUDED GHDKD INCLUDED (Sub-parts Serial associated) EGDBD INCLUDED
	 * (Sub-parts Serial associated) Battery INCLUDED (This Sub-parts has no barcode, So manually INCLUDED by shipper)
	 *
	 * @param entry
	 *           the entry
	 * @param serialProductModels
	 *           the serial product models
	 */
	void setItemsMap(final ConsignmentEntryModel entry, final Set<BlSerialProductModel> serialProductModels);

	/**
	 * Created Map to display Shipper what all items are attached to the consignment.
	 * @param entry
	 *           the entry
	 * @param orderEntry
	 *           the serial product models
	 */
	void setItemsMapForInternalTransferOrders(final ConsignmentEntryModel entry, final AbstractOrderEntryModel orderEntry);
	
	/**
	 * Assign serial and order code on billing charges.
	 *
	 * @param consignmentEntryModel the consignment entry model
	 */
	void assignSerialAndOrderCodeOnBillingCharges(final ConsignmentEntryModel consignmentEntryModel);
	
	/**
	 * Gets the consignment entry from order for serial code.
	 *
	 * @param order the order
	 * @param serialCode the serial code
	 * @return the consignment entry from order for serial
	 */
	ConsignmentEntryModel getConsignmentEntryFromOrderForSerial(final OrderModel order, final String serialCode);
}
