package com.bl.core.services.consignment.entry;

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
	 * Removes the serial from consignment entry.
	 *
	 * @param blSerialProductModel
	 *           the bl serial product model
	 */
	void removeSerialFromConsignmentEntry(final BlSerialProductModel blSerialProductModel);
}
