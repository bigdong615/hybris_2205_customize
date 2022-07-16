/**
 *
 */
package com.bl.integration.dao;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

/**
 * @author Aditi
 *
 */
public interface BlPrintingShippingLabelDao
{
	/**
	 * This method is used to get the consignment by PK
	 * @param code
	 * @return
	 */
	ConsignmentModel getConsignmentByPk(final String code);
}
