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
	ConsignmentModel getConsignmentByPk(final String code);
}
