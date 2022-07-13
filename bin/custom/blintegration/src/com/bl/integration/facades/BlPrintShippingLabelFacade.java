/**
 *
 */
package com.bl.integration.facades;

import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import java.util.List;


/**
 * @author Aditi Sharma
 */
public interface BlPrintShippingLabelFacade
{
	/**
	 * This method is responsible to get the consignment by PK 
	 * @param code
	 * @return
	 */
	List<PackagingInfoData> getConsignmentByPk(final String code);
}
