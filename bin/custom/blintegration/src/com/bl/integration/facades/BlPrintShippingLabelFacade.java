/**
 *
 */
package com.bl.integration.facades;

import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import java.util.List;


/**
 * @author Aditi
 *
 */
public interface BlPrintShippingLabelFacade
{
	List<PackagingInfoData> getConsignmentByPk(final String code);
}
