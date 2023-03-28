/**
 *
 */
package com.bl.core.service.invoice;

import de.hybris.platform.commercefacades.order.data.OrderData;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * @author Admin
 *
 */
public interface GenerateInvoicePdfService
{

	/**
	 * @param orderDetails
	 * @param request
	 * @param response
	 */
	void generateInvoicePdf(OrderData orderDetails, HttpServletRequest request, HttpServletResponse response);
}
