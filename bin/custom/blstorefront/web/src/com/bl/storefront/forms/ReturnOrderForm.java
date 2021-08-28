package com.bl.storefront.forms;

import java.io.Serializable;
import java.util.List;


/**
 * Return order form
 */
public class ReturnOrderForm implements Serializable {

	private static final long serialVersionUID = 1L;

	private String orderCode;
	//private Map<String, Integer> product;

	private List<String> returnOrderEntries;


	/**
	 * @return the orderCode
	 */
	public String getOrderCode()
	{
		return orderCode;
	}

	/**
	 * @param orderCode
	 *           the orderCode to set
	 */
	public void setOrderCode(final String orderCode)
	{
		this.orderCode = orderCode;
	}

	/**
	 * @return the returnOrderEntries
	 */
	public List<String> getReturnOrderEntries()
	{
		return returnOrderEntries;
	}

	/**
	 * @param returnOrderEntries
	 *           the returnOrderEntries to set
	 */
	public void setReturnOrderEntries(final List<String> returnOrderEntries)
	{
		this.returnOrderEntries = returnOrderEntries;
	}



}
