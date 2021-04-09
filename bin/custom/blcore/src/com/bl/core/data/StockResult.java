package com.bl.core.data;

import java.io.Serializable;


/**
 * @author Moumita
 * This POJO class is to get the stock related required information
 */
public class StockResult implements Serializable
{
	private long availableCount;

	private long totalCount;

	/**
	 * @return the availableCount
	 */
	public long getAvailableCount()
	{
		return availableCount;
	}

	/**
	 * @param availableCount
	 *           the availableCount to set
	 */
	public void setAvailableCount(final long availableCount)
	{
		this.availableCount = availableCount;
	}

	/**
	 * @return the totalCount
	 */
	public long getTotalCount()
	{
		return totalCount;
	}

	/**
	 * @param totalCount
	 *           the totalCount to set
	 */
	public void setTotalCount(final long totalCount)
	{
		this.totalCount = totalCount;
	}

}
