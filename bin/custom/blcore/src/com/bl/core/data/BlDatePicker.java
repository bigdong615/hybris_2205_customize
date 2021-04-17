package com.bl.core.data;

import java.io.Serializable;


/**
 * @author Moumita
 * This POJO class is to get the stock related required information
 */
public class BlDatePicker implements Serializable
{
	private String rentalStartDate;

	private String rentalEndDate;

	/**
	 * @return the rentalStartDate
	 */
	public String getRentalStartDate()
	{
		return rentalStartDate;
	}

	/**
	 * @param rentalStartDate
	 *           the rentalStartDate to set
	 */
	public void setRentalStartDate(final String rentalStartDate)
	{
		this.rentalStartDate = rentalStartDate;
	}

	/**
	 * @return the rentalEndDate
	 */
	public String getRentalEndDate()
	{
		return rentalEndDate;
	}

	/**
	 * @param rentalEndDate
	 *           the rentalEndDate to set
	 */
	public void setRentalEndDate(final String rentalEndDate)
	{
		this.rentalEndDate = rentalEndDate;
	}

}
