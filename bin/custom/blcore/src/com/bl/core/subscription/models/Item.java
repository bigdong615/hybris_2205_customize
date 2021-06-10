package com.bl.core.subscription.models;

import java.util.List;


public class Item{
	private List<Value> values;

	/**
	 * @return the values
	 */
	public List<Value> getValues()
	{
		return values;
	}

	/**
	 * @param values
	 *           the values to set
	 */
	public void setValues(final List<Value> values)
	{
		this.values = values;
	}


}

