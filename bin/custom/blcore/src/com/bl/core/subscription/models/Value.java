package com.bl.core.subscription.models;

import com.fasterxml.jackson.annotation.JsonProperty;


public class Value{
	private String name;

	@JsonProperty(value = "value")
	private Object valueData;

	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * @return the valueData
	 */
	public Object getValueData()
	{
		return valueData;
	}

	/**
	 * @param name
	 *           the name to set
	 */
	public void setName(final String name)
	{
		this.name = name;
	}

	/**
	 * @param valueData
	 *           the valueData to set
	 */
	public void setValueData(final Object valueData)
	{
		this.valueData = valueData;
	}


}

