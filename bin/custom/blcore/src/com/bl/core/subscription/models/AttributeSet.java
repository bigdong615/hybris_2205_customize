package com.bl.core.subscription.models;

import java.util.List;


public class AttributeSet{
	private String name;
	private List<Item> items;


	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
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
	 * @return the items
	 */
	public List<Item> getItems()
	{
		return items;
	}

	/**
	 * @param items
	 *           the items to set
	 */
	public void setItems(final List<Item> items)
	{
		this.items = items;
	}


}

