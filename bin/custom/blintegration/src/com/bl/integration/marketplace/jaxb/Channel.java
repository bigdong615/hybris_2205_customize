package com.bl.integration.marketplace.jaxb;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
	 "title",
	 "link",
	 "description",
	 "items"
})
@XmlRootElement(name = "channel")
public class Channel
{
	@XmlElement(name = "title")
   protected String title;
	@XmlElement(name = "link")
   protected String link;
	@XmlElement(name = "description")
   protected String description;
	@XmlElement(name = "items")
	protected List<Item> items;

	/**
	 * @return the title
	 */
	public String getTitle()
	{
		return title;
	}

	/**
	 * @param title the title to set
	 */
	public void setTitle(final String title)
	{
		this.title = title;
	}

	/**
	 * @return the link
	 */
	public String getLink()
	{
		return link;
	}

	/**
	 * @param link
	 *           the link to set
	 */
	public void setLink(final String link)
	{
		this.link = link;
	}

	/**
	 * @return the description
	 */
	public String getDescription()
	{
		return description;
	}

	/**
	 * @param description
	 *           the description to set
	 */
	public void setDescription(final String description)
	{
		this.description = description;
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
