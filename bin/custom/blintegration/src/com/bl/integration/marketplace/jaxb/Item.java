/**
 *
 */
package com.bl.integration.marketplace.jaxb;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
		"id",
		"title",
		"link",
		"description",
		"condition",
		"availability",
		"brand",
		"image_Link",
		"model_Number",
		"gtin",
		"mpn",
		"price",
		"shipping"
})
@XmlRootElement(name = "item")
public class Item
{
	@XmlElement(name = "g:id")
	protected String id;
	@XmlElement(name = "title")
	protected String title;
	@XmlElement(name = "link")
	protected String link;
	@XmlElement(name = "g:description")
	protected String description;
	@XmlElement(name = "g:condition")
	protected String condition;
	@XmlElement(name = "g:availability")
	protected String availability;
	@XmlElement(name = "g:brand")
	protected String brand;
	@XmlElement(name = "g:image_Link")
	protected String image_Link;
	@XmlElement(name = "g:model_Number")
	protected String model_Number;
	@XmlElement(name = "gtin")
	protected String gtin;
	@XmlElement(name = "g:mpn")
	protected String mpn;
	@XmlElement(name = "g:price")
	protected String price;
	@XmlElement(name = "g:sale_price")
	protected String sale_price;
	@XmlElement(name = "g:sale_price_effective_date")
	protected String sale_price_effective_date;
	@XmlElement(name = "g:shipping")
	protected Shipping shipping;
	/**
	 * @return the id
	 */
	public String getId()
	{
		return id;
	}

	/**
	 * @param id
	 *           the id to set
	 */
	public void setId(final String id)
	{
		this.id = id;
	}
	/**
	 * @return the title
	 */
	public String getTitle()
	{
		return title;
	}

	/**
	 * @param title
	 *           the title to set
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
	 * @return the condition
	 */
	public String getCondition()
	{
		return condition;
	}

	/**
	 * @param condition
	 *           the condition to set
	 */
	public void setCondition(final String condition)
	{
		this.condition = condition;
	}
	/**
	 * @return the availability
	 */
	public String getAvailability()
	{
		return availability;
	}

	/**
	 * @param availability
	 *           the availability to set
	 */
	public void setAvailability(final String availability)
	{
		this.availability = availability;
	}

	/**
	 * @return the brand
	 */
	public String getBrand()
	{
		return brand;
	}

	/**
	 * @param brand
	 *           the brand to set
	 */
	public void setBrand(final String brand)
	{
		this.brand = brand;
	}

	/**
	 * @return the image_Link
	 */
	public String getImage_Link()
	{
		return image_Link;
	}

	/**
	 * @param image_Link
	 *           the image_Link to set
	 */
	public void setImage_Link(final String image_Link)
	{
		this.image_Link = image_Link;
	}

	/**
	 * @return the model_Number
	 */
	public String getModel_Number()
	{
		return model_Number;
	}

	/**
	 * @param model_Number
	 *           the model_Number to set
	 */
	public void setModel_Number(final String model_Number)
	{
		this.model_Number = model_Number;
	}
	/**
	 * @return the gtin
	 */
	public String getGtin()
	{
		return gtin;
	}

	/**
	 * @param gtin
	 *           the gtin to set
	 */
	public void setGtin(final String gtin)
	{
		this.gtin = gtin;
	}

	/**
	 * @return the mpn
	 */
	public String getMpn()
	{
		return mpn;
	}

	/**
	 * @param mpn
	 *           the mpn to set
	 */
	public void setMpn(final String mpn)
	{
		this.mpn = mpn;
	}

	/**
	 * @return the price
	 */
	public String getPrice()
	{
		return price;
	}

	/**
	 * @param price
	 *           the price to set
	 */
	public void setPrice(final String price)
	{
		this.price = price;
	}
	/**
	 * @return the shipping
	 */
	public Shipping getShipping()
	{
		return shipping;
	}

	/**
	 * @param shipping
	 *           the shipping to set
	 */
	public void setShipping(final Shipping shipping)
	{
		this.shipping = shipping;
	}
	/**
	 * @return the sale_price
	 */
	public String getSale_price()
	{
		return sale_price;
	}

	/**
	 * @param sale_price the sale_price to set
	 */
	public void setSale_price(String sale_price)
	{
		this.sale_price = sale_price;
	}

	/**
	 * @return the sale_price_effective_date
	 */
	public String getSale_price_effective_date()
	{
		return sale_price_effective_date;
	}

	/**
	 * @param sale_price_effective_date the sale_price_effective_date to set
	 */
	public void setSale_price_effective_date(String sale_price_effective_date)
	{
		this.sale_price_effective_date = sale_price_effective_date;
	}
}
