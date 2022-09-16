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
		"product_Type",
		"google_Product_Category",
		"brand",
		"image_Link",
		"model_Number",
		"gtin",
		"price",
		"shipping"
})
@XmlRootElement(name = "item")
public class Item
{
	@XmlElement(name = "id")
	protected String id;
	@XmlElement(name = "title")
	protected String title;
	@XmlElement(name = "link")
	protected String link;
	@XmlElement(name = "description")
	protected String description;
	@XmlElement(name = "condition")
	protected String condition;
	@XmlElement(name = "availability")
	protected String availability;
	@XmlElement(name = "product_Type")
	protected String product_Type;
	@XmlElement(name = "google_Product_Category")
	protected String google_Product_Category;
	@XmlElement(name = "brand")
	protected String brand;
	@XmlElement(name = "image_Link")
	protected String image_Link;
	@XmlElement(name = "model_Number")
	protected String model_Number;
	@XmlElement(name = "gtin")
	protected String gtin;
	@XmlElement(name = "price")
	protected String price;
	@XmlElement(name = "shipping")
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
	 * @return the product_Type
	 */
	public String getProduct_Type()
	{
		return product_Type;
	}
	
	/**
	 * @param product_Type
	 *           the product_Type to set
	 */
	public void setProduct_Type(final String product_Type)
	{
		this.product_Type = product_Type;
	}
	
	/**
	 * @return the google_Product_Category
	 */
	public String getGoogle_Product_Category()
	{
		return google_Product_Category;
	}
	
	/**
	 * @param google_Product_Category
	 *           the google_Product_Category to set
	 */
	public void setGoogle_Product_Category(final String google_Product_Category)
	{
		this.google_Product_Category = google_Product_Category;
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
}
