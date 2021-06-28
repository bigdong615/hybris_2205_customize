//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.1.10 in JDK 6
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a>
// Any modifications to this file will be lost upon recompilation of the source schema.
// Generated on: 2021.06.07 at 12:15:46 PM IST
//


package com.bl.integration.ups.ship.v1.pojo;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>
 * Java class for RestrictedArticlesType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType name="RestrictedArticlesType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="DiagnosticSpecimensIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="AlcoholicBeveragesIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="PerishablesIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="PlantsIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="SeedsIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="SpecialExceptionsIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="TobaccoIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RestrictedArticlesType", propOrder =
{ "diagnosticSpecimensIndicator", "alcoholicBeveragesIndicator", "perishablesIndicator", "plantsIndicator", "seedsIndicator",
		"specialExceptionsIndicator", "tobaccoIndicator" })
public class RestrictedArticlesType
{

	@XmlElement(name = "DiagnosticSpecimensIndicator")
	protected String diagnosticSpecimensIndicator;
	@XmlElement(name = "AlcoholicBeveragesIndicator")
	protected String alcoholicBeveragesIndicator;
	@XmlElement(name = "PerishablesIndicator")
	protected String perishablesIndicator;
	@XmlElement(name = "PlantsIndicator")
	protected String plantsIndicator;
	@XmlElement(name = "SeedsIndicator")
	protected String seedsIndicator;
	@XmlElement(name = "SpecialExceptionsIndicator")
	protected String specialExceptionsIndicator;
	@XmlElement(name = "TobaccoIndicator")
	protected String tobaccoIndicator;

	/**
	 * Gets the value of the diagnosticSpecimensIndicator property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getDiagnosticSpecimensIndicator()
	{
		return diagnosticSpecimensIndicator;
	}

	/**
	 * Sets the value of the diagnosticSpecimensIndicator property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setDiagnosticSpecimensIndicator(final String value)
	{
		this.diagnosticSpecimensIndicator = value;
	}

	/**
	 * Gets the value of the alcoholicBeveragesIndicator property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getAlcoholicBeveragesIndicator()
	{
		return alcoholicBeveragesIndicator;
	}

	/**
	 * Sets the value of the alcoholicBeveragesIndicator property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setAlcoholicBeveragesIndicator(final String value)
	{
		this.alcoholicBeveragesIndicator = value;
	}

	/**
	 * Gets the value of the perishablesIndicator property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getPerishablesIndicator()
	{
		return perishablesIndicator;
	}

	/**
	 * Sets the value of the perishablesIndicator property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setPerishablesIndicator(final String value)
	{
		this.perishablesIndicator = value;
	}

	/**
	 * Gets the value of the plantsIndicator property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getPlantsIndicator()
	{
		return plantsIndicator;
	}

	/**
	 * Sets the value of the plantsIndicator property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setPlantsIndicator(final String value)
	{
		this.plantsIndicator = value;
	}

	/**
	 * Gets the value of the seedsIndicator property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getSeedsIndicator()
	{
		return seedsIndicator;
	}

	/**
	 * Sets the value of the seedsIndicator property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setSeedsIndicator(final String value)
	{
		this.seedsIndicator = value;
	}

	/**
	 * Gets the value of the specialExceptionsIndicator property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getSpecialExceptionsIndicator()
	{
		return specialExceptionsIndicator;
	}

	/**
	 * Sets the value of the specialExceptionsIndicator property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setSpecialExceptionsIndicator(final String value)
	{
		this.specialExceptionsIndicator = value;
	}

	/**
	 * Gets the value of the tobaccoIndicator property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getTobaccoIndicator()
	{
		return tobaccoIndicator;
	}

	/**
	 * Sets the value of the tobaccoIndicator property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setTobaccoIndicator(final String value)
	{
		this.tobaccoIndicator = value;
	}

}
