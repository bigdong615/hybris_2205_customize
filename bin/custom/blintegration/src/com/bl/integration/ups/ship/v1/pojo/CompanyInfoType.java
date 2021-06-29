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
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>
 * Java class for CompanyInfoType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType name="CompanyInfoType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="AttentionName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="CompanyDisplayableName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="TaxIdentificationNumber" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="TaxIDType" type="{http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0}TaxIDCodeDescType" minOccurs="0"/>
 *         &lt;element name="Phone" type="{http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0}ShipPhoneType" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CompanyInfoType", propOrder =
{ "name", "attentionName", "companyDisplayableName", "taxIdentificationNumber", "taxIDType", "phone" })
@XmlSeeAlso(
{ ShipperType.class, ShipFromType.class, ShipToType.class })
public class CompanyInfoType
{

	@XmlElement(name = "Name", required = true)
	protected String name;
	@XmlElement(name = "AttentionName")
	protected String attentionName;
	@XmlElement(name = "CompanyDisplayableName")
	protected String companyDisplayableName;
	@XmlElement(name = "TaxIdentificationNumber")
	protected String taxIdentificationNumber;
	@XmlElement(name = "TaxIDType")
	protected TaxIDCodeDescType taxIDType;
	@XmlElement(name = "Phone")
	protected ShipPhoneType phone;

	/**
	 * Gets the value of the name property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Sets the value of the name property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setName(final String value)
	{
		this.name = value;
	}

	/**
	 * Gets the value of the attentionName property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getAttentionName()
	{
		return attentionName;
	}

	/**
	 * Sets the value of the attentionName property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setAttentionName(final String value)
	{
		this.attentionName = value;
	}

	/**
	 * Gets the value of the companyDisplayableName property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getCompanyDisplayableName()
	{
		return companyDisplayableName;
	}

	/**
	 * Sets the value of the companyDisplayableName property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setCompanyDisplayableName(final String value)
	{
		this.companyDisplayableName = value;
	}

	/**
	 * Gets the value of the taxIdentificationNumber property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getTaxIdentificationNumber()
	{
		return taxIdentificationNumber;
	}

	/**
	 * Sets the value of the taxIdentificationNumber property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setTaxIdentificationNumber(final String value)
	{
		this.taxIdentificationNumber = value;
	}

	/**
	 * Gets the value of the taxIDType property.
	 * 
	 * @return possible object is {@link TaxIDCodeDescType }
	 * 
	 */
	public TaxIDCodeDescType getTaxIDType()
	{
		return taxIDType;
	}

	/**
	 * Sets the value of the taxIDType property.
	 * 
	 * @param value
	 *           allowed object is {@link TaxIDCodeDescType }
	 * 
	 */
	public void setTaxIDType(final TaxIDCodeDescType value)
	{
		this.taxIDType = value;
	}

	/**
	 * Gets the value of the phone property.
	 * 
	 * @return possible object is {@link ShipPhoneType }
	 * 
	 */
	public ShipPhoneType getPhone()
	{
		return phone;
	}

	/**
	 * Sets the value of the phone property.
	 * 
	 * @param value
	 *           allowed object is {@link ShipPhoneType }
	 * 
	 */
	public void setPhone(final ShipPhoneType value)
	{
		this.phone = value;
	}

}
