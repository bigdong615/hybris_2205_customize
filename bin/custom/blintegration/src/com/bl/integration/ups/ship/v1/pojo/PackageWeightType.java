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
 * Java class for PackageWeightType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType name="PackageWeightType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="UnitOfMeasurement" type="{http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0}ShipUnitOfMeasurementType"/>
 *         &lt;element name="Weight" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PackageWeightType", propOrder =
{ "unitOfMeasurement", "weight" })
public class PackageWeightType
{

	@XmlElement(name = "UnitOfMeasurement", required = true)
	protected ShipUnitOfMeasurementType unitOfMeasurement;
	@XmlElement(name = "Weight", required = true)
	protected String weight;

	/**
	 * Gets the value of the unitOfMeasurement property.
	 * 
	 * @return possible object is {@link ShipUnitOfMeasurementType }
	 * 
	 */
	public ShipUnitOfMeasurementType getUnitOfMeasurement()
	{
		return unitOfMeasurement;
	}

	/**
	 * Sets the value of the unitOfMeasurement property.
	 * 
	 * @param value
	 *           allowed object is {@link ShipUnitOfMeasurementType }
	 * 
	 */
	public void setUnitOfMeasurement(final ShipUnitOfMeasurementType value)
	{
		this.unitOfMeasurement = value;
	}

	/**
	 * Gets the value of the weight property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getWeight()
	{
		return weight;
	}

	/**
	 * Sets the value of the weight property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setWeight(final String value)
	{
		this.weight = value;
	}

}
