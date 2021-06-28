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
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.bl.ups.common.v1.pojo.ResponseType;


/**
 * <p>
 * Java class for anonymous complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0}Response"/>
 *         &lt;element name="ShipmentResults" type="{http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0}ShipmentResultsType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder =
{ "response", "shipmentResults" })
@XmlRootElement(name = "ShipConfirmResponse")
public class ShipConfirmResponse
{

	@XmlElement(name = "Response", namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0", required = true)
	protected ResponseType response;
	@XmlElement(name = "ShipmentResults", required = true)
	protected ShipmentResultsType shipmentResults;

	/**
	 * Gets the value of the response property.
	 *
	 * @return possible object is {@link ResponseType }
	 *
	 */
	public ResponseType getResponse()
	{
		return response;
	}

	/**
	 * Sets the value of the response property.
	 *
	 * @param value
	 *           allowed object is {@link ResponseType }
	 *
	 */
	public void setResponse(final ResponseType value)
	{
		this.response = value;
	}

	/**
	 * Gets the value of the shipmentResults property.
	 *
	 * @return possible object is {@link ShipmentResultsType }
	 *
	 */
	public ShipmentResultsType getShipmentResults()
	{
		return shipmentResults;
	}

	/**
	 * Sets the value of the shipmentResults property.
	 *
	 * @param value
	 *           allowed object is {@link ShipmentResultsType }
	 *
	 */
	public void setShipmentResults(final ShipmentResultsType value)
	{
		this.shipmentResults = value;
	}

}
