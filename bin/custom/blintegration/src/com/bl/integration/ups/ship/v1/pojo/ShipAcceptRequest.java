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

import com.bl.ups.common.v1.pojo.RequestType;


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
 *         &lt;element ref="{http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0}Request"/>
 *         &lt;element name="ShipmentDigest" type="{http://www.w3.org/2001/XMLSchema}string"/>
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
{ "request", "shipmentDigest" })
@XmlRootElement(name = "ShipAcceptRequest")
public class ShipAcceptRequest
{

	@XmlElement(name = "Request", namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0", required = true)
	protected RequestType request;
	@XmlElement(name = "ShipmentDigest", required = true)
	protected String shipmentDigest;

	/**
	 * Gets the value of the request property.
	 *
	 * @return possible object is {@link RequestType }
	 *
	 */
	public RequestType getRequest()
	{
		return request;
	}

	/**
	 * Sets the value of the request property.
	 *
	 * @param value
	 *           allowed object is {@link RequestType }
	 *
	 */
	public void setRequest(final RequestType value)
	{
		this.request = value;
	}

	/**
	 * Gets the value of the shipmentDigest property.
	 *
	 * @return possible object is {@link String }
	 *
	 */
	public String getShipmentDigest()
	{
		return shipmentDigest;
	}

	/**
	 * Sets the value of the shipmentDigest property.
	 *
	 * @param value
	 *           allowed object is {@link String }
	 *
	 */
	public void setShipmentDigest(final String value)
	{
		this.shipmentDigest = value;
	}

}
