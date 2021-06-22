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
 *         &lt;element name="Shipment" type="{http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0}ShipmentType"/>
 *         &lt;element name="LabelSpecification" type="{http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0}LabelSpecificationType" minOccurs="0"/>
 *         &lt;element name="ReceiptSpecification" type="{http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0}ReceiptSpecificationType" minOccurs="0"/>
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
{ "request", "shipment", "labelSpecification", "receiptSpecification" })
@XmlRootElement(name = "ShipConfirmRequest")
public class ShipConfirmRequest
{

	@XmlElement(name = "Request", namespace = "http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0", required = true)
	protected RequestType request;
	@XmlElement(name = "Shipment", required = true)
	protected ShipmentType shipment;
	@XmlElement(name = "LabelSpecification")
	protected LabelSpecificationType labelSpecification;
	@XmlElement(name = "ReceiptSpecification")
	protected ReceiptSpecificationType receiptSpecification;

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
	 * Gets the value of the shipment property.
	 *
	 * @return possible object is {@link ShipmentType }
	 *
	 */
	public ShipmentType getShipment()
	{
		return shipment;
	}

	/**
	 * Sets the value of the shipment property.
	 *
	 * @param value
	 *           allowed object is {@link ShipmentType }
	 *
	 */
	public void setShipment(final ShipmentType value)
	{
		this.shipment = value;
	}

	/**
	 * Gets the value of the labelSpecification property.
	 *
	 * @return possible object is {@link LabelSpecificationType }
	 *
	 */
	public LabelSpecificationType getLabelSpecification()
	{
		return labelSpecification;
	}

	/**
	 * Sets the value of the labelSpecification property.
	 *
	 * @param value
	 *           allowed object is {@link LabelSpecificationType }
	 *
	 */
	public void setLabelSpecification(final LabelSpecificationType value)
	{
		this.labelSpecification = value;
	}

	/**
	 * Gets the value of the receiptSpecification property.
	 *
	 * @return possible object is {@link ReceiptSpecificationType }
	 *
	 */
	public ReceiptSpecificationType getReceiptSpecification()
	{
		return receiptSpecification;
	}

	/**
	 * Sets the value of the receiptSpecification property.
	 *
	 * @param value
	 *           allowed object is {@link ReceiptSpecificationType }
	 *
	 */
	public void setReceiptSpecification(final ReceiptSpecificationType value)
	{
		this.receiptSpecification = value;
	}

}
