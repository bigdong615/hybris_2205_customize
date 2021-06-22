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
 * Java class for PSONotificationType complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType name="PSONotificationType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="NotificationCode" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="EMail" type="{http://www.ups.com/XMLSchema/XOLTWS/Ship/v1.0}EmailDetailsType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PSONotificationType", propOrder =
{ "notificationCode", "eMail" })
public class PSONotificationType
{

	@XmlElement(name = "NotificationCode", required = true)
	protected String notificationCode;
	@XmlElement(name = "EMail", required = true)
	protected EmailDetailsType eMail;

	/**
	 * Gets the value of the notificationCode property.
	 * 
	 * @return possible object is {@link String }
	 * 
	 */
	public String getNotificationCode()
	{
		return notificationCode;
	}

	/**
	 * Sets the value of the notificationCode property.
	 * 
	 * @param value
	 *           allowed object is {@link String }
	 * 
	 */
	public void setNotificationCode(final String value)
	{
		this.notificationCode = value;
	}

	/**
	 * Gets the value of the eMail property.
	 * 
	 * @return possible object is {@link EmailDetailsType }
	 * 
	 */
	public EmailDetailsType getEMail()
	{
		return eMail;
	}

	/**
	 * Sets the value of the eMail property.
	 * 
	 * @param value
	 *           allowed object is {@link EmailDetailsType }
	 * 
	 */
	public void setEMail(final EmailDetailsType value)
	{
		this.eMail = value;
	}

}
