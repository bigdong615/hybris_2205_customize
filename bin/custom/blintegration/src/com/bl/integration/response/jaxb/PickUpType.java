//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.8-b130911.1802 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2021.04.16 at 04:41:38 PM IST 
//


package com.bl.integration.response.jaxb;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for PickUpType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PickUpType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="DayOfWeek" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="PickUpDetails" type="{}PickUpDetailsType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PickUpType", propOrder = {
    "dayOfWeek",
    "pickUpDetails"
})
public class PickUpType {

    @XmlElement(name = "DayOfWeek", required = true)
    protected String dayOfWeek;
    @XmlElement(name = "PickUpDetails", required = true)
    protected PickUpDetailsType pickUpDetails;

    /**
     * Gets the value of the dayOfWeek property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDayOfWeek() {
        return dayOfWeek;
    }

    /**
     * Sets the value of the dayOfWeek property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDayOfWeek(String value) {
        this.dayOfWeek = value;
    }

    /**
     * Gets the value of the pickUpDetails property.
     * 
     * @return
     *     possible object is
     *     {@link PickUpDetailsType }
     *     
     */
    public PickUpDetailsType getPickUpDetails() {
        return pickUpDetails;
    }

    /**
     * Sets the value of the pickUpDetails property.
     * 
     * @param value
     *     allowed object is
     *     {@link PickUpDetailsType }
     *     
     */
    public void setPickUpDetails(PickUpDetailsType value) {
        this.pickUpDetails = value;
    }

}
