//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.1.10 in JDK 6 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2021.06.07 at 12:15:46 PM IST 
//


package com.ups.xmlschema.xoltws.ship.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for DeliveryConfirmationType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DeliveryConfirmationType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="DCISType" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="DCISNumber" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DeliveryConfirmationType", propOrder = {
    "dcisType",
    "dcisNumber"
})
public class DeliveryConfirmationType {

    @XmlElement(name = "DCISType", required = true)
    protected String dcisType;
    @XmlElement(name = "DCISNumber")
    protected String dcisNumber;

    /**
     * Gets the value of the dcisType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDCISType() {
        return dcisType;
    }

    /**
     * Sets the value of the dcisType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDCISType(String value) {
        this.dcisType = value;
    }

    /**
     * Gets the value of the dcisNumber property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDCISNumber() {
        return dcisNumber;
    }

    /**
     * Sets the value of the dcisNumber property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDCISNumber(String value) {
        this.dcisNumber = value;
    }

}
