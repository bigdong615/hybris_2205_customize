//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.8-b130911.1802 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2021.04.16 at 04:42:03 PM IST 
//


package com.bl.integration.request.jaxb;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for OriginAddressType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="OriginAddressType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Geocode" type="{}GeocodeType" minOccurs="0"/>
 *         &lt;element name="AddressKeyFormat" type="{}AddressKeyFormatType" minOccurs="0"/>
 *         &lt;element name="MaximumListSize" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "OriginAddressType", propOrder = {
    "geocode",
    "addressKeyFormat",
    "maximumListSize"
})
public class OriginAddressType {

    @XmlElement(name = "Geocode")
    protected GeocodeType geocode;
    @XmlElement(name = "AddressKeyFormat")
    protected AddressKeyFormatType addressKeyFormat;
    @XmlElement(name = "MaximumListSize")
    protected String maximumListSize;

    /**
     * Gets the value of the geocode property.
     * 
     * @return
     *     possible object is
     *     {@link GeocodeType }
     *     
     */
    public GeocodeType getGeocode() {
        return geocode;
    }

    /**
     * Sets the value of the geocode property.
     * 
     * @param value
     *     allowed object is
     *     {@link GeocodeType }
     *     
     */
    public void setGeocode(GeocodeType value) {
        this.geocode = value;
    }

    /**
     * Gets the value of the addressKeyFormat property.
     * 
     * @return
     *     possible object is
     *     {@link AddressKeyFormatType }
     *     
     */
    public AddressKeyFormatType getAddressKeyFormat() {
        return addressKeyFormat;
    }

    /**
     * Sets the value of the addressKeyFormat property.
     * 
     * @param value
     *     allowed object is
     *     {@link AddressKeyFormatType }
     *     
     */
    public void setAddressKeyFormat(AddressKeyFormatType value) {
        this.addressKeyFormat = value;
    }

    /**
     * Gets the value of the maximumListSize property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMaximumListSize() {
        return maximumListSize;
    }

    /**
     * Sets the value of the maximumListSize property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMaximumListSize(String value) {
        this.maximumListSize = value;
    }

}
