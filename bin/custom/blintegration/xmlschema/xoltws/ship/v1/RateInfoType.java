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
 * <p>Java class for RateInfoType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="RateInfoType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="NegotiatedRatesIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="FRSShipmentIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="RateChartIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="TPFCNegotiatedRatesIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="UserLevelDiscountIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RateInfoType", propOrder = {
    "negotiatedRatesIndicator",
    "frsShipmentIndicator",
    "rateChartIndicator",
    "tpfcNegotiatedRatesIndicator",
    "userLevelDiscountIndicator"
})
public class RateInfoType {

    @XmlElement(name = "NegotiatedRatesIndicator")
    protected String negotiatedRatesIndicator;
    @XmlElement(name = "FRSShipmentIndicator")
    protected String frsShipmentIndicator;
    @XmlElement(name = "RateChartIndicator")
    protected String rateChartIndicator;
    @XmlElement(name = "TPFCNegotiatedRatesIndicator")
    protected String tpfcNegotiatedRatesIndicator;
    @XmlElement(name = "UserLevelDiscountIndicator")
    protected String userLevelDiscountIndicator;

    /**
     * Gets the value of the negotiatedRatesIndicator property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNegotiatedRatesIndicator() {
        return negotiatedRatesIndicator;
    }

    /**
     * Sets the value of the negotiatedRatesIndicator property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNegotiatedRatesIndicator(String value) {
        this.negotiatedRatesIndicator = value;
    }

    /**
     * Gets the value of the frsShipmentIndicator property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFRSShipmentIndicator() {
        return frsShipmentIndicator;
    }

    /**
     * Sets the value of the frsShipmentIndicator property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFRSShipmentIndicator(String value) {
        this.frsShipmentIndicator = value;
    }

    /**
     * Gets the value of the rateChartIndicator property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRateChartIndicator() {
        return rateChartIndicator;
    }

    /**
     * Sets the value of the rateChartIndicator property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRateChartIndicator(String value) {
        this.rateChartIndicator = value;
    }

    /**
     * Gets the value of the tpfcNegotiatedRatesIndicator property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTPFCNegotiatedRatesIndicator() {
        return tpfcNegotiatedRatesIndicator;
    }

    /**
     * Sets the value of the tpfcNegotiatedRatesIndicator property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTPFCNegotiatedRatesIndicator(String value) {
        this.tpfcNegotiatedRatesIndicator = value;
    }

    /**
     * Gets the value of the userLevelDiscountIndicator property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getUserLevelDiscountIndicator() {
        return userLevelDiscountIndicator;
    }

    /**
     * Sets the value of the userLevelDiscountIndicator property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setUserLevelDiscountIndicator(String value) {
        this.userLevelDiscountIndicator = value;
    }

}
