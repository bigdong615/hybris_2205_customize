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
 * <p>Java class for AccessPointInformationType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AccessPointInformationType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="PublicAccessPointID" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="ImageURL" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="LoadCapacityRatio" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="BusinessClassificationList" type="{}BusinessClassificationListType" minOccurs="0"/>
 *         &lt;element name="AccessPointStatus" type="{}CodeType" minOccurs="0"/>
 *         &lt;element name="FacilitySLIC" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="PrivateNetworkList" type="{}PrivateNetworkListType" minOccurs="0"/>
 *         &lt;element name="Availability" type="{}AvailabilityType" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AccessPointInformationType", propOrder = {
    "publicAccessPointID",
    "imageURL",
    "loadCapacityRatio",
    "businessClassificationList",
    "accessPointStatus",
    "facilitySLIC",
    "privateNetworkList",
    "availability"
})
public class AccessPointInformationType {

    @XmlElement(name = "PublicAccessPointID")
    protected String publicAccessPointID;
    @XmlElement(name = "ImageURL")
    protected String imageURL;
    @XmlElement(name = "LoadCapacityRatio")
    protected String loadCapacityRatio;
    @XmlElement(name = "BusinessClassificationList")
    protected BusinessClassificationListType businessClassificationList;
    @XmlElement(name = "AccessPointStatus")
    protected CodeType accessPointStatus;
    @XmlElement(name = "FacilitySLIC")
    protected String facilitySLIC;
    @XmlElement(name = "PrivateNetworkList")
    protected PrivateNetworkListType privateNetworkList;
    @XmlElement(name = "Availability")
    protected AvailabilityType availability;

    /**
     * Gets the value of the publicAccessPointID property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPublicAccessPointID() {
        return publicAccessPointID;
    }

    /**
     * Sets the value of the publicAccessPointID property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPublicAccessPointID(String value) {
        this.publicAccessPointID = value;
    }

    /**
     * Gets the value of the imageURL property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getImageURL() {
        return imageURL;
    }

    /**
     * Sets the value of the imageURL property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setImageURL(String value) {
        this.imageURL = value;
    }

    /**
     * Gets the value of the loadCapacityRatio property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLoadCapacityRatio() {
        return loadCapacityRatio;
    }

    /**
     * Sets the value of the loadCapacityRatio property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLoadCapacityRatio(String value) {
        this.loadCapacityRatio = value;
    }

    /**
     * Gets the value of the businessClassificationList property.
     * 
     * @return
     *     possible object is
     *     {@link BusinessClassificationListType }
     *     
     */
    public BusinessClassificationListType getBusinessClassificationList() {
        return businessClassificationList;
    }

    /**
     * Sets the value of the businessClassificationList property.
     * 
     * @param value
     *     allowed object is
     *     {@link BusinessClassificationListType }
     *     
     */
    public void setBusinessClassificationList(BusinessClassificationListType value) {
        this.businessClassificationList = value;
    }

    /**
     * Gets the value of the accessPointStatus property.
     * 
     * @return
     *     possible object is
     *     {@link CodeType }
     *     
     */
    public CodeType getAccessPointStatus() {
        return accessPointStatus;
    }

    /**
     * Sets the value of the accessPointStatus property.
     * 
     * @param value
     *     allowed object is
     *     {@link CodeType }
     *     
     */
    public void setAccessPointStatus(CodeType value) {
        this.accessPointStatus = value;
    }

    /**
     * Gets the value of the facilitySLIC property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFacilitySLIC() {
        return facilitySLIC;
    }

    /**
     * Sets the value of the facilitySLIC property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFacilitySLIC(String value) {
        this.facilitySLIC = value;
    }

    /**
     * Gets the value of the privateNetworkList property.
     * 
     * @return
     *     possible object is
     *     {@link PrivateNetworkListType }
     *     
     */
    public PrivateNetworkListType getPrivateNetworkList() {
        return privateNetworkList;
    }

    /**
     * Sets the value of the privateNetworkList property.
     * 
     * @param value
     *     allowed object is
     *     {@link PrivateNetworkListType }
     *     
     */
    public void setPrivateNetworkList(PrivateNetworkListType value) {
        this.privateNetworkList = value;
    }

    /**
     * Gets the value of the availability property.
     * 
     * @return
     *     possible object is
     *     {@link AvailabilityType }
     *     
     */
    public AvailabilityType getAvailability() {
        return availability;
    }

    /**
     * Sets the value of the availability property.
     * 
     * @param value
     *     allowed object is
     *     {@link AvailabilityType }
     *     
     */
    public void setAvailability(AvailabilityType value) {
        this.availability = value;
    }

}
