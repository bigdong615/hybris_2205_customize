//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.1.10 in JDK 6 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2021.05.11 at 01:36:00 PM IST 
//


package com.bl.integration.shipping.response.avsresponse;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{}Response"/>
 *         &lt;sequence minOccurs="0">
 *           &lt;choice>
 *             &lt;element name="ValidAddressIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *             &lt;element name="AmbiguousAddressIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *             &lt;element name="NoCandidatesIndicator" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *           &lt;/choice>
 *           &lt;element name="AddressClassification" type="{}AddressClassificationType" minOccurs="0"/>
 *           &lt;element name="AddressKeyFormat" type="{}AddressKeyFormatType" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;/sequence>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "response",
    "validAddressIndicator",
    "ambiguousAddressIndicator",
    "noCandidatesIndicator",
    "addressClassification",
    "addressKeyFormat"
})
@XmlRootElement(name = "AddressValidationResponse")
public class AddressValidationResponse {

    @XmlElement(name = "Response", required = true)
    protected Response response;
    @XmlElement(name = "ValidAddressIndicator")
    protected String validAddressIndicator;
    @XmlElement(name = "AmbiguousAddressIndicator")
    protected String ambiguousAddressIndicator;
    @XmlElement(name = "NoCandidatesIndicator")
    protected String noCandidatesIndicator;
    @XmlElement(name = "AddressClassification")
    protected AddressClassificationType addressClassification;
    @XmlElement(name = "AddressKeyFormat")
    protected List<AddressKeyFormatType> addressKeyFormat;

    /**
     * Gets the value of the response property.
     * 
     * @return
     *     possible object is
     *     {@link Response }
     *     
     */
    public Response getResponse() {
        return response;
    }

    /**
     * Sets the value of the response property.
     * 
     * @param value
     *     allowed object is
     *     {@link Response }
     *     
     */
    public void setResponse(Response value) {
        this.response = value;
    }

    /**
     * Gets the value of the validAddressIndicator property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getValidAddressIndicator() {
        return validAddressIndicator;
    }

    /**
     * Sets the value of the validAddressIndicator property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setValidAddressIndicator(String value) {
        this.validAddressIndicator = value;
    }

    /**
     * Gets the value of the ambiguousAddressIndicator property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAmbiguousAddressIndicator() {
        return ambiguousAddressIndicator;
    }

    /**
     * Sets the value of the ambiguousAddressIndicator property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAmbiguousAddressIndicator(String value) {
        this.ambiguousAddressIndicator = value;
    }

    /**
     * Gets the value of the noCandidatesIndicator property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNoCandidatesIndicator() {
        return noCandidatesIndicator;
    }

    /**
     * Sets the value of the noCandidatesIndicator property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNoCandidatesIndicator(String value) {
        this.noCandidatesIndicator = value;
    }

    /**
     * Gets the value of the addressClassification property.
     * 
     * @return
     *     possible object is
     *     {@link AddressClassificationType }
     *     
     */
    public AddressClassificationType getAddressClassification() {
        return addressClassification;
    }

    /**
     * Sets the value of the addressClassification property.
     * 
     * @param value
     *     allowed object is
     *     {@link AddressClassificationType }
     *     
     */
    public void setAddressClassification(AddressClassificationType value) {
        this.addressClassification = value;
    }

    /**
     * Gets the value of the addressKeyFormat property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the addressKeyFormat property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAddressKeyFormat().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AddressKeyFormatType }
     * 
     * 
     */
    public List<AddressKeyFormatType> getAddressKeyFormat() {
        if (addressKeyFormat == null) {
            addressKeyFormat = new ArrayList<AddressKeyFormatType>();
        }
        return this.addressKeyFormat;
    }

}
