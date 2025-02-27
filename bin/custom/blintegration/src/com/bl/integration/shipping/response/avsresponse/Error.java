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
 *         &lt;element name="ErrorSeverity" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ErrorCode" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ErrorDescription" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="MinimumRetrySeconds" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element ref="{}ErrorLocation" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="ErrorDigest" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded" minOccurs="0"/>
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
    "errorSeverity",
    "errorCode",
    "errorDescription",
    "minimumRetrySeconds",
    "errorLocation",
    "errorDigest"
})
@XmlRootElement(name = "Error")
public class Error {

    @XmlElement(name = "ErrorSeverity", required = true)
    protected String errorSeverity;
    @XmlElement(name = "ErrorCode", required = true)
    protected String errorCode;
    @XmlElement(name = "ErrorDescription")
    protected String errorDescription;
    @XmlElement(name = "MinimumRetrySeconds")
    protected String minimumRetrySeconds;
    @XmlElement(name = "ErrorLocation")
    protected List<ErrorLocation> errorLocation;
    @XmlElement(name = "ErrorDigest")
    protected List<String> errorDigest;

    /**
     * Gets the value of the errorSeverity property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getErrorSeverity() {
        return errorSeverity;
    }

    /**
     * Sets the value of the errorSeverity property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setErrorSeverity(String value) {
        this.errorSeverity = value;
    }

    /**
     * Gets the value of the errorCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getErrorCode() {
        return errorCode;
    }

    /**
     * Sets the value of the errorCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setErrorCode(String value) {
        this.errorCode = value;
    }

    /**
     * Gets the value of the errorDescription property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getErrorDescription() {
        return errorDescription;
    }

    /**
     * Sets the value of the errorDescription property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setErrorDescription(String value) {
        this.errorDescription = value;
    }

    /**
     * Gets the value of the minimumRetrySeconds property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMinimumRetrySeconds() {
        return minimumRetrySeconds;
    }

    /**
     * Sets the value of the minimumRetrySeconds property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMinimumRetrySeconds(String value) {
        this.minimumRetrySeconds = value;
    }

    /**
     * Gets the value of the errorLocation property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the errorLocation property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getErrorLocation().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ErrorLocation }
     * 
     * 
     */
    public List<ErrorLocation> getErrorLocation() {
        if (errorLocation == null) {
            errorLocation = new ArrayList<ErrorLocation>();
        }
        return this.errorLocation;
    }

    /**
     * Gets the value of the errorDigest property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the errorDigest property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getErrorDigest().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getErrorDigest() {
        if (errorDigest == null) {
            errorDigest = new ArrayList<String>();
        }
        return this.errorDigest;
    }

}
