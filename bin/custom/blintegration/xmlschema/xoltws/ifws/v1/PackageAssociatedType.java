//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.1.10 in JDK 6 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2021.06.07 at 12:13:33 PM IST 
//


package com.ups.xmlschema.xoltws.ifws.v1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for PackageAssociatedType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PackageAssociatedType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="PackageNumber" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="ProductAmount" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PackageAssociatedType", propOrder = {
    "packageNumber",
    "productAmount"
})
public class PackageAssociatedType {

    @XmlElement(name = "PackageNumber")
    protected String packageNumber;
    @XmlElement(name = "ProductAmount")
    protected String productAmount;

    /**
     * Gets the value of the packageNumber property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPackageNumber() {
        return packageNumber;
    }

    /**
     * Sets the value of the packageNumber property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPackageNumber(String value) {
        this.packageNumber = value;
    }

    /**
     * Gets the value of the productAmount property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getProductAmount() {
        return productAmount;
    }

    /**
     * Sets the value of the productAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setProductAmount(String value) {
        this.productAmount = value;
    }

}
