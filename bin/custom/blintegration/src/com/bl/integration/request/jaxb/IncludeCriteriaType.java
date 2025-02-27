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
 * <p>Java class for IncludeCriteriaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="IncludeCriteriaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="MerchantAccountNumberList" type="{}MerchantAccountNumberListType" minOccurs="0"/>
 *         &lt;element name="SearchFilter" type="{}SearchFilterType" minOccurs="0"/>
 *         &lt;element name="ServiceOfferingList" type="{}ServiceOfferingListType" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "IncludeCriteriaType", propOrder = {
    "merchantAccountNumberList",
    "searchFilter",
    "serviceOfferingList"
})
public class IncludeCriteriaType {

    @XmlElement(name = "MerchantAccountNumberList")
    protected MerchantAccountNumberListType merchantAccountNumberList;
    @XmlElement(name = "SearchFilter")
    protected SearchFilterType searchFilter;
    @XmlElement(name = "ServiceOfferingList")
    protected ServiceOfferingListType serviceOfferingList;

    /**
     * Gets the value of the merchantAccountNumberList property.
     * 
     * @return
     *     possible object is
     *     {@link MerchantAccountNumberListType }
     *     
     */
    public MerchantAccountNumberListType getMerchantAccountNumberList() {
        return merchantAccountNumberList;
    }

    /**
     * Sets the value of the merchantAccountNumberList property.
     * 
     * @param value
     *     allowed object is
     *     {@link MerchantAccountNumberListType }
     *     
     */
    public void setMerchantAccountNumberList(MerchantAccountNumberListType value) {
        this.merchantAccountNumberList = value;
    }

    /**
     * Gets the value of the searchFilter property.
     * 
     * @return
     *     possible object is
     *     {@link SearchFilterType }
     *     
     */
    public SearchFilterType getSearchFilter() {
        return searchFilter;
    }

    /**
     * Sets the value of the searchFilter property.
     * 
     * @param value
     *     allowed object is
     *     {@link SearchFilterType }
     *     
     */
    public void setSearchFilter(SearchFilterType value) {
        this.searchFilter = value;
    }

    /**
     * Gets the value of the serviceOfferingList property.
     * 
     * @return
     *     possible object is
     *     {@link ServiceOfferingListType }
     *     
     */
    public ServiceOfferingListType getServiceOfferingList() {
        return serviceOfferingList;
    }

    /**
     * Sets the value of the serviceOfferingList property.
     * 
     * @param value
     *     allowed object is
     *     {@link ServiceOfferingListType }
     *     
     */
    public void setServiceOfferingList(ServiceOfferingListType value) {
        this.serviceOfferingList = value;
    }

}
