
package com.bl.integration.ups.common.v1;

import com.ups.xmlschema.xoltws.common.v1.ElementLevelInformationType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for DetailType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DetailType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Code" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="Description" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ElementLevelInformation" type="{http://www.ups.com/XMLSchema/XOLTWS/Common/v1.0}ElementLevelInformationType" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DetailType", propOrder = {
    "code",
    "description",
    "elementLevelInformation"
})
public class DetailType {

    @XmlElement(name = "Code", required = true)
    protected String code;
    @XmlElement(name = "Description", required = true)
    protected String description;
    @XmlElement(name = "ElementLevelInformation")
    protected ElementLevelInformationType elementLevelInformation;

    /**
     * Gets the value of the code property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCode() {
        return code;
    }

    /**
     * Sets the value of the code property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCode(String value) {
        this.code = value;
    }

    /**
     * Gets the value of the description property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the value of the description property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDescription(String value) {
        this.description = value;
    }

    /**
     * Gets the value of the elementLevelInformation property.
     * 
     * @return
     *     possible object is
     *     {@link ElementLevelInformationType }
     *     
     */
    public ElementLevelInformationType getElementLevelInformation() {
        return elementLevelInformation;
    }

    /**
     * Sets the value of the elementLevelInformation property.
     * 
     * @param value
     *     allowed object is
     *     {@link ElementLevelInformationType }
     *     
     */
    public void setElementLevelInformation(ElementLevelInformationType value) {
        this.elementLevelInformation = value;
    }

}
