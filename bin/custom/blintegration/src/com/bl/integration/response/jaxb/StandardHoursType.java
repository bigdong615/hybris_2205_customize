//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.8-b130911.1802 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2021.04.16 at 04:41:38 PM IST 
//


package com.bl.integration.response.jaxb;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for StandardHoursType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="StandardHoursType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="HoursType" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="DayOfWeek" type="{}DayOfWeekType" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "StandardHoursType", propOrder = {
    "hoursType",
    "dayOfWeek"
})
public class StandardHoursType {

    @XmlElement(name = "HoursType", required = true)
    protected String hoursType;
    @XmlElement(name = "DayOfWeek")
    protected List<DayOfWeekType> dayOfWeek;

    /**
     * Gets the value of the hoursType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHoursType() {
        return hoursType;
    }

    /**
     * Sets the value of the hoursType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHoursType(String value) {
        this.hoursType = value;
    }

    /**
     * Gets the value of the dayOfWeek property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the dayOfWeek property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getDayOfWeek().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link DayOfWeekType }
     * 
     * 
     */
    public List<DayOfWeekType> getDayOfWeek() {
        if (dayOfWeek == null) {
            dayOfWeek = new ArrayList<DayOfWeekType>();
        }
        return this.dayOfWeek;
    }

}
