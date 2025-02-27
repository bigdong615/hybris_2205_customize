/**
 * DeliveryOptionEligibilityDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;


/**
 * Details about the eligibility for a delivery option.
 */
public class DeliveryOptionEligibilityDetail  implements java.io.Serializable {
    /* Type of delivery option. */
    private  DeliveryOptionType option;

    /* Eligibility of the customer for the specific delivery option. */
    private  EligibilityType eligibility;

    public DeliveryOptionEligibilityDetail() {
    }

    public DeliveryOptionEligibilityDetail(
            DeliveryOptionType option,
            EligibilityType eligibility) {
           this.option = option;
           this.eligibility = eligibility;
    }


    /**
     * Gets the option value for this DeliveryOptionEligibilityDetail.
     * 
     * @return option   * Type of delivery option.
     */
    public  DeliveryOptionType getOption() {
        return option;
    }


    /**
     * Sets the option value for this DeliveryOptionEligibilityDetail.
     * 
     * @param option   * Type of delivery option.
     */
    public void setOption( DeliveryOptionType option) {
        this.option = option;
    }


    /**
     * Gets the eligibility value for this DeliveryOptionEligibilityDetail.
     * 
     * @return eligibility   * Eligibility of the customer for the specific delivery option.
     */
    public  EligibilityType getEligibility() {
        return eligibility;
    }


    /**
     * Sets the eligibility value for this DeliveryOptionEligibilityDetail.
     * 
     * @param eligibility   * Eligibility of the customer for the specific delivery option.
     */
    public void setEligibility( EligibilityType eligibility) {
        this.eligibility = eligibility;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof DeliveryOptionEligibilityDetail)) return false;
        DeliveryOptionEligibilityDetail other = (DeliveryOptionEligibilityDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.option==null && other.getOption()==null) ||
             (this.option!=null &&
              this.option.equals(other.getOption()))) &&
            ((this.eligibility==null && other.getEligibility()==null) ||
             (this.eligibility!=null &&
              this.eligibility.equals(other.getEligibility())));
        __equalsCalc = null;
        return _equals;
    }

    private boolean __hashCodeCalc = false;
    public synchronized int hashCode() {
        if (__hashCodeCalc) {
            return 0;
        }
        __hashCodeCalc = true;
        int _hashCode = 1;
        if (getOption() != null) {
            _hashCode += getOption().hashCode();
        }
        if (getEligibility() != null) {
            _hashCode += getEligibility().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(DeliveryOptionEligibilityDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "DeliveryOptionEligibilityDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("option");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Option"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "DeliveryOptionType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("eligibility");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Eligibility"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "EligibilityType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
    }

    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

    /**
     * Get Custom Serializer
     */
    public static org.apache.axis.encoding.Serializer getSerializer(
           String mechType,
           Class _javaType,
           javax.xml.namespace.QName _xmlType) {
        return
          new  org.apache.axis.encoding.ser.BeanSerializer(
            _javaType, _xmlType, typeDesc);
    }

    /**
     * Get Custom Deserializer
     */
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           String mechType,
           Class _javaType,
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanDeserializer(
            _javaType, _xmlType, typeDesc);
    }

}
