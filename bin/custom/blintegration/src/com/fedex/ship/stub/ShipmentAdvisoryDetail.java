/**
 * ShipmentAdvisoryDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class ShipmentAdvisoryDetail  implements java.io.Serializable {
    private com.fedex.ship.stub.RegulatoryAdvisoryDetail regulatoryAdvisory;

    public ShipmentAdvisoryDetail() {
    }

    public ShipmentAdvisoryDetail(
           com.fedex.ship.stub.RegulatoryAdvisoryDetail regulatoryAdvisory) {
           this.regulatoryAdvisory = regulatoryAdvisory;
    }


    /**
     * Gets the regulatoryAdvisory value for this ShipmentAdvisoryDetail.
     * 
     * @return regulatoryAdvisory
     */
    public com.fedex.ship.stub.RegulatoryAdvisoryDetail getRegulatoryAdvisory() {
        return regulatoryAdvisory;
    }


    /**
     * Sets the regulatoryAdvisory value for this ShipmentAdvisoryDetail.
     * 
     * @param regulatoryAdvisory
     */
    public void setRegulatoryAdvisory(com.fedex.ship.stub.RegulatoryAdvisoryDetail regulatoryAdvisory) {
        this.regulatoryAdvisory = regulatoryAdvisory;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof ShipmentAdvisoryDetail)) return false;
        ShipmentAdvisoryDetail other = (ShipmentAdvisoryDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.regulatoryAdvisory==null && other.getRegulatoryAdvisory()==null) || 
             (this.regulatoryAdvisory!=null &&
              this.regulatoryAdvisory.equals(other.getRegulatoryAdvisory())));
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
        if (getRegulatoryAdvisory() != null) {
            _hashCode += getRegulatoryAdvisory().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(ShipmentAdvisoryDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipmentAdvisoryDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("regulatoryAdvisory");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RegulatoryAdvisory"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RegulatoryAdvisoryDetail"));
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
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanSerializer(
            _javaType, _xmlType, typeDesc);
    }

    /**
     * Get Custom Deserializer
     */
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanDeserializer(
            _javaType, _xmlType, typeDesc);
    }

}
