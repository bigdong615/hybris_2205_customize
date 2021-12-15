/**
 * RegulatoryAdvisoryDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class RegulatoryAdvisoryDetail  implements java.io.Serializable {
    private com.fedex.ship.stub.RegulatoryProhibition[] prohibitions;

    public RegulatoryAdvisoryDetail() {
    }

    public RegulatoryAdvisoryDetail(
           com.fedex.ship.stub.RegulatoryProhibition[] prohibitions) {
           this.prohibitions = prohibitions;
    }


    /**
     * Gets the prohibitions value for this RegulatoryAdvisoryDetail.
     * 
     * @return prohibitions
     */
    public com.fedex.ship.stub.RegulatoryProhibition[] getProhibitions() {
        return prohibitions;
    }


    /**
     * Sets the prohibitions value for this RegulatoryAdvisoryDetail.
     * 
     * @param prohibitions
     */
    public void setProhibitions(com.fedex.ship.stub.RegulatoryProhibition[] prohibitions) {
        this.prohibitions = prohibitions;
    }

    public com.fedex.ship.stub.RegulatoryProhibition getProhibitions(int i) {
        return this.prohibitions[i];
    }

    public void setProhibitions(int i, com.fedex.ship.stub.RegulatoryProhibition _value) {
        this.prohibitions[i] = _value;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof RegulatoryAdvisoryDetail)) return false;
        RegulatoryAdvisoryDetail other = (RegulatoryAdvisoryDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.prohibitions==null && other.getProhibitions()==null) || 
             (this.prohibitions!=null &&
              java.util.Arrays.equals(this.prohibitions, other.getProhibitions())));
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
        if (getProhibitions() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getProhibitions());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getProhibitions(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(RegulatoryAdvisoryDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RegulatoryAdvisoryDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("prohibitions");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Prohibitions"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RegulatoryProhibition"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
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
