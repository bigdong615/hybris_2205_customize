/**
 * LocalTimeRange.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;


/**
 * Time Range specified in local time.
 */
public class LocalTimeRange  implements java.io.Serializable {
    private String begins;

    private String ends;

    public LocalTimeRange() {
    }

    public LocalTimeRange(
           String begins,
           String ends) {
           this.begins = begins;
           this.ends = ends;
    }


    /**
     * Gets the begins value for this LocalTimeRange.
     *
     * @return begins
     */
    public String getBegins() {
        return begins;
    }


    /**
     * Sets the begins value for this LocalTimeRange.
     *
     * @param begins
     */
    public void setBegins(String begins) {
        this.begins = begins;
    }


    /**
     * Gets the ends value for this LocalTimeRange.
     *
     * @return ends
     */
    public String getEnds() {
        return ends;
    }


    /**
     * Sets the ends value for this LocalTimeRange.
     *
     * @param ends
     */
    public void setEnds(String ends) {
        this.ends = ends;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof LocalTimeRange)) return false;
        LocalTimeRange other = (LocalTimeRange) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.begins==null && other.getBegins()==null) ||
             (this.begins!=null &&
              this.begins.equals(other.getBegins()))) &&
            ((this.ends==null && other.getEnds()==null) ||
             (this.ends!=null &&
              this.ends.equals(other.getEnds())));
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
        if (getBegins() != null) {
            _hashCode += getBegins().hashCode();
        }
        if (getEnds() != null) {
            _hashCode += getEnds().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(LocalTimeRange.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "LocalTimeRange"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("begins");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Begins"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("ends");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Ends"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
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
