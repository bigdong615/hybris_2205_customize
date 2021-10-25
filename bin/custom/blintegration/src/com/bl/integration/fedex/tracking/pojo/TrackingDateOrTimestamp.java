/**
 * TrackingDateOrTimestamp.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class TrackingDateOrTimestamp  implements java.io.Serializable {
    private  TrackingDateOrTimestampType type;

    private String dateOrTimestamp;

    public TrackingDateOrTimestamp() {
    }

    public TrackingDateOrTimestamp(
            TrackingDateOrTimestampType type,
           String dateOrTimestamp) {
           this.type = type;
           this.dateOrTimestamp = dateOrTimestamp;
    }


    /**
     * Gets the type value for this TrackingDateOrTimestamp.
     *
     * @return type
     */
    public  TrackingDateOrTimestampType getType() {
        return type;
    }


    /**
     * Sets the type value for this TrackingDateOrTimestamp.
     *
     * @param type
     */
    public void setType( TrackingDateOrTimestampType type) {
        this.type = type;
    }


    /**
     * Gets the dateOrTimestamp value for this TrackingDateOrTimestamp.
     *
     * @return dateOrTimestamp
     */
    public String getDateOrTimestamp() {
        return dateOrTimestamp;
    }


    /**
     * Sets the dateOrTimestamp value for this TrackingDateOrTimestamp.
     *
     * @param dateOrTimestamp
     */
    public void setDateOrTimestamp(String dateOrTimestamp) {
        this.dateOrTimestamp = dateOrTimestamp;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof TrackingDateOrTimestamp)) return false;
        TrackingDateOrTimestamp other = (TrackingDateOrTimestamp) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.type==null && other.getType()==null) ||
             (this.type!=null &&
              this.type.equals(other.getType()))) &&
            ((this.dateOrTimestamp==null && other.getDateOrTimestamp()==null) ||
             (this.dateOrTimestamp!=null &&
              this.dateOrTimestamp.equals(other.getDateOrTimestamp())));
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
        if (getType() != null) {
            _hashCode += getType().hashCode();
        }
        if (getDateOrTimestamp() != null) {
            _hashCode += getDateOrTimestamp().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(TrackingDateOrTimestamp.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDateOrTimestamp"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("type");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Type"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDateOrTimestampType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dateOrTimestamp");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "DateOrTimestamp"));
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
