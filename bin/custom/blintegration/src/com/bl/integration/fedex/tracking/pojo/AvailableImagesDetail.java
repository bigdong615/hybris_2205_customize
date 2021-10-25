/**
 * AvailableImagesDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class AvailableImagesDetail  implements java.io.Serializable {
    private  AvailableImageType type;

    private  ImageSizeType size;

    public AvailableImagesDetail() {
    }

    public AvailableImagesDetail(
            AvailableImageType type,
            ImageSizeType size) {
           this.type = type;
           this.size = size;
    }


    /**
     * Gets the type value for this AvailableImagesDetail.
     * 
     * @return type
     */
    public  AvailableImageType getType() {
        return type;
    }


    /**
     * Sets the type value for this AvailableImagesDetail.
     * 
     * @param type
     */
    public void setType( AvailableImageType type) {
        this.type = type;
    }


    /**
     * Gets the size value for this AvailableImagesDetail.
     * 
     * @return size
     */
    public  ImageSizeType getSize() {
        return size;
    }


    /**
     * Sets the size value for this AvailableImagesDetail.
     * 
     * @param size
     */
    public void setSize( ImageSizeType size) {
        this.size = size;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof AvailableImagesDetail)) return false;
        AvailableImagesDetail other = (AvailableImagesDetail) obj;
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
            ((this.size==null && other.getSize()==null) ||
             (this.size!=null &&
              this.size.equals(other.getSize())));
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
        if (getSize() != null) {
            _hashCode += getSize().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(AvailableImagesDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "AvailableImagesDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("type");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Type"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "AvailableImageType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("size");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Size"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ImageSizeType"));
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
