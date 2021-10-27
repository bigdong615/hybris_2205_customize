/**
 * TrackingSignatureProofOfDeliveryDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class TrackingSignatureProofOfDeliveryDetail  implements java.io.Serializable {
    private  TrackingDocumentFormat documentFormat;

    public TrackingSignatureProofOfDeliveryDetail() {
    }

    public TrackingSignatureProofOfDeliveryDetail(
            TrackingDocumentFormat documentFormat) {
           this.documentFormat = documentFormat;
    }


    /**
     * Gets the documentFormat value for this TrackingSignatureProofOfDeliveryDetail.
     * 
     * @return documentFormat
     */
    public  TrackingDocumentFormat getDocumentFormat() {
        return documentFormat;
    }


    /**
     * Sets the documentFormat value for this TrackingSignatureProofOfDeliveryDetail.
     * 
     * @param documentFormat
     */
    public void setDocumentFormat( TrackingDocumentFormat documentFormat) {
        this.documentFormat = documentFormat;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof TrackingSignatureProofOfDeliveryDetail)) return false;
        TrackingSignatureProofOfDeliveryDetail other = (TrackingSignatureProofOfDeliveryDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.documentFormat==null && other.getDocumentFormat()==null) ||
             (this.documentFormat!=null &&
              this.documentFormat.equals(other.getDocumentFormat())));
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
        if (getDocumentFormat() != null) {
            _hashCode += getDocumentFormat().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(TrackingSignatureProofOfDeliveryDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingSignatureProofOfDeliveryDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("documentFormat");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "DocumentFormat"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDocumentFormat"));
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
