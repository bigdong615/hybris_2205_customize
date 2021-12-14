/**
 * CompletedEtdDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class CompletedEtdDetail  implements java.io.Serializable {
    /* The identifier for all clearance documents associated with
     * this shipment. */
    private java.lang.String folderId;

    private com.fedex.ship.stub.CompletedEtdType type;

    private com.fedex.ship.stub.UploadDocumentReferenceDetail[] uploadDocumentReferenceDetails;

    public CompletedEtdDetail() {
    }

    public CompletedEtdDetail(
           java.lang.String folderId,
           com.fedex.ship.stub.CompletedEtdType type,
           com.fedex.ship.stub.UploadDocumentReferenceDetail[] uploadDocumentReferenceDetails) {
           this.folderId = folderId;
           this.type = type;
           this.uploadDocumentReferenceDetails = uploadDocumentReferenceDetails;
    }


    /**
     * Gets the folderId value for this CompletedEtdDetail.
     * 
     * @return folderId   * The identifier for all clearance documents associated with
     * this shipment.
     */
    public java.lang.String getFolderId() {
        return folderId;
    }


    /**
     * Sets the folderId value for this CompletedEtdDetail.
     * 
     * @param folderId   * The identifier for all clearance documents associated with
     * this shipment.
     */
    public void setFolderId(java.lang.String folderId) {
        this.folderId = folderId;
    }


    /**
     * Gets the type value for this CompletedEtdDetail.
     * 
     * @return type
     */
    public com.fedex.ship.stub.CompletedEtdType getType() {
        return type;
    }


    /**
     * Sets the type value for this CompletedEtdDetail.
     * 
     * @param type
     */
    public void setType(com.fedex.ship.stub.CompletedEtdType type) {
        this.type = type;
    }


    /**
     * Gets the uploadDocumentReferenceDetails value for this CompletedEtdDetail.
     * 
     * @return uploadDocumentReferenceDetails
     */
    public com.fedex.ship.stub.UploadDocumentReferenceDetail[] getUploadDocumentReferenceDetails() {
        return uploadDocumentReferenceDetails;
    }


    /**
     * Sets the uploadDocumentReferenceDetails value for this CompletedEtdDetail.
     * 
     * @param uploadDocumentReferenceDetails
     */
    public void setUploadDocumentReferenceDetails(com.fedex.ship.stub.UploadDocumentReferenceDetail[] uploadDocumentReferenceDetails) {
        this.uploadDocumentReferenceDetails = uploadDocumentReferenceDetails;
    }

    public com.fedex.ship.stub.UploadDocumentReferenceDetail getUploadDocumentReferenceDetails(int i) {
        return this.uploadDocumentReferenceDetails[i];
    }

    public void setUploadDocumentReferenceDetails(int i, com.fedex.ship.stub.UploadDocumentReferenceDetail _value) {
        this.uploadDocumentReferenceDetails[i] = _value;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof CompletedEtdDetail)) return false;
        CompletedEtdDetail other = (CompletedEtdDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.folderId==null && other.getFolderId()==null) || 
             (this.folderId!=null &&
              this.folderId.equals(other.getFolderId()))) &&
            ((this.type==null && other.getType()==null) || 
             (this.type!=null &&
              this.type.equals(other.getType()))) &&
            ((this.uploadDocumentReferenceDetails==null && other.getUploadDocumentReferenceDetails()==null) || 
             (this.uploadDocumentReferenceDetails!=null &&
              java.util.Arrays.equals(this.uploadDocumentReferenceDetails, other.getUploadDocumentReferenceDetails())));
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
        if (getFolderId() != null) {
            _hashCode += getFolderId().hashCode();
        }
        if (getType() != null) {
            _hashCode += getType().hashCode();
        }
        if (getUploadDocumentReferenceDetails() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getUploadDocumentReferenceDetails());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getUploadDocumentReferenceDetails(), i);
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
        new org.apache.axis.description.TypeDesc(CompletedEtdDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedEtdDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("folderId");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FolderId"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("type");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Type"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedEtdType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("uploadDocumentReferenceDetails");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UploadDocumentReferenceDetails"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UploadDocumentReferenceDetail"));
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
