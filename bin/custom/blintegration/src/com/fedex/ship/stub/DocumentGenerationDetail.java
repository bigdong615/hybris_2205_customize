/**
 * DocumentGenerationDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class DocumentGenerationDetail  implements java.io.Serializable {
    private com.fedex.ship.stub.EnterpriseDocumentType type;

    private org.apache.axis.types.NonNegativeInteger minimumCopiesRequired;

    private com.fedex.ship.stub.RequirementType letterhead;

    private com.fedex.ship.stub.RequirementType electronicSignature;

    public DocumentGenerationDetail() {
    }

    public DocumentGenerationDetail(
           com.fedex.ship.stub.EnterpriseDocumentType type,
           org.apache.axis.types.NonNegativeInteger minimumCopiesRequired,
           com.fedex.ship.stub.RequirementType letterhead,
           com.fedex.ship.stub.RequirementType electronicSignature) {
           this.type = type;
           this.minimumCopiesRequired = minimumCopiesRequired;
           this.letterhead = letterhead;
           this.electronicSignature = electronicSignature;
    }


    /**
     * Gets the type value for this DocumentGenerationDetail.
     * 
     * @return type
     */
    public com.fedex.ship.stub.EnterpriseDocumentType getType() {
        return type;
    }


    /**
     * Sets the type value for this DocumentGenerationDetail.
     * 
     * @param type
     */
    public void setType(com.fedex.ship.stub.EnterpriseDocumentType type) {
        this.type = type;
    }


    /**
     * Gets the minimumCopiesRequired value for this DocumentGenerationDetail.
     * 
     * @return minimumCopiesRequired
     */
    public org.apache.axis.types.NonNegativeInteger getMinimumCopiesRequired() {
        return minimumCopiesRequired;
    }


    /**
     * Sets the minimumCopiesRequired value for this DocumentGenerationDetail.
     * 
     * @param minimumCopiesRequired
     */
    public void setMinimumCopiesRequired(org.apache.axis.types.NonNegativeInteger minimumCopiesRequired) {
        this.minimumCopiesRequired = minimumCopiesRequired;
    }


    /**
     * Gets the letterhead value for this DocumentGenerationDetail.
     * 
     * @return letterhead
     */
    public com.fedex.ship.stub.RequirementType getLetterhead() {
        return letterhead;
    }


    /**
     * Sets the letterhead value for this DocumentGenerationDetail.
     * 
     * @param letterhead
     */
    public void setLetterhead(com.fedex.ship.stub.RequirementType letterhead) {
        this.letterhead = letterhead;
    }


    /**
     * Gets the electronicSignature value for this DocumentGenerationDetail.
     * 
     * @return electronicSignature
     */
    public com.fedex.ship.stub.RequirementType getElectronicSignature() {
        return electronicSignature;
    }


    /**
     * Sets the electronicSignature value for this DocumentGenerationDetail.
     * 
     * @param electronicSignature
     */
    public void setElectronicSignature(com.fedex.ship.stub.RequirementType electronicSignature) {
        this.electronicSignature = electronicSignature;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof DocumentGenerationDetail)) return false;
        DocumentGenerationDetail other = (DocumentGenerationDetail) obj;
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
            ((this.minimumCopiesRequired==null && other.getMinimumCopiesRequired()==null) || 
             (this.minimumCopiesRequired!=null &&
              this.minimumCopiesRequired.equals(other.getMinimumCopiesRequired()))) &&
            ((this.letterhead==null && other.getLetterhead()==null) || 
             (this.letterhead!=null &&
              this.letterhead.equals(other.getLetterhead()))) &&
            ((this.electronicSignature==null && other.getElectronicSignature()==null) || 
             (this.electronicSignature!=null &&
              this.electronicSignature.equals(other.getElectronicSignature())));
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
        if (getMinimumCopiesRequired() != null) {
            _hashCode += getMinimumCopiesRequired().hashCode();
        }
        if (getLetterhead() != null) {
            _hashCode += getLetterhead().hashCode();
        }
        if (getElectronicSignature() != null) {
            _hashCode += getElectronicSignature().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(DocumentGenerationDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DocumentGenerationDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("type");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Type"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EnterpriseDocumentType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("minimumCopiesRequired");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "MinimumCopiesRequired"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "nonNegativeInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("letterhead");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Letterhead"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RequirementType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("electronicSignature");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ElectronicSignature"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RequirementType"));
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
