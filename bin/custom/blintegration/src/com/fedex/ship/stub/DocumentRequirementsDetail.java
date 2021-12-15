/**
 * DocumentRequirementsDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class DocumentRequirementsDetail  implements java.io.Serializable {
    private com.fedex.ship.stub.RequiredDocumentType[] requiredDocuments;

    private com.fedex.ship.stub.DocumentGenerationDetail[] generationDetails;

    /* Lists the documents that are not accepted by FedEx for this
     * shipment. */
    private com.fedex.ship.stub.EnterpriseDocumentType[] prohibitedDocuments;

    public DocumentRequirementsDetail() {
    }

    public DocumentRequirementsDetail(
           com.fedex.ship.stub.RequiredDocumentType[] requiredDocuments,
           com.fedex.ship.stub.DocumentGenerationDetail[] generationDetails,
           com.fedex.ship.stub.EnterpriseDocumentType[] prohibitedDocuments) {
           this.requiredDocuments = requiredDocuments;
           this.generationDetails = generationDetails;
           this.prohibitedDocuments = prohibitedDocuments;
    }


    /**
     * Gets the requiredDocuments value for this DocumentRequirementsDetail.
     * 
     * @return requiredDocuments
     */
    public com.fedex.ship.stub.RequiredDocumentType[] getRequiredDocuments() {
        return requiredDocuments;
    }


    /**
     * Sets the requiredDocuments value for this DocumentRequirementsDetail.
     * 
     * @param requiredDocuments
     */
    public void setRequiredDocuments(com.fedex.ship.stub.RequiredDocumentType[] requiredDocuments) {
        this.requiredDocuments = requiredDocuments;
    }

    public com.fedex.ship.stub.RequiredDocumentType getRequiredDocuments(int i) {
        return this.requiredDocuments[i];
    }

    public void setRequiredDocuments(int i, com.fedex.ship.stub.RequiredDocumentType _value) {
        this.requiredDocuments[i] = _value;
    }


    /**
     * Gets the generationDetails value for this DocumentRequirementsDetail.
     * 
     * @return generationDetails
     */
    public com.fedex.ship.stub.DocumentGenerationDetail[] getGenerationDetails() {
        return generationDetails;
    }


    /**
     * Sets the generationDetails value for this DocumentRequirementsDetail.
     * 
     * @param generationDetails
     */
    public void setGenerationDetails(com.fedex.ship.stub.DocumentGenerationDetail[] generationDetails) {
        this.generationDetails = generationDetails;
    }

    public com.fedex.ship.stub.DocumentGenerationDetail getGenerationDetails(int i) {
        return this.generationDetails[i];
    }

    public void setGenerationDetails(int i, com.fedex.ship.stub.DocumentGenerationDetail _value) {
        this.generationDetails[i] = _value;
    }


    /**
     * Gets the prohibitedDocuments value for this DocumentRequirementsDetail.
     * 
     * @return prohibitedDocuments   * Lists the documents that are not accepted by FedEx for this
     * shipment.
     */
    public com.fedex.ship.stub.EnterpriseDocumentType[] getProhibitedDocuments() {
        return prohibitedDocuments;
    }


    /**
     * Sets the prohibitedDocuments value for this DocumentRequirementsDetail.
     * 
     * @param prohibitedDocuments   * Lists the documents that are not accepted by FedEx for this
     * shipment.
     */
    public void setProhibitedDocuments(com.fedex.ship.stub.EnterpriseDocumentType[] prohibitedDocuments) {
        this.prohibitedDocuments = prohibitedDocuments;
    }

    public com.fedex.ship.stub.EnterpriseDocumentType getProhibitedDocuments(int i) {
        return this.prohibitedDocuments[i];
    }

    public void setProhibitedDocuments(int i, com.fedex.ship.stub.EnterpriseDocumentType _value) {
        this.prohibitedDocuments[i] = _value;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof DocumentRequirementsDetail)) return false;
        DocumentRequirementsDetail other = (DocumentRequirementsDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.requiredDocuments==null && other.getRequiredDocuments()==null) || 
             (this.requiredDocuments!=null &&
              java.util.Arrays.equals(this.requiredDocuments, other.getRequiredDocuments()))) &&
            ((this.generationDetails==null && other.getGenerationDetails()==null) || 
             (this.generationDetails!=null &&
              java.util.Arrays.equals(this.generationDetails, other.getGenerationDetails()))) &&
            ((this.prohibitedDocuments==null && other.getProhibitedDocuments()==null) || 
             (this.prohibitedDocuments!=null &&
              java.util.Arrays.equals(this.prohibitedDocuments, other.getProhibitedDocuments())));
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
        if (getRequiredDocuments() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getRequiredDocuments());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getRequiredDocuments(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getGenerationDetails() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getGenerationDetails());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getGenerationDetails(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getProhibitedDocuments() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getProhibitedDocuments());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getProhibitedDocuments(), i);
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
        new org.apache.axis.description.TypeDesc(DocumentRequirementsDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DocumentRequirementsDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("requiredDocuments");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RequiredDocuments"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RequiredDocumentType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("generationDetails");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "GenerationDetails"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DocumentGenerationDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("prohibitedDocuments");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ProhibitedDocuments"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EnterpriseDocumentType"));
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
