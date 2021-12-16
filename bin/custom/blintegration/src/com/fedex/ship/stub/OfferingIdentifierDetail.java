/**
 * OfferingIdentifierDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class OfferingIdentifierDetail  implements java.io.Serializable {
    /* Unique identifier for this offering. */
    private java.lang.String id;

    /* Specifies values like PRIORITY_OVERNIGHT, FEDEX_ENVELOPE, PRIORITY_ALERT,
     * and other values typically used to identify various offerings in transactions. */
    private java.lang.String type;

    /* Specifies the two character code used for operationalization
     * of this offering. */
    private java.lang.String code;

    public OfferingIdentifierDetail() {
    }

    public OfferingIdentifierDetail(
           java.lang.String id,
           java.lang.String type,
           java.lang.String code) {
           this.id = id;
           this.type = type;
           this.code = code;
    }


    /**
     * Gets the id value for this OfferingIdentifierDetail.
     * 
     * @return id   * Unique identifier for this offering.
     */
    public java.lang.String getId() {
        return id;
    }


    /**
     * Sets the id value for this OfferingIdentifierDetail.
     * 
     * @param id   * Unique identifier for this offering.
     */
    public void setId(java.lang.String id) {
        this.id = id;
    }


    /**
     * Gets the type value for this OfferingIdentifierDetail.
     * 
     * @return type   * Specifies values like PRIORITY_OVERNIGHT, FEDEX_ENVELOPE, PRIORITY_ALERT,
     * and other values typically used to identify various offerings in transactions.
     */
    public java.lang.String getType() {
        return type;
    }


    /**
     * Sets the type value for this OfferingIdentifierDetail.
     * 
     * @param type   * Specifies values like PRIORITY_OVERNIGHT, FEDEX_ENVELOPE, PRIORITY_ALERT,
     * and other values typically used to identify various offerings in transactions.
     */
    public void setType(java.lang.String type) {
        this.type = type;
    }


    /**
     * Gets the code value for this OfferingIdentifierDetail.
     * 
     * @return code   * Specifies the two character code used for operationalization
     * of this offering.
     */
    public java.lang.String getCode() {
        return code;
    }


    /**
     * Sets the code value for this OfferingIdentifierDetail.
     * 
     * @param code   * Specifies the two character code used for operationalization
     * of this offering.
     */
    public void setCode(java.lang.String code) {
        this.code = code;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof OfferingIdentifierDetail)) return false;
        OfferingIdentifierDetail other = (OfferingIdentifierDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.id==null && other.getId()==null) || 
             (this.id!=null &&
              this.id.equals(other.getId()))) &&
            ((this.type==null && other.getType()==null) || 
             (this.type!=null &&
              this.type.equals(other.getType()))) &&
            ((this.code==null && other.getCode()==null) || 
             (this.code!=null &&
              this.code.equals(other.getCode())));
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
        if (getId() != null) {
            _hashCode += getId().hashCode();
        }
        if (getType() != null) {
            _hashCode += getType().hashCode();
        }
        if (getCode() != null) {
            _hashCode += getCode().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(OfferingIdentifierDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "OfferingIdentifierDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("id");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Id"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("type");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Type"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("code");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Code"));
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
