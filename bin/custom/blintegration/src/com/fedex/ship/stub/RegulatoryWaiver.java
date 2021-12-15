/**
 * RegulatoryWaiver.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class RegulatoryWaiver  implements java.io.Serializable {
    /* FOR FEDEX INTERNAL USE ONLY: The id of the waiver. */
    private java.lang.String id;

    /* FOR FEDEX INTERNAL USE ONLY: The description of the waiver. */
    private java.lang.String description;

    private com.fedex.ship.stub.Message[] advisories;

    public RegulatoryWaiver() {
    }

    public RegulatoryWaiver(
           java.lang.String id,
           java.lang.String description,
           com.fedex.ship.stub.Message[] advisories) {
           this.id = id;
           this.description = description;
           this.advisories = advisories;
    }


    /**
     * Gets the id value for this RegulatoryWaiver.
     * 
     * @return id   * FOR FEDEX INTERNAL USE ONLY: The id of the waiver.
     */
    public java.lang.String getId() {
        return id;
    }


    /**
     * Sets the id value for this RegulatoryWaiver.
     * 
     * @param id   * FOR FEDEX INTERNAL USE ONLY: The id of the waiver.
     */
    public void setId(java.lang.String id) {
        this.id = id;
    }


    /**
     * Gets the description value for this RegulatoryWaiver.
     * 
     * @return description   * FOR FEDEX INTERNAL USE ONLY: The description of the waiver.
     */
    public java.lang.String getDescription() {
        return description;
    }


    /**
     * Sets the description value for this RegulatoryWaiver.
     * 
     * @param description   * FOR FEDEX INTERNAL USE ONLY: The description of the waiver.
     */
    public void setDescription(java.lang.String description) {
        this.description = description;
    }


    /**
     * Gets the advisories value for this RegulatoryWaiver.
     * 
     * @return advisories
     */
    public com.fedex.ship.stub.Message[] getAdvisories() {
        return advisories;
    }


    /**
     * Sets the advisories value for this RegulatoryWaiver.
     * 
     * @param advisories
     */
    public void setAdvisories(com.fedex.ship.stub.Message[] advisories) {
        this.advisories = advisories;
    }

    public com.fedex.ship.stub.Message getAdvisories(int i) {
        return this.advisories[i];
    }

    public void setAdvisories(int i, com.fedex.ship.stub.Message _value) {
        this.advisories[i] = _value;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof RegulatoryWaiver)) return false;
        RegulatoryWaiver other = (RegulatoryWaiver) obj;
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
            ((this.description==null && other.getDescription()==null) || 
             (this.description!=null &&
              this.description.equals(other.getDescription()))) &&
            ((this.advisories==null && other.getAdvisories()==null) || 
             (this.advisories!=null &&
              java.util.Arrays.equals(this.advisories, other.getAdvisories())));
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
        if (getDescription() != null) {
            _hashCode += getDescription().hashCode();
        }
        if (getAdvisories() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getAdvisories());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getAdvisories(), i);
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
        new org.apache.axis.description.TypeDesc(RegulatoryWaiver.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RegulatoryWaiver"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("id");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Id"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("description");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Description"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("advisories");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Advisories"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Message"));
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
