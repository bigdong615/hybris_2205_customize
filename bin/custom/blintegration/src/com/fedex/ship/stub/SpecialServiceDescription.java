/**
 * SpecialServiceDescription.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class SpecialServiceDescription  implements java.io.Serializable {
    private com.fedex.ship.stub.OfferingIdentifierDetail identifier;

    private com.fedex.ship.stub.ProductName[] names;

    public SpecialServiceDescription() {
    }

    public SpecialServiceDescription(
           com.fedex.ship.stub.OfferingIdentifierDetail identifier,
           com.fedex.ship.stub.ProductName[] names) {
           this.identifier = identifier;
           this.names = names;
    }


    /**
     * Gets the identifier value for this SpecialServiceDescription.
     * 
     * @return identifier
     */
    public com.fedex.ship.stub.OfferingIdentifierDetail getIdentifier() {
        return identifier;
    }


    /**
     * Sets the identifier value for this SpecialServiceDescription.
     * 
     * @param identifier
     */
    public void setIdentifier(com.fedex.ship.stub.OfferingIdentifierDetail identifier) {
        this.identifier = identifier;
    }


    /**
     * Gets the names value for this SpecialServiceDescription.
     * 
     * @return names
     */
    public com.fedex.ship.stub.ProductName[] getNames() {
        return names;
    }


    /**
     * Sets the names value for this SpecialServiceDescription.
     * 
     * @param names
     */
    public void setNames(com.fedex.ship.stub.ProductName[] names) {
        this.names = names;
    }

    public com.fedex.ship.stub.ProductName getNames(int i) {
        return this.names[i];
    }

    public void setNames(int i, com.fedex.ship.stub.ProductName _value) {
        this.names[i] = _value;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof SpecialServiceDescription)) return false;
        SpecialServiceDescription other = (SpecialServiceDescription) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.identifier==null && other.getIdentifier()==null) || 
             (this.identifier!=null &&
              this.identifier.equals(other.getIdentifier()))) &&
            ((this.names==null && other.getNames()==null) || 
             (this.names!=null &&
              java.util.Arrays.equals(this.names, other.getNames())));
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
        if (getIdentifier() != null) {
            _hashCode += getIdentifier().hashCode();
        }
        if (getNames() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getNames());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getNames(), i);
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
        new org.apache.axis.description.TypeDesc(SpecialServiceDescription.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SpecialServiceDescription"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("identifier");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Identifier"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "OfferingIdentifierDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("names");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Names"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ProductName"));
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
