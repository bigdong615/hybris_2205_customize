/**
 * CompletedEtdType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class CompletedEtdType implements java.io.Serializable {
    private java.lang.String _value_;
    private static java.util.HashMap _table_ = new java.util.HashMap();

    // Constructor
    protected CompletedEtdType(java.lang.String value) {
        _value_ = value;
        _table_.put(_value_,this);
    }

    public static final java.lang.String _ELECTRONIC_DOCUMENTS_ONLY = "ELECTRONIC_DOCUMENTS_ONLY";
    public static final java.lang.String _ELECTRONIC_DOCUMENTS_WITH_ORIGINALS = "ELECTRONIC_DOCUMENTS_WITH_ORIGINALS";
    public static final CompletedEtdType ELECTRONIC_DOCUMENTS_ONLY = new CompletedEtdType(_ELECTRONIC_DOCUMENTS_ONLY);
    public static final CompletedEtdType ELECTRONIC_DOCUMENTS_WITH_ORIGINALS = new CompletedEtdType(_ELECTRONIC_DOCUMENTS_WITH_ORIGINALS);
    public java.lang.String getValue() { return _value_;}
    public static CompletedEtdType fromValue(java.lang.String value)
          throws java.lang.IllegalArgumentException {
        CompletedEtdType enumeration = (CompletedEtdType)
            _table_.get(value);
        if (enumeration==null) throw new java.lang.IllegalArgumentException();
        return enumeration;
    }
    public static CompletedEtdType fromString(java.lang.String value)
          throws java.lang.IllegalArgumentException {
        return fromValue(value);
    }
    public boolean equals(java.lang.Object obj) {return (obj == this);}
    public int hashCode() { return toString().hashCode();}
    public java.lang.String toString() { return _value_;}
    public java.lang.Object readResolve() throws java.io.ObjectStreamException { return fromValue(_value_);}
    public static org.apache.axis.encoding.Serializer getSerializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new org.apache.axis.encoding.ser.EnumSerializer(
            _javaType, _xmlType);
    }
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new org.apache.axis.encoding.ser.EnumDeserializer(
            _javaType, _xmlType);
    }
    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(CompletedEtdType.class);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedEtdType"));
    }
    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

}
