/**
 * BarcodeEntryType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class BarcodeEntryType implements java.io.Serializable {
    private String _value_;
    private static java.util.HashMap _table_ = new java.util.HashMap();

    // Constructor
    protected BarcodeEntryType(String value) {
        _value_ = value;
        _table_.put(_value_,this);
    }

    public static final String _MANUAL_ENTRY = "MANUAL_ENTRY";
    public static final String _SCAN = "SCAN";
    public static final BarcodeEntryType MANUAL_ENTRY = new BarcodeEntryType(_MANUAL_ENTRY);
    public static final BarcodeEntryType SCAN = new BarcodeEntryType(_SCAN);
    public String getValue() { return _value_;}
    public static BarcodeEntryType fromValue(String value)
          throws IllegalArgumentException {
        BarcodeEntryType enumeration = (BarcodeEntryType)
            _table_.get(value);
        if (enumeration==null) throw new IllegalArgumentException();
        return enumeration;
    }
    public static BarcodeEntryType fromString(String value)
          throws IllegalArgumentException {
        return fromValue(value);
    }
    public boolean equals(Object obj) {return (obj == this);}
    public int hashCode() { return toString().hashCode();}
    public String toString() { return _value_;}
    public Object readResolve() throws java.io.ObjectStreamException { return fromValue(_value_);}
    public static org.apache.axis.encoding.Serializer getSerializer(
           String mechType,
           Class _javaType,
           javax.xml.namespace.QName _xmlType) {
        return
          new org.apache.axis.encoding.ser.EnumSerializer(
            _javaType, _xmlType);
    }
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           String mechType,
           Class _javaType,
           javax.xml.namespace.QName _xmlType) {
        return 
          new org.apache.axis.encoding.ser.EnumDeserializer(
            _javaType, _xmlType);
    }
    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(BarcodeEntryType.class);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "BarcodeEntryType"));
    }
    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

}
