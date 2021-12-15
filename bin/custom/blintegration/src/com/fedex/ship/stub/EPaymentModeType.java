/**
 * EPaymentModeType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class EPaymentModeType implements java.io.Serializable {
    private java.lang.String _value_;
    private static java.util.HashMap _table_ = new java.util.HashMap();

    // Constructor
    protected EPaymentModeType(java.lang.String value) {
        _value_ = value;
        _table_.put(_value_,this);
    }

    public static final java.lang.String _APPLE_PAY = "APPLE_PAY";
    public static final java.lang.String _CASH = "CASH";
    public static final java.lang.String _CHECK = "CHECK";
    public static final java.lang.String _CREDIT_CARD = "CREDIT_CARD";
    public static final java.lang.String _GOOGLE_PAY = "GOOGLE_PAY";
    public static final java.lang.String _PAYPAL = "PAYPAL";
    public static final EPaymentModeType APPLE_PAY = new EPaymentModeType(_APPLE_PAY);
    public static final EPaymentModeType CASH = new EPaymentModeType(_CASH);
    public static final EPaymentModeType CHECK = new EPaymentModeType(_CHECK);
    public static final EPaymentModeType CREDIT_CARD = new EPaymentModeType(_CREDIT_CARD);
    public static final EPaymentModeType GOOGLE_PAY = new EPaymentModeType(_GOOGLE_PAY);
    public static final EPaymentModeType PAYPAL = new EPaymentModeType(_PAYPAL);
    public java.lang.String getValue() { return _value_;}
    public static EPaymentModeType fromValue(java.lang.String value)
          throws java.lang.IllegalArgumentException {
        EPaymentModeType enumeration = (EPaymentModeType)
            _table_.get(value);
        if (enumeration==null) throw new java.lang.IllegalArgumentException();
        return enumeration;
    }
    public static EPaymentModeType fromString(java.lang.String value)
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
        new org.apache.axis.description.TypeDesc(EPaymentModeType.class);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EPaymentModeType"));
    }
    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

}
