/**
 * GroundDeliveryEligibilityType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class GroundDeliveryEligibilityType implements java.io.Serializable {
    private java.lang.String _value_;
    private static java.util.HashMap _table_ = new java.util.HashMap();

    // Constructor
    protected GroundDeliveryEligibilityType(java.lang.String value) {
        _value_ = value;
        _table_.put(_value_,this);
    }

    public static final java.lang.String _ALTERNATE_DAY_SERVICE = "ALTERNATE_DAY_SERVICE";
    public static final java.lang.String _CARTAGE_AGENT_DELIVERY = "CARTAGE_AGENT_DELIVERY";
    public static final java.lang.String _SATURDAY_DELIVERY = "SATURDAY_DELIVERY";
    public static final java.lang.String _USPS_DELIVERY = "USPS_DELIVERY";
    public static final GroundDeliveryEligibilityType ALTERNATE_DAY_SERVICE = new GroundDeliveryEligibilityType(_ALTERNATE_DAY_SERVICE);
    public static final GroundDeliveryEligibilityType CARTAGE_AGENT_DELIVERY = new GroundDeliveryEligibilityType(_CARTAGE_AGENT_DELIVERY);
    public static final GroundDeliveryEligibilityType SATURDAY_DELIVERY = new GroundDeliveryEligibilityType(_SATURDAY_DELIVERY);
    public static final GroundDeliveryEligibilityType USPS_DELIVERY = new GroundDeliveryEligibilityType(_USPS_DELIVERY);
    public java.lang.String getValue() { return _value_;}
    public static GroundDeliveryEligibilityType fromValue(java.lang.String value)
          throws java.lang.IllegalArgumentException {
        GroundDeliveryEligibilityType enumeration = (GroundDeliveryEligibilityType)
            _table_.get(value);
        if (enumeration==null) throw new java.lang.IllegalArgumentException();
        return enumeration;
    }
    public static GroundDeliveryEligibilityType fromString(java.lang.String value)
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
        new org.apache.axis.description.TypeDesc(GroundDeliveryEligibilityType.class);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "GroundDeliveryEligibilityType"));
    }
    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

}
