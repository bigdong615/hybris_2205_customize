/**
 * NotificationEventType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class NotificationEventType implements java.io.Serializable {
    private String _value_;
    private static java.util.HashMap _table_ = new java.util.HashMap();

    // Constructor
    protected NotificationEventType(String value) {
        _value_ = value;
        _table_.put(_value_,this);
    }

    public static final String _ON_DELIVERY = "ON_DELIVERY";
    public static final String _ON_ESTIMATED_DELIVERY = "ON_ESTIMATED_DELIVERY";
    public static final String _ON_EXCEPTION = "ON_EXCEPTION";
    public static final String _ON_PICKUP_DRIVER_ARRIVED = "ON_PICKUP_DRIVER_ARRIVED";
    public static final String _ON_PICKUP_DRIVER_ASSIGNED = "ON_PICKUP_DRIVER_ASSIGNED";
    public static final String _ON_PICKUP_DRIVER_DEPARTED = "ON_PICKUP_DRIVER_DEPARTED";
    public static final String _ON_PICKUP_DRIVER_EN_ROUTE = "ON_PICKUP_DRIVER_EN_ROUTE";
    public static final String _ON_SHIPMENT = "ON_SHIPMENT";
    public static final String _ON_TENDER = "ON_TENDER";
    public static final NotificationEventType ON_DELIVERY = new NotificationEventType(_ON_DELIVERY);
    public static final NotificationEventType ON_ESTIMATED_DELIVERY = new NotificationEventType(_ON_ESTIMATED_DELIVERY);
    public static final NotificationEventType ON_EXCEPTION = new NotificationEventType(_ON_EXCEPTION);
    public static final NotificationEventType ON_PICKUP_DRIVER_ARRIVED = new NotificationEventType(_ON_PICKUP_DRIVER_ARRIVED);
    public static final NotificationEventType ON_PICKUP_DRIVER_ASSIGNED = new NotificationEventType(_ON_PICKUP_DRIVER_ASSIGNED);
    public static final NotificationEventType ON_PICKUP_DRIVER_DEPARTED = new NotificationEventType(_ON_PICKUP_DRIVER_DEPARTED);
    public static final NotificationEventType ON_PICKUP_DRIVER_EN_ROUTE = new NotificationEventType(_ON_PICKUP_DRIVER_EN_ROUTE);
    public static final NotificationEventType ON_SHIPMENT = new NotificationEventType(_ON_SHIPMENT);
    public static final NotificationEventType ON_TENDER = new NotificationEventType(_ON_TENDER);
    public String getValue() { return _value_;}
    public static NotificationEventType fromValue(String value)
          throws IllegalArgumentException {
        NotificationEventType enumeration = (NotificationEventType)
            _table_.get(value);
        if (enumeration==null) throw new IllegalArgumentException();
        return enumeration;
    }
    public static NotificationEventType fromString(String value)
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
        new org.apache.axis.description.TypeDesc(NotificationEventType.class);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "NotificationEventType"));
    }
    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

}
