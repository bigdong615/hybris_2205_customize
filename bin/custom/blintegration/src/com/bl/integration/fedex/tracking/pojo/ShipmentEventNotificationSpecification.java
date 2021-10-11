/**
 * ShipmentEventNotificationSpecification.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class ShipmentEventNotificationSpecification  implements java.io.Serializable {
    private  ShipmentNotificationRoleType role;

    private  NotificationEventType[] events;

    private  NotificationDetail notificationDetail;

    private  ShipmentNotificationFormatSpecification formatSpecification;

    public ShipmentEventNotificationSpecification() {
    }

    public ShipmentEventNotificationSpecification(
            ShipmentNotificationRoleType role,
            NotificationEventType[] events,
            NotificationDetail notificationDetail,
            ShipmentNotificationFormatSpecification formatSpecification) {
           this.role = role;
           this.events = events;
           this.notificationDetail = notificationDetail;
           this.formatSpecification = formatSpecification;
    }


    /**
     * Gets the role value for this ShipmentEventNotificationSpecification.
     * 
     * @return role
     */
    public  ShipmentNotificationRoleType getRole() {
        return role;
    }


    /**
     * Sets the role value for this ShipmentEventNotificationSpecification.
     * 
     * @param role
     */
    public void setRole( ShipmentNotificationRoleType role) {
        this.role = role;
    }


    /**
     * Gets the events value for this ShipmentEventNotificationSpecification.
     * 
     * @return events
     */
    public  NotificationEventType[] getEvents() {
        return events;
    }


    /**
     * Sets the events value for this ShipmentEventNotificationSpecification.
     * 
     * @param events
     */
    public void setEvents( NotificationEventType[] events) {
        this.events = events;
    }

    public  NotificationEventType getEvents(int i) {
        return this.events[i];
    }

    public void setEvents(int i,  NotificationEventType _value) {
        this.events[i] = _value;
    }


    /**
     * Gets the notificationDetail value for this ShipmentEventNotificationSpecification.
     * 
     * @return notificationDetail
     */
    public  NotificationDetail getNotificationDetail() {
        return notificationDetail;
    }


    /**
     * Sets the notificationDetail value for this ShipmentEventNotificationSpecification.
     * 
     * @param notificationDetail
     */
    public void setNotificationDetail( NotificationDetail notificationDetail) {
        this.notificationDetail = notificationDetail;
    }


    /**
     * Gets the formatSpecification value for this ShipmentEventNotificationSpecification.
     * 
     * @return formatSpecification
     */
    public  ShipmentNotificationFormatSpecification getFormatSpecification() {
        return formatSpecification;
    }


    /**
     * Sets the formatSpecification value for this ShipmentEventNotificationSpecification.
     * 
     * @param formatSpecification
     */
    public void setFormatSpecification( ShipmentNotificationFormatSpecification formatSpecification) {
        this.formatSpecification = formatSpecification;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof ShipmentEventNotificationSpecification)) return false;
        ShipmentEventNotificationSpecification other = (ShipmentEventNotificationSpecification) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.role==null && other.getRole()==null) ||
             (this.role!=null &&
              this.role.equals(other.getRole()))) &&
            ((this.events==null && other.getEvents()==null) ||
             (this.events!=null &&
              java.util.Arrays.equals(this.events, other.getEvents()))) &&
            ((this.notificationDetail==null && other.getNotificationDetail()==null) ||
             (this.notificationDetail!=null &&
              this.notificationDetail.equals(other.getNotificationDetail()))) &&
            ((this.formatSpecification==null && other.getFormatSpecification()==null) ||
             (this.formatSpecification!=null &&
              this.formatSpecification.equals(other.getFormatSpecification())));
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
        if (getRole() != null) {
            _hashCode += getRole().hashCode();
        }
        if (getEvents() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getEvents());
                 i++) {
                Object obj = java.lang.reflect.Array.get(getEvents(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getNotificationDetail() != null) {
            _hashCode += getNotificationDetail().hashCode();
        }
        if (getFormatSpecification() != null) {
            _hashCode += getFormatSpecification().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(ShipmentEventNotificationSpecification.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ShipmentEventNotificationSpecification"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("role");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Role"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ShipmentNotificationRoleType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("events");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Events"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "NotificationEventType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("notificationDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "NotificationDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "NotificationDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("formatSpecification");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "FormatSpecification"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ShipmentNotificationFormatSpecification"));
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
           String mechType,
           Class _javaType,
           javax.xml.namespace.QName _xmlType) {
        return
          new  org.apache.axis.encoding.ser.BeanSerializer(
            _javaType, _xmlType, typeDesc);
    }

    /**
     * Get Custom Deserializer
     */
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           String mechType,
           Class _javaType,
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanDeserializer(
            _javaType, _xmlType, typeDesc);
    }

}
