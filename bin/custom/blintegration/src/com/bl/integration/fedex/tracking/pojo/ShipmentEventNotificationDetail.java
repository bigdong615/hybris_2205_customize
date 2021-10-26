/**
 * ShipmentEventNotificationDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class ShipmentEventNotificationDetail  implements java.io.Serializable {
    private  ShipmentNotificationAggregationType aggregationType;

    private String personalMessage;

    private  ShipmentEventNotificationSpecification[] eventNotifications;

    public ShipmentEventNotificationDetail() {
    }

    public ShipmentEventNotificationDetail(
            ShipmentNotificationAggregationType aggregationType,
           String personalMessage,
            ShipmentEventNotificationSpecification[] eventNotifications) {
           this.aggregationType = aggregationType;
           this.personalMessage = personalMessage;
           this.eventNotifications = eventNotifications;
    }


    /**
     * Gets the aggregationType value for this ShipmentEventNotificationDetail.
     *
     * @return aggregationType
     */
    public  ShipmentNotificationAggregationType getAggregationType() {
        return aggregationType;
    }


    /**
     * Sets the aggregationType value for this ShipmentEventNotificationDetail.
     *
     * @param aggregationType
     */
    public void setAggregationType( ShipmentNotificationAggregationType aggregationType) {
        this.aggregationType = aggregationType;
    }


    /**
     * Gets the personalMessage value for this ShipmentEventNotificationDetail.
     *
     * @return personalMessage
     */
    public String getPersonalMessage() {
        return personalMessage;
    }


    /**
     * Sets the personalMessage value for this ShipmentEventNotificationDetail.
     *
     * @param personalMessage
     */
    public void setPersonalMessage(String personalMessage) {
        this.personalMessage = personalMessage;
    }


    /**
     * Gets the eventNotifications value for this ShipmentEventNotificationDetail.
     *
     * @return eventNotifications
     */
    public  ShipmentEventNotificationSpecification[] getEventNotifications() {
        return eventNotifications;
    }


    /**
     * Sets the eventNotifications value for this ShipmentEventNotificationDetail.
     *
     * @param eventNotifications
     */
    public void setEventNotifications( ShipmentEventNotificationSpecification[] eventNotifications) {
        this.eventNotifications = eventNotifications;
    }

    public  ShipmentEventNotificationSpecification getEventNotifications(int i) {
        return this.eventNotifications[i];
    }

    public void setEventNotifications(int i,  ShipmentEventNotificationSpecification _value) {
        this.eventNotifications[i] = _value;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof ShipmentEventNotificationDetail)) return false;
        ShipmentEventNotificationDetail other = (ShipmentEventNotificationDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.aggregationType==null && other.getAggregationType()==null) ||
             (this.aggregationType!=null &&
              this.aggregationType.equals(other.getAggregationType()))) &&
            ((this.personalMessage==null && other.getPersonalMessage()==null) ||
             (this.personalMessage!=null &&
              this.personalMessage.equals(other.getPersonalMessage()))) &&
            ((this.eventNotifications==null && other.getEventNotifications()==null) ||
             (this.eventNotifications!=null &&
              java.util.Arrays.equals(this.eventNotifications, other.getEventNotifications())));
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
        if (getAggregationType() != null) {
            _hashCode += getAggregationType().hashCode();
        }
        if (getPersonalMessage() != null) {
            _hashCode += getPersonalMessage().hashCode();
        }
        if (getEventNotifications() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getEventNotifications());
                 i++) {
                Object obj = java.lang.reflect.Array.get(getEventNotifications(), i);
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
        new org.apache.axis.description.TypeDesc(ShipmentEventNotificationDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ShipmentEventNotificationDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("aggregationType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "AggregationType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ShipmentNotificationAggregationType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("personalMessage");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "PersonalMessage"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("eventNotifications");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "EventNotifications"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ShipmentEventNotificationSpecification"));
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
