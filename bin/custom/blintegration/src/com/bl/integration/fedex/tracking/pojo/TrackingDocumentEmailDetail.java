/**
 * TrackingDocumentEmailDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class TrackingDocumentEmailDetail  implements java.io.Serializable {
    /* Specifies the recipients of the email. */
    private  EMailDetail[] recipients;

    /* Identifies the person initiating the email. */
    private  EMailDetail sender;

    /* This is the localization of the email content. */
    private  Localization localization;

    /* A message included in the body of the email. */
    private String personalMessage;

    public TrackingDocumentEmailDetail() {
    }

    public TrackingDocumentEmailDetail(
            EMailDetail[] recipients,
            EMailDetail sender,
            Localization localization,
           String personalMessage) {
           this.recipients = recipients;
           this.sender = sender;
           this.localization = localization;
           this.personalMessage = personalMessage;
    }


    /**
     * Gets the recipients value for this TrackingDocumentEmailDetail.
     *
     * @return recipients   * Specifies the recipients of the email.
     */
    public  EMailDetail[] getRecipients() {
        return recipients;
    }


    /**
     * Sets the recipients value for this TrackingDocumentEmailDetail.
     *
     * @param recipients   * Specifies the recipients of the email.
     */
    public void setRecipients( EMailDetail[] recipients) {
        this.recipients = recipients;
    }

    public  EMailDetail getRecipients(int i) {
        return this.recipients[i];
    }

    public void setRecipients(int i,  EMailDetail _value) {
        this.recipients[i] = _value;
    }


    /**
     * Gets the sender value for this TrackingDocumentEmailDetail.
     *
     * @return sender   * Identifies the person initiating the email.
     */
    public  EMailDetail getSender() {
        return sender;
    }


    /**
     * Sets the sender value for this TrackingDocumentEmailDetail.
     *
     * @param sender   * Identifies the person initiating the email.
     */
    public void setSender( EMailDetail sender) {
        this.sender = sender;
    }


    /**
     * Gets the localization value for this TrackingDocumentEmailDetail.
     *
     * @return localization   * This is the localization of the email content.
     */
    public  Localization getLocalization() {
        return localization;
    }


    /**
     * Sets the localization value for this TrackingDocumentEmailDetail.
     *
     * @param localization   * This is the localization of the email content.
     */
    public void setLocalization( Localization localization) {
        this.localization = localization;
    }


    /**
     * Gets the personalMessage value for this TrackingDocumentEmailDetail.
     *
     * @return personalMessage   * A message included in the body of the email.
     */
    public String getPersonalMessage() {
        return personalMessage;
    }


    /**
     * Sets the personalMessage value for this TrackingDocumentEmailDetail.
     *
     * @param personalMessage   * A message included in the body of the email.
     */
    public void setPersonalMessage(String personalMessage) {
        this.personalMessage = personalMessage;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof TrackingDocumentEmailDetail)) return false;
        TrackingDocumentEmailDetail other = (TrackingDocumentEmailDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.recipients==null && other.getRecipients()==null) ||
             (this.recipients!=null &&
              java.util.Arrays.equals(this.recipients, other.getRecipients()))) &&
            ((this.sender==null && other.getSender()==null) ||
             (this.sender!=null &&
              this.sender.equals(other.getSender()))) &&
            ((this.localization==null && other.getLocalization()==null) ||
             (this.localization!=null &&
              this.localization.equals(other.getLocalization()))) &&
            ((this.personalMessage==null && other.getPersonalMessage()==null) ||
             (this.personalMessage!=null &&
              this.personalMessage.equals(other.getPersonalMessage())));
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
        if (getRecipients() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getRecipients());
                 i++) {
                Object obj = java.lang.reflect.Array.get(getRecipients(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getSender() != null) {
            _hashCode += getSender().hashCode();
        }
        if (getLocalization() != null) {
            _hashCode += getLocalization().hashCode();
        }
        if (getPersonalMessage() != null) {
            _hashCode += getPersonalMessage().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(TrackingDocumentEmailDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDocumentEmailDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("recipients");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Recipients"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "EMailDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("sender");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Sender"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "EMailDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("localization");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Localization"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Localization"));
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
