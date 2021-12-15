/**
 * EMailRecipient.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;


/**
 * Information describing the address of of the email recipient, role
 * of the email recipient and languages that are requested to be supported.
 */
public class EMailRecipient  implements java.io.Serializable {
    /* EMail address of the recipient. */
    private java.lang.String emailAddress;

    /* The relationship that the customer has to the pending shipment. */
    private com.fedex.ship.stub.AccessorRoleType role;

    /* Specifies how the email notification for the pending shipment
     * need to be processed. */
    private com.fedex.ship.stub.EmailOptionsRequested optionsRequested;

    /* Localization and language details specified by the recipient
     * of the EMail. */
    private com.fedex.ship.stub.Localization localization;

    public EMailRecipient() {
    }

    public EMailRecipient(
           java.lang.String emailAddress,
           com.fedex.ship.stub.AccessorRoleType role,
           com.fedex.ship.stub.EmailOptionsRequested optionsRequested,
           com.fedex.ship.stub.Localization localization) {
           this.emailAddress = emailAddress;
           this.role = role;
           this.optionsRequested = optionsRequested;
           this.localization = localization;
    }


    /**
     * Gets the emailAddress value for this EMailRecipient.
     * 
     * @return emailAddress   * EMail address of the recipient.
     */
    public java.lang.String getEmailAddress() {
        return emailAddress;
    }


    /**
     * Sets the emailAddress value for this EMailRecipient.
     * 
     * @param emailAddress   * EMail address of the recipient.
     */
    public void setEmailAddress(java.lang.String emailAddress) {
        this.emailAddress = emailAddress;
    }


    /**
     * Gets the role value for this EMailRecipient.
     * 
     * @return role   * The relationship that the customer has to the pending shipment.
     */
    public com.fedex.ship.stub.AccessorRoleType getRole() {
        return role;
    }


    /**
     * Sets the role value for this EMailRecipient.
     * 
     * @param role   * The relationship that the customer has to the pending shipment.
     */
    public void setRole(com.fedex.ship.stub.AccessorRoleType role) {
        this.role = role;
    }


    /**
     * Gets the optionsRequested value for this EMailRecipient.
     * 
     * @return optionsRequested   * Specifies how the email notification for the pending shipment
     * need to be processed.
     */
    public com.fedex.ship.stub.EmailOptionsRequested getOptionsRequested() {
        return optionsRequested;
    }


    /**
     * Sets the optionsRequested value for this EMailRecipient.
     * 
     * @param optionsRequested   * Specifies how the email notification for the pending shipment
     * need to be processed.
     */
    public void setOptionsRequested(com.fedex.ship.stub.EmailOptionsRequested optionsRequested) {
        this.optionsRequested = optionsRequested;
    }


    /**
     * Gets the localization value for this EMailRecipient.
     * 
     * @return localization   * Localization and language details specified by the recipient
     * of the EMail.
     */
    public com.fedex.ship.stub.Localization getLocalization() {
        return localization;
    }


    /**
     * Sets the localization value for this EMailRecipient.
     * 
     * @param localization   * Localization and language details specified by the recipient
     * of the EMail.
     */
    public void setLocalization(com.fedex.ship.stub.Localization localization) {
        this.localization = localization;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof EMailRecipient)) return false;
        EMailRecipient other = (EMailRecipient) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.emailAddress==null && other.getEmailAddress()==null) || 
             (this.emailAddress!=null &&
              this.emailAddress.equals(other.getEmailAddress()))) &&
            ((this.role==null && other.getRole()==null) || 
             (this.role!=null &&
              this.role.equals(other.getRole()))) &&
            ((this.optionsRequested==null && other.getOptionsRequested()==null) || 
             (this.optionsRequested!=null &&
              this.optionsRequested.equals(other.getOptionsRequested()))) &&
            ((this.localization==null && other.getLocalization()==null) || 
             (this.localization!=null &&
              this.localization.equals(other.getLocalization())));
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
        if (getEmailAddress() != null) {
            _hashCode += getEmailAddress().hashCode();
        }
        if (getRole() != null) {
            _hashCode += getRole().hashCode();
        }
        if (getOptionsRequested() != null) {
            _hashCode += getOptionsRequested().hashCode();
        }
        if (getLocalization() != null) {
            _hashCode += getLocalization().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(EMailRecipient.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EMailRecipient"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("emailAddress");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EmailAddress"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("role");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Role"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AccessorRoleType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("optionsRequested");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "OptionsRequested"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EmailOptionsRequested"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("localization");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Localization"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Localization"));
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
