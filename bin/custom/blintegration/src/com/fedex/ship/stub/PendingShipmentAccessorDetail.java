/**
 * PendingShipmentAccessorDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;


/**
 * Specifies the details to be used by the user of the pending shipment
 */
public class PendingShipmentAccessorDetail  implements java.io.Serializable {
    /* Specifies the role of the user who is trying to access the
     * pending shipment. */
    private com.fedex.ship.stub.AccessorRoleType role;

    private java.lang.String userId;

    private java.lang.String password;

    private java.lang.String emailLabelUrl;

    public PendingShipmentAccessorDetail() {
    }

    public PendingShipmentAccessorDetail(
           com.fedex.ship.stub.AccessorRoleType role,
           java.lang.String userId,
           java.lang.String password,
           java.lang.String emailLabelUrl) {
           this.role = role;
           this.userId = userId;
           this.password = password;
           this.emailLabelUrl = emailLabelUrl;
    }


    /**
     * Gets the role value for this PendingShipmentAccessorDetail.
     * 
     * @return role   * Specifies the role of the user who is trying to access the
     * pending shipment.
     */
    public com.fedex.ship.stub.AccessorRoleType getRole() {
        return role;
    }


    /**
     * Sets the role value for this PendingShipmentAccessorDetail.
     * 
     * @param role   * Specifies the role of the user who is trying to access the
     * pending shipment.
     */
    public void setRole(com.fedex.ship.stub.AccessorRoleType role) {
        this.role = role;
    }


    /**
     * Gets the userId value for this PendingShipmentAccessorDetail.
     * 
     * @return userId
     */
    public java.lang.String getUserId() {
        return userId;
    }


    /**
     * Sets the userId value for this PendingShipmentAccessorDetail.
     * 
     * @param userId
     */
    public void setUserId(java.lang.String userId) {
        this.userId = userId;
    }


    /**
     * Gets the password value for this PendingShipmentAccessorDetail.
     * 
     * @return password
     */
    public java.lang.String getPassword() {
        return password;
    }


    /**
     * Sets the password value for this PendingShipmentAccessorDetail.
     * 
     * @param password
     */
    public void setPassword(java.lang.String password) {
        this.password = password;
    }


    /**
     * Gets the emailLabelUrl value for this PendingShipmentAccessorDetail.
     * 
     * @return emailLabelUrl
     */
    public java.lang.String getEmailLabelUrl() {
        return emailLabelUrl;
    }


    /**
     * Sets the emailLabelUrl value for this PendingShipmentAccessorDetail.
     * 
     * @param emailLabelUrl
     */
    public void setEmailLabelUrl(java.lang.String emailLabelUrl) {
        this.emailLabelUrl = emailLabelUrl;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof PendingShipmentAccessorDetail)) return false;
        PendingShipmentAccessorDetail other = (PendingShipmentAccessorDetail) obj;
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
            ((this.userId==null && other.getUserId()==null) || 
             (this.userId!=null &&
              this.userId.equals(other.getUserId()))) &&
            ((this.password==null && other.getPassword()==null) || 
             (this.password!=null &&
              this.password.equals(other.getPassword()))) &&
            ((this.emailLabelUrl==null && other.getEmailLabelUrl()==null) || 
             (this.emailLabelUrl!=null &&
              this.emailLabelUrl.equals(other.getEmailLabelUrl())));
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
        if (getUserId() != null) {
            _hashCode += getUserId().hashCode();
        }
        if (getPassword() != null) {
            _hashCode += getPassword().hashCode();
        }
        if (getEmailLabelUrl() != null) {
            _hashCode += getEmailLabelUrl().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(PendingShipmentAccessorDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PendingShipmentAccessorDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("role");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Role"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AccessorRoleType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("userId");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UserId"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("password");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Password"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("emailLabelUrl");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EmailLabelUrl"));
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
