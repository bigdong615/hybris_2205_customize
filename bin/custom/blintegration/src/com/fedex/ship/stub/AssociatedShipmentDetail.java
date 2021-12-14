/**
 * AssociatedShipmentDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class AssociatedShipmentDetail  implements java.io.Serializable {
    private com.fedex.ship.stub.AssociatedShipmentType type;

    private com.fedex.ship.stub.Party sender;

    private com.fedex.ship.stub.Party recipient;

    private java.lang.String serviceType;

    private java.lang.String packagingType;

    /* Specifies the tracking id for the payment on the return. */
    private com.fedex.ship.stub.TrackingId trackingId;

    /* Specifies additional customer reference data about the associated
     * shipment. */
    private com.fedex.ship.stub.CustomerReference[] customerReferences;

    /* Specifies shipment level operational information. */
    private com.fedex.ship.stub.ShipmentOperationalDetail shipmentOperationalDetail;

    /* Specifies package level operational information on the associated
     * shipment. This information is not tied to an individual outbound package. */
    private com.fedex.ship.stub.PackageOperationalDetail packageOperationalDetail;

    private com.fedex.ship.stub.ShippingDocument label;

    public AssociatedShipmentDetail() {
    }

    public AssociatedShipmentDetail(
           com.fedex.ship.stub.AssociatedShipmentType type,
           com.fedex.ship.stub.Party sender,
           com.fedex.ship.stub.Party recipient,
           java.lang.String serviceType,
           java.lang.String packagingType,
           com.fedex.ship.stub.TrackingId trackingId,
           com.fedex.ship.stub.CustomerReference[] customerReferences,
           com.fedex.ship.stub.ShipmentOperationalDetail shipmentOperationalDetail,
           com.fedex.ship.stub.PackageOperationalDetail packageOperationalDetail,
           com.fedex.ship.stub.ShippingDocument label) {
           this.type = type;
           this.sender = sender;
           this.recipient = recipient;
           this.serviceType = serviceType;
           this.packagingType = packagingType;
           this.trackingId = trackingId;
           this.customerReferences = customerReferences;
           this.shipmentOperationalDetail = shipmentOperationalDetail;
           this.packageOperationalDetail = packageOperationalDetail;
           this.label = label;
    }


    /**
     * Gets the type value for this AssociatedShipmentDetail.
     * 
     * @return type
     */
    public com.fedex.ship.stub.AssociatedShipmentType getType() {
        return type;
    }


    /**
     * Sets the type value for this AssociatedShipmentDetail.
     * 
     * @param type
     */
    public void setType(com.fedex.ship.stub.AssociatedShipmentType type) {
        this.type = type;
    }


    /**
     * Gets the sender value for this AssociatedShipmentDetail.
     * 
     * @return sender
     */
    public com.fedex.ship.stub.Party getSender() {
        return sender;
    }


    /**
     * Sets the sender value for this AssociatedShipmentDetail.
     * 
     * @param sender
     */
    public void setSender(com.fedex.ship.stub.Party sender) {
        this.sender = sender;
    }


    /**
     * Gets the recipient value for this AssociatedShipmentDetail.
     * 
     * @return recipient
     */
    public com.fedex.ship.stub.Party getRecipient() {
        return recipient;
    }


    /**
     * Sets the recipient value for this AssociatedShipmentDetail.
     * 
     * @param recipient
     */
    public void setRecipient(com.fedex.ship.stub.Party recipient) {
        this.recipient = recipient;
    }


    /**
     * Gets the serviceType value for this AssociatedShipmentDetail.
     * 
     * @return serviceType
     */
    public java.lang.String getServiceType() {
        return serviceType;
    }


    /**
     * Sets the serviceType value for this AssociatedShipmentDetail.
     * 
     * @param serviceType
     */
    public void setServiceType(java.lang.String serviceType) {
        this.serviceType = serviceType;
    }


    /**
     * Gets the packagingType value for this AssociatedShipmentDetail.
     * 
     * @return packagingType
     */
    public java.lang.String getPackagingType() {
        return packagingType;
    }


    /**
     * Sets the packagingType value for this AssociatedShipmentDetail.
     * 
     * @param packagingType
     */
    public void setPackagingType(java.lang.String packagingType) {
        this.packagingType = packagingType;
    }


    /**
     * Gets the trackingId value for this AssociatedShipmentDetail.
     * 
     * @return trackingId   * Specifies the tracking id for the payment on the return.
     */
    public com.fedex.ship.stub.TrackingId getTrackingId() {
        return trackingId;
    }


    /**
     * Sets the trackingId value for this AssociatedShipmentDetail.
     * 
     * @param trackingId   * Specifies the tracking id for the payment on the return.
     */
    public void setTrackingId(com.fedex.ship.stub.TrackingId trackingId) {
        this.trackingId = trackingId;
    }


    /**
     * Gets the customerReferences value for this AssociatedShipmentDetail.
     * 
     * @return customerReferences   * Specifies additional customer reference data about the associated
     * shipment.
     */
    public com.fedex.ship.stub.CustomerReference[] getCustomerReferences() {
        return customerReferences;
    }


    /**
     * Sets the customerReferences value for this AssociatedShipmentDetail.
     * 
     * @param customerReferences   * Specifies additional customer reference data about the associated
     * shipment.
     */
    public void setCustomerReferences(com.fedex.ship.stub.CustomerReference[] customerReferences) {
        this.customerReferences = customerReferences;
    }

    public com.fedex.ship.stub.CustomerReference getCustomerReferences(int i) {
        return this.customerReferences[i];
    }

    public void setCustomerReferences(int i, com.fedex.ship.stub.CustomerReference _value) {
        this.customerReferences[i] = _value;
    }


    /**
     * Gets the shipmentOperationalDetail value for this AssociatedShipmentDetail.
     * 
     * @return shipmentOperationalDetail   * Specifies shipment level operational information.
     */
    public com.fedex.ship.stub.ShipmentOperationalDetail getShipmentOperationalDetail() {
        return shipmentOperationalDetail;
    }


    /**
     * Sets the shipmentOperationalDetail value for this AssociatedShipmentDetail.
     * 
     * @param shipmentOperationalDetail   * Specifies shipment level operational information.
     */
    public void setShipmentOperationalDetail(com.fedex.ship.stub.ShipmentOperationalDetail shipmentOperationalDetail) {
        this.shipmentOperationalDetail = shipmentOperationalDetail;
    }


    /**
     * Gets the packageOperationalDetail value for this AssociatedShipmentDetail.
     * 
     * @return packageOperationalDetail   * Specifies package level operational information on the associated
     * shipment. This information is not tied to an individual outbound package.
     */
    public com.fedex.ship.stub.PackageOperationalDetail getPackageOperationalDetail() {
        return packageOperationalDetail;
    }


    /**
     * Sets the packageOperationalDetail value for this AssociatedShipmentDetail.
     * 
     * @param packageOperationalDetail   * Specifies package level operational information on the associated
     * shipment. This information is not tied to an individual outbound package.
     */
    public void setPackageOperationalDetail(com.fedex.ship.stub.PackageOperationalDetail packageOperationalDetail) {
        this.packageOperationalDetail = packageOperationalDetail;
    }


    /**
     * Gets the label value for this AssociatedShipmentDetail.
     * 
     * @return label
     */
    public com.fedex.ship.stub.ShippingDocument getLabel() {
        return label;
    }


    /**
     * Sets the label value for this AssociatedShipmentDetail.
     * 
     * @param label
     */
    public void setLabel(com.fedex.ship.stub.ShippingDocument label) {
        this.label = label;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof AssociatedShipmentDetail)) return false;
        AssociatedShipmentDetail other = (AssociatedShipmentDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.type==null && other.getType()==null) || 
             (this.type!=null &&
              this.type.equals(other.getType()))) &&
            ((this.sender==null && other.getSender()==null) || 
             (this.sender!=null &&
              this.sender.equals(other.getSender()))) &&
            ((this.recipient==null && other.getRecipient()==null) || 
             (this.recipient!=null &&
              this.recipient.equals(other.getRecipient()))) &&
            ((this.serviceType==null && other.getServiceType()==null) || 
             (this.serviceType!=null &&
              this.serviceType.equals(other.getServiceType()))) &&
            ((this.packagingType==null && other.getPackagingType()==null) || 
             (this.packagingType!=null &&
              this.packagingType.equals(other.getPackagingType()))) &&
            ((this.trackingId==null && other.getTrackingId()==null) || 
             (this.trackingId!=null &&
              this.trackingId.equals(other.getTrackingId()))) &&
            ((this.customerReferences==null && other.getCustomerReferences()==null) || 
             (this.customerReferences!=null &&
              java.util.Arrays.equals(this.customerReferences, other.getCustomerReferences()))) &&
            ((this.shipmentOperationalDetail==null && other.getShipmentOperationalDetail()==null) || 
             (this.shipmentOperationalDetail!=null &&
              this.shipmentOperationalDetail.equals(other.getShipmentOperationalDetail()))) &&
            ((this.packageOperationalDetail==null && other.getPackageOperationalDetail()==null) || 
             (this.packageOperationalDetail!=null &&
              this.packageOperationalDetail.equals(other.getPackageOperationalDetail()))) &&
            ((this.label==null && other.getLabel()==null) || 
             (this.label!=null &&
              this.label.equals(other.getLabel())));
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
        if (getType() != null) {
            _hashCode += getType().hashCode();
        }
        if (getSender() != null) {
            _hashCode += getSender().hashCode();
        }
        if (getRecipient() != null) {
            _hashCode += getRecipient().hashCode();
        }
        if (getServiceType() != null) {
            _hashCode += getServiceType().hashCode();
        }
        if (getPackagingType() != null) {
            _hashCode += getPackagingType().hashCode();
        }
        if (getTrackingId() != null) {
            _hashCode += getTrackingId().hashCode();
        }
        if (getCustomerReferences() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getCustomerReferences());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getCustomerReferences(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getShipmentOperationalDetail() != null) {
            _hashCode += getShipmentOperationalDetail().hashCode();
        }
        if (getPackageOperationalDetail() != null) {
            _hashCode += getPackageOperationalDetail().hashCode();
        }
        if (getLabel() != null) {
            _hashCode += getLabel().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(AssociatedShipmentDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AssociatedShipmentDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("type");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Type"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AssociatedShipmentType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("sender");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Sender"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Party"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("recipient");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Recipient"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Party"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("serviceType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ServiceType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("packagingType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PackagingType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("trackingId");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TrackingId"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TrackingId"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("customerReferences");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomerReferences"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomerReference"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("shipmentOperationalDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipmentOperationalDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipmentOperationalDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("packageOperationalDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PackageOperationalDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PackageOperationalDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("label");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Label"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocument"));
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
