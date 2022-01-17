/**
 * ProcessShipmentReply.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class ProcessShipmentReply  implements java.io.Serializable {
    private com.fedex.ship.stub.NotificationSeverityType highestSeverity;

    private com.fedex.ship.stub.Notification[] notifications;

    private com.fedex.ship.stub.TransactionDetail transactionDetail;

    private com.fedex.ship.stub.VersionId version;

    private java.lang.String jobId;

    private com.fedex.ship.stub.CompletedShipmentDetail completedShipmentDetail;

    /* Empty unless error label behavior is PACKAGE_ERROR_LABELS and
     * one or more errors occured during transaction processing. */
    private com.fedex.ship.stub.ShippingDocument[] errorLabels;

    private com.fedex.ship.stub.ShipmentAdvisoryDetail advisoryDetail;

    public ProcessShipmentReply() {
    }

    public ProcessShipmentReply(
           com.fedex.ship.stub.NotificationSeverityType highestSeverity,
           com.fedex.ship.stub.Notification[] notifications,
           com.fedex.ship.stub.TransactionDetail transactionDetail,
           com.fedex.ship.stub.VersionId version,
           java.lang.String jobId,
           com.fedex.ship.stub.CompletedShipmentDetail completedShipmentDetail,
           com.fedex.ship.stub.ShippingDocument[] errorLabels,
           com.fedex.ship.stub.ShipmentAdvisoryDetail advisoryDetail) {
           this.highestSeverity = highestSeverity;
           this.notifications = notifications;
           this.transactionDetail = transactionDetail;
           this.version = version;
           this.jobId = jobId;
           this.completedShipmentDetail = completedShipmentDetail;
           this.errorLabels = errorLabels;
           this.advisoryDetail = advisoryDetail;
    }


    /**
     * Gets the highestSeverity value for this ProcessShipmentReply.
     * 
     * @return highestSeverity
     */
    public com.fedex.ship.stub.NotificationSeverityType getHighestSeverity() {
        return highestSeverity;
    }


    /**
     * Sets the highestSeverity value for this ProcessShipmentReply.
     * 
     * @param highestSeverity
     */
    public void setHighestSeverity(com.fedex.ship.stub.NotificationSeverityType highestSeverity) {
        this.highestSeverity = highestSeverity;
    }


    /**
     * Gets the notifications value for this ProcessShipmentReply.
     * 
     * @return notifications
     */
    public com.fedex.ship.stub.Notification[] getNotifications() {
        return notifications;
    }


    /**
     * Sets the notifications value for this ProcessShipmentReply.
     * 
     * @param notifications
     */
    public void setNotifications(com.fedex.ship.stub.Notification[] notifications) {
        this.notifications = notifications;
    }

    public com.fedex.ship.stub.Notification getNotifications(int i) {
        return this.notifications[i];
    }

    public void setNotifications(int i, com.fedex.ship.stub.Notification _value) {
        this.notifications[i] = _value;
    }


    /**
     * Gets the transactionDetail value for this ProcessShipmentReply.
     * 
     * @return transactionDetail
     */
    public com.fedex.ship.stub.TransactionDetail getTransactionDetail() {
        return transactionDetail;
    }


    /**
     * Sets the transactionDetail value for this ProcessShipmentReply.
     * 
     * @param transactionDetail
     */
    public void setTransactionDetail(com.fedex.ship.stub.TransactionDetail transactionDetail) {
        this.transactionDetail = transactionDetail;
    }


    /**
     * Gets the version value for this ProcessShipmentReply.
     * 
     * @return version
     */
    public com.fedex.ship.stub.VersionId getVersion() {
        return version;
    }


    /**
     * Sets the version value for this ProcessShipmentReply.
     * 
     * @param version
     */
    public void setVersion(com.fedex.ship.stub.VersionId version) {
        this.version = version;
    }


    /**
     * Gets the jobId value for this ProcessShipmentReply.
     * 
     * @return jobId
     */
    public java.lang.String getJobId() {
        return jobId;
    }


    /**
     * Sets the jobId value for this ProcessShipmentReply.
     * 
     * @param jobId
     */
    public void setJobId(java.lang.String jobId) {
        this.jobId = jobId;
    }


    /**
     * Gets the completedShipmentDetail value for this ProcessShipmentReply.
     * 
     * @return completedShipmentDetail
     */
    public com.fedex.ship.stub.CompletedShipmentDetail getCompletedShipmentDetail() {
        return completedShipmentDetail;
    }


    /**
     * Sets the completedShipmentDetail value for this ProcessShipmentReply.
     * 
     * @param completedShipmentDetail
     */
    public void setCompletedShipmentDetail(com.fedex.ship.stub.CompletedShipmentDetail completedShipmentDetail) {
        this.completedShipmentDetail = completedShipmentDetail;
    }


    /**
     * Gets the errorLabels value for this ProcessShipmentReply.
     * 
     * @return errorLabels   * Empty unless error label behavior is PACKAGE_ERROR_LABELS and
     * one or more errors occured during transaction processing.
     */
    public com.fedex.ship.stub.ShippingDocument[] getErrorLabels() {
        return errorLabels;
    }


    /**
     * Sets the errorLabels value for this ProcessShipmentReply.
     * 
     * @param errorLabels   * Empty unless error label behavior is PACKAGE_ERROR_LABELS and
     * one or more errors occured during transaction processing.
     */
    public void setErrorLabels(com.fedex.ship.stub.ShippingDocument[] errorLabels) {
        this.errorLabels = errorLabels;
    }

    public com.fedex.ship.stub.ShippingDocument getErrorLabels(int i) {
        return this.errorLabels[i];
    }

    public void setErrorLabels(int i, com.fedex.ship.stub.ShippingDocument _value) {
        this.errorLabels[i] = _value;
    }


    /**
     * Gets the advisoryDetail value for this ProcessShipmentReply.
     * 
     * @return advisoryDetail
     */
    public com.fedex.ship.stub.ShipmentAdvisoryDetail getAdvisoryDetail() {
        return advisoryDetail;
    }


    /**
     * Sets the advisoryDetail value for this ProcessShipmentReply.
     * 
     * @param advisoryDetail
     */
    public void setAdvisoryDetail(com.fedex.ship.stub.ShipmentAdvisoryDetail advisoryDetail) {
        this.advisoryDetail = advisoryDetail;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof ProcessShipmentReply)) return false;
        ProcessShipmentReply other = (ProcessShipmentReply) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.highestSeverity==null && other.getHighestSeverity()==null) || 
             (this.highestSeverity!=null &&
              this.highestSeverity.equals(other.getHighestSeverity()))) &&
            ((this.notifications==null && other.getNotifications()==null) || 
             (this.notifications!=null &&
              java.util.Arrays.equals(this.notifications, other.getNotifications()))) &&
            ((this.transactionDetail==null && other.getTransactionDetail()==null) || 
             (this.transactionDetail!=null &&
              this.transactionDetail.equals(other.getTransactionDetail()))) &&
            ((this.version==null && other.getVersion()==null) || 
             (this.version!=null &&
              this.version.equals(other.getVersion()))) &&
            ((this.jobId==null && other.getJobId()==null) || 
             (this.jobId!=null &&
              this.jobId.equals(other.getJobId()))) &&
            ((this.completedShipmentDetail==null && other.getCompletedShipmentDetail()==null) || 
             (this.completedShipmentDetail!=null &&
              this.completedShipmentDetail.equals(other.getCompletedShipmentDetail()))) &&
            ((this.errorLabels==null && other.getErrorLabels()==null) || 
             (this.errorLabels!=null &&
              java.util.Arrays.equals(this.errorLabels, other.getErrorLabels()))) &&
            ((this.advisoryDetail==null && other.getAdvisoryDetail()==null) || 
             (this.advisoryDetail!=null &&
              this.advisoryDetail.equals(other.getAdvisoryDetail())));
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
        if (getHighestSeverity() != null) {
            _hashCode += getHighestSeverity().hashCode();
        }
        if (getNotifications() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getNotifications());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getNotifications(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getTransactionDetail() != null) {
            _hashCode += getTransactionDetail().hashCode();
        }
        if (getVersion() != null) {
            _hashCode += getVersion().hashCode();
        }
        if (getJobId() != null) {
            _hashCode += getJobId().hashCode();
        }
        if (getCompletedShipmentDetail() != null) {
            _hashCode += getCompletedShipmentDetail().hashCode();
        }
        if (getErrorLabels() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getErrorLabels());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getErrorLabels(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getAdvisoryDetail() != null) {
            _hashCode += getAdvisoryDetail().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(ProcessShipmentReply.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ProcessShipmentReply"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("highestSeverity");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "HighestSeverity"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "NotificationSeverityType"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("notifications");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Notifications"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Notification"));
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("transactionDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TransactionDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TransactionDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("version");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Version"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "VersionId"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("jobId");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "JobId"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("completedShipmentDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedShipmentDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedShipmentDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("errorLabels");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ErrorLabels"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocument"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("advisoryDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AdvisoryDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipmentAdvisoryDetail"));
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
