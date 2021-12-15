/**
 * ShippingDocumentDispositionDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;


/**
 * Each occurrence of this class specifies a particular way in which
 * a kind of shipping document is to be produced and provided.
 */
public class ShippingDocumentDispositionDetail  implements java.io.Serializable {
    /* Values in this field specify how to create and return the document. */
    private com.fedex.ship.stub.ShippingDocumentDispositionType dispositionType;

    /* Specifies how to organize all documents of this type. */
    private com.fedex.ship.stub.ShippingDocumentGroupingType grouping;

    /* Specifies how to store document images. */
    private com.fedex.ship.stub.ShippingDocumentStorageDetail storageDetail;

    /* Specifies how to e-mail document images. */
    private com.fedex.ship.stub.ShippingDocumentEMailDetail EMailDetail;

    /* Specifies how a queued document is to be printed. */
    private com.fedex.ship.stub.ShippingDocumentPrintDetail printDetail;

    public ShippingDocumentDispositionDetail() {
    }

    public ShippingDocumentDispositionDetail(
           com.fedex.ship.stub.ShippingDocumentDispositionType dispositionType,
           com.fedex.ship.stub.ShippingDocumentGroupingType grouping,
           com.fedex.ship.stub.ShippingDocumentStorageDetail storageDetail,
           com.fedex.ship.stub.ShippingDocumentEMailDetail EMailDetail,
           com.fedex.ship.stub.ShippingDocumentPrintDetail printDetail) {
           this.dispositionType = dispositionType;
           this.grouping = grouping;
           this.storageDetail = storageDetail;
           this.EMailDetail = EMailDetail;
           this.printDetail = printDetail;
    }


    /**
     * Gets the dispositionType value for this ShippingDocumentDispositionDetail.
     * 
     * @return dispositionType   * Values in this field specify how to create and return the document.
     */
    public com.fedex.ship.stub.ShippingDocumentDispositionType getDispositionType() {
        return dispositionType;
    }


    /**
     * Sets the dispositionType value for this ShippingDocumentDispositionDetail.
     * 
     * @param dispositionType   * Values in this field specify how to create and return the document.
     */
    public void setDispositionType(com.fedex.ship.stub.ShippingDocumentDispositionType dispositionType) {
        this.dispositionType = dispositionType;
    }


    /**
     * Gets the grouping value for this ShippingDocumentDispositionDetail.
     * 
     * @return grouping   * Specifies how to organize all documents of this type.
     */
    public com.fedex.ship.stub.ShippingDocumentGroupingType getGrouping() {
        return grouping;
    }


    /**
     * Sets the grouping value for this ShippingDocumentDispositionDetail.
     * 
     * @param grouping   * Specifies how to organize all documents of this type.
     */
    public void setGrouping(com.fedex.ship.stub.ShippingDocumentGroupingType grouping) {
        this.grouping = grouping;
    }


    /**
     * Gets the storageDetail value for this ShippingDocumentDispositionDetail.
     * 
     * @return storageDetail   * Specifies how to store document images.
     */
    public com.fedex.ship.stub.ShippingDocumentStorageDetail getStorageDetail() {
        return storageDetail;
    }


    /**
     * Sets the storageDetail value for this ShippingDocumentDispositionDetail.
     * 
     * @param storageDetail   * Specifies how to store document images.
     */
    public void setStorageDetail(com.fedex.ship.stub.ShippingDocumentStorageDetail storageDetail) {
        this.storageDetail = storageDetail;
    }


    /**
     * Gets the EMailDetail value for this ShippingDocumentDispositionDetail.
     * 
     * @return EMailDetail   * Specifies how to e-mail document images.
     */
    public com.fedex.ship.stub.ShippingDocumentEMailDetail getEMailDetail() {
        return EMailDetail;
    }


    /**
     * Sets the EMailDetail value for this ShippingDocumentDispositionDetail.
     * 
     * @param EMailDetail   * Specifies how to e-mail document images.
     */
    public void setEMailDetail(com.fedex.ship.stub.ShippingDocumentEMailDetail EMailDetail) {
        this.EMailDetail = EMailDetail;
    }


    /**
     * Gets the printDetail value for this ShippingDocumentDispositionDetail.
     * 
     * @return printDetail   * Specifies how a queued document is to be printed.
     */
    public com.fedex.ship.stub.ShippingDocumentPrintDetail getPrintDetail() {
        return printDetail;
    }


    /**
     * Sets the printDetail value for this ShippingDocumentDispositionDetail.
     * 
     * @param printDetail   * Specifies how a queued document is to be printed.
     */
    public void setPrintDetail(com.fedex.ship.stub.ShippingDocumentPrintDetail printDetail) {
        this.printDetail = printDetail;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof ShippingDocumentDispositionDetail)) return false;
        ShippingDocumentDispositionDetail other = (ShippingDocumentDispositionDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.dispositionType==null && other.getDispositionType()==null) || 
             (this.dispositionType!=null &&
              this.dispositionType.equals(other.getDispositionType()))) &&
            ((this.grouping==null && other.getGrouping()==null) || 
             (this.grouping!=null &&
              this.grouping.equals(other.getGrouping()))) &&
            ((this.storageDetail==null && other.getStorageDetail()==null) || 
             (this.storageDetail!=null &&
              this.storageDetail.equals(other.getStorageDetail()))) &&
            ((this.EMailDetail==null && other.getEMailDetail()==null) || 
             (this.EMailDetail!=null &&
              this.EMailDetail.equals(other.getEMailDetail()))) &&
            ((this.printDetail==null && other.getPrintDetail()==null) || 
             (this.printDetail!=null &&
              this.printDetail.equals(other.getPrintDetail())));
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
        if (getDispositionType() != null) {
            _hashCode += getDispositionType().hashCode();
        }
        if (getGrouping() != null) {
            _hashCode += getGrouping().hashCode();
        }
        if (getStorageDetail() != null) {
            _hashCode += getStorageDetail().hashCode();
        }
        if (getEMailDetail() != null) {
            _hashCode += getEMailDetail().hashCode();
        }
        if (getPrintDetail() != null) {
            _hashCode += getPrintDetail().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(ShippingDocumentDispositionDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentDispositionDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dispositionType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DispositionType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentDispositionType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("grouping");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Grouping"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentGroupingType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("storageDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "StorageDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentStorageDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("EMailDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EMailDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentEMailDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("printDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PrintDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentPrintDetail"));
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
