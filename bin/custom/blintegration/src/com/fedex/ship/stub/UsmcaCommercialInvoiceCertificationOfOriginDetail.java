/**
 * UsmcaCommercialInvoiceCertificationOfOriginDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class UsmcaCommercialInvoiceCertificationOfOriginDetail  implements java.io.Serializable {
    private com.fedex.ship.stub.ShippingDocumentFormat format;

    private com.fedex.ship.stub.CustomerImageUsage[] customerImageUsages;

    private java.lang.String formVersion;

    private com.fedex.ship.stub.UsmcaCertifierSpecificationType certifierSpecification;

    private com.fedex.ship.stub.UsmcaProducerSpecificationType producerSpecification;

    private com.fedex.ship.stub.Party producer;

    public UsmcaCommercialInvoiceCertificationOfOriginDetail() {
    }

    public UsmcaCommercialInvoiceCertificationOfOriginDetail(
           com.fedex.ship.stub.ShippingDocumentFormat format,
           com.fedex.ship.stub.CustomerImageUsage[] customerImageUsages,
           java.lang.String formVersion,
           com.fedex.ship.stub.UsmcaCertifierSpecificationType certifierSpecification,
           com.fedex.ship.stub.UsmcaProducerSpecificationType producerSpecification,
           com.fedex.ship.stub.Party producer) {
           this.format = format;
           this.customerImageUsages = customerImageUsages;
           this.formVersion = formVersion;
           this.certifierSpecification = certifierSpecification;
           this.producerSpecification = producerSpecification;
           this.producer = producer;
    }


    /**
     * Gets the format value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @return format
     */
    public com.fedex.ship.stub.ShippingDocumentFormat getFormat() {
        return format;
    }


    /**
     * Sets the format value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @param format
     */
    public void setFormat(com.fedex.ship.stub.ShippingDocumentFormat format) {
        this.format = format;
    }


    /**
     * Gets the customerImageUsages value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @return customerImageUsages
     */
    public com.fedex.ship.stub.CustomerImageUsage[] getCustomerImageUsages() {
        return customerImageUsages;
    }


    /**
     * Sets the customerImageUsages value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @param customerImageUsages
     */
    public void setCustomerImageUsages(com.fedex.ship.stub.CustomerImageUsage[] customerImageUsages) {
        this.customerImageUsages = customerImageUsages;
    }

    public com.fedex.ship.stub.CustomerImageUsage getCustomerImageUsages(int i) {
        return this.customerImageUsages[i];
    }

    public void setCustomerImageUsages(int i, com.fedex.ship.stub.CustomerImageUsage _value) {
        this.customerImageUsages[i] = _value;
    }


    /**
     * Gets the formVersion value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @return formVersion
     */
    public java.lang.String getFormVersion() {
        return formVersion;
    }


    /**
     * Sets the formVersion value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @param formVersion
     */
    public void setFormVersion(java.lang.String formVersion) {
        this.formVersion = formVersion;
    }


    /**
     * Gets the certifierSpecification value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @return certifierSpecification
     */
    public com.fedex.ship.stub.UsmcaCertifierSpecificationType getCertifierSpecification() {
        return certifierSpecification;
    }


    /**
     * Sets the certifierSpecification value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @param certifierSpecification
     */
    public void setCertifierSpecification(com.fedex.ship.stub.UsmcaCertifierSpecificationType certifierSpecification) {
        this.certifierSpecification = certifierSpecification;
    }


    /**
     * Gets the producerSpecification value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @return producerSpecification
     */
    public com.fedex.ship.stub.UsmcaProducerSpecificationType getProducerSpecification() {
        return producerSpecification;
    }


    /**
     * Sets the producerSpecification value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @param producerSpecification
     */
    public void setProducerSpecification(com.fedex.ship.stub.UsmcaProducerSpecificationType producerSpecification) {
        this.producerSpecification = producerSpecification;
    }


    /**
     * Gets the producer value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @return producer
     */
    public com.fedex.ship.stub.Party getProducer() {
        return producer;
    }


    /**
     * Sets the producer value for this UsmcaCommercialInvoiceCertificationOfOriginDetail.
     * 
     * @param producer
     */
    public void setProducer(com.fedex.ship.stub.Party producer) {
        this.producer = producer;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof UsmcaCommercialInvoiceCertificationOfOriginDetail)) return false;
        UsmcaCommercialInvoiceCertificationOfOriginDetail other = (UsmcaCommercialInvoiceCertificationOfOriginDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.format==null && other.getFormat()==null) || 
             (this.format!=null &&
              this.format.equals(other.getFormat()))) &&
            ((this.customerImageUsages==null && other.getCustomerImageUsages()==null) || 
             (this.customerImageUsages!=null &&
              java.util.Arrays.equals(this.customerImageUsages, other.getCustomerImageUsages()))) &&
            ((this.formVersion==null && other.getFormVersion()==null) || 
             (this.formVersion!=null &&
              this.formVersion.equals(other.getFormVersion()))) &&
            ((this.certifierSpecification==null && other.getCertifierSpecification()==null) || 
             (this.certifierSpecification!=null &&
              this.certifierSpecification.equals(other.getCertifierSpecification()))) &&
            ((this.producerSpecification==null && other.getProducerSpecification()==null) || 
             (this.producerSpecification!=null &&
              this.producerSpecification.equals(other.getProducerSpecification()))) &&
            ((this.producer==null && other.getProducer()==null) || 
             (this.producer!=null &&
              this.producer.equals(other.getProducer())));
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
        if (getFormat() != null) {
            _hashCode += getFormat().hashCode();
        }
        if (getCustomerImageUsages() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getCustomerImageUsages());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getCustomerImageUsages(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getFormVersion() != null) {
            _hashCode += getFormVersion().hashCode();
        }
        if (getCertifierSpecification() != null) {
            _hashCode += getCertifierSpecification().hashCode();
        }
        if (getProducerSpecification() != null) {
            _hashCode += getProducerSpecification().hashCode();
        }
        if (getProducer() != null) {
            _hashCode += getProducer().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(UsmcaCommercialInvoiceCertificationOfOriginDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UsmcaCommercialInvoiceCertificationOfOriginDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("format");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Format"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentFormat"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("customerImageUsages");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomerImageUsages"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomerImageUsage"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("formVersion");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FormVersion"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("certifierSpecification");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CertifierSpecification"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UsmcaCertifierSpecificationType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("producerSpecification");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ProducerSpecification"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UsmcaProducerSpecificationType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("producer");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Producer"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Party"));
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
