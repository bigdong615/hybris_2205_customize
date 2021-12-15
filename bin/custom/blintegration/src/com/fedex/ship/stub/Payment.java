/**
 * Payment.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class Payment  implements java.io.Serializable {
    private com.fedex.ship.stub.PaymentType paymentType;

    private com.fedex.ship.stub.Payor payor;

    /* FOR FEDEX INTERNAL USE ONLY */
    private com.fedex.ship.stub.EPaymentDetail EPaymentDetail;

    public Payment() {
    }

    public Payment(
           com.fedex.ship.stub.PaymentType paymentType,
           com.fedex.ship.stub.Payor payor,
           com.fedex.ship.stub.EPaymentDetail EPaymentDetail) {
           this.paymentType = paymentType;
           this.payor = payor;
           this.EPaymentDetail = EPaymentDetail;
    }


    /**
     * Gets the paymentType value for this Payment.
     * 
     * @return paymentType
     */
    public com.fedex.ship.stub.PaymentType getPaymentType() {
        return paymentType;
    }


    /**
     * Sets the paymentType value for this Payment.
     * 
     * @param paymentType
     */
    public void setPaymentType(com.fedex.ship.stub.PaymentType paymentType) {
        this.paymentType = paymentType;
    }


    /**
     * Gets the payor value for this Payment.
     * 
     * @return payor
     */
    public com.fedex.ship.stub.Payor getPayor() {
        return payor;
    }


    /**
     * Sets the payor value for this Payment.
     * 
     * @param payor
     */
    public void setPayor(com.fedex.ship.stub.Payor payor) {
        this.payor = payor;
    }


    /**
     * Gets the EPaymentDetail value for this Payment.
     * 
     * @return EPaymentDetail   * FOR FEDEX INTERNAL USE ONLY
     */
    public com.fedex.ship.stub.EPaymentDetail getEPaymentDetail() {
        return EPaymentDetail;
    }


    /**
     * Sets the EPaymentDetail value for this Payment.
     * 
     * @param EPaymentDetail   * FOR FEDEX INTERNAL USE ONLY
     */
    public void setEPaymentDetail(com.fedex.ship.stub.EPaymentDetail EPaymentDetail) {
        this.EPaymentDetail = EPaymentDetail;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof Payment)) return false;
        Payment other = (Payment) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.paymentType==null && other.getPaymentType()==null) || 
             (this.paymentType!=null &&
              this.paymentType.equals(other.getPaymentType()))) &&
            ((this.payor==null && other.getPayor()==null) || 
             (this.payor!=null &&
              this.payor.equals(other.getPayor()))) &&
            ((this.EPaymentDetail==null && other.getEPaymentDetail()==null) || 
             (this.EPaymentDetail!=null &&
              this.EPaymentDetail.equals(other.getEPaymentDetail())));
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
        if (getPaymentType() != null) {
            _hashCode += getPaymentType().hashCode();
        }
        if (getPayor() != null) {
            _hashCode += getPayor().hashCode();
        }
        if (getEPaymentDetail() != null) {
            _hashCode += getEPaymentDetail().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(Payment.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Payment"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("paymentType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PaymentType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PaymentType"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("payor");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Payor"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Payor"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("EPaymentDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EPaymentDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EPaymentDetail"));
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
