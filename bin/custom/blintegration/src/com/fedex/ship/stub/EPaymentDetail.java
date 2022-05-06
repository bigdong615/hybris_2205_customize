/**
 * EPaymentDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class EPaymentDetail  implements java.io.Serializable {
    private java.lang.String id;

    private com.fedex.ship.stub.EPaymentProcessorType EPaymentProcessor;

    private com.fedex.ship.stub.EPaymentModeType EPaymentMode;

    private java.lang.String maskedCreditCard;

    private java.lang.String creditCardExpirationDate;

    private com.fedex.ship.stub.Money amount;

    public EPaymentDetail() {
    }

    public EPaymentDetail(
           java.lang.String id,
           com.fedex.ship.stub.EPaymentProcessorType EPaymentProcessor,
           com.fedex.ship.stub.EPaymentModeType EPaymentMode,
           java.lang.String maskedCreditCard,
           java.lang.String creditCardExpirationDate,
           com.fedex.ship.stub.Money amount) {
           this.id = id;
           this.EPaymentProcessor = EPaymentProcessor;
           this.EPaymentMode = EPaymentMode;
           this.maskedCreditCard = maskedCreditCard;
           this.creditCardExpirationDate = creditCardExpirationDate;
           this.amount = amount;
    }


    /**
     * Gets the id value for this EPaymentDetail.
     * 
     * @return id
     */
    public java.lang.String getId() {
        return id;
    }


    /**
     * Sets the id value for this EPaymentDetail.
     * 
     * @param id
     */
    public void setId(java.lang.String id) {
        this.id = id;
    }


    /**
     * Gets the EPaymentProcessor value for this EPaymentDetail.
     * 
     * @return EPaymentProcessor
     */
    public com.fedex.ship.stub.EPaymentProcessorType getEPaymentProcessor() {
        return EPaymentProcessor;
    }


    /**
     * Sets the EPaymentProcessor value for this EPaymentDetail.
     * 
     * @param EPaymentProcessor
     */
    public void setEPaymentProcessor(com.fedex.ship.stub.EPaymentProcessorType EPaymentProcessor) {
        this.EPaymentProcessor = EPaymentProcessor;
    }


    /**
     * Gets the EPaymentMode value for this EPaymentDetail.
     * 
     * @return EPaymentMode
     */
    public com.fedex.ship.stub.EPaymentModeType getEPaymentMode() {
        return EPaymentMode;
    }


    /**
     * Sets the EPaymentMode value for this EPaymentDetail.
     * 
     * @param EPaymentMode
     */
    public void setEPaymentMode(com.fedex.ship.stub.EPaymentModeType EPaymentMode) {
        this.EPaymentMode = EPaymentMode;
    }


    /**
     * Gets the maskedCreditCard value for this EPaymentDetail.
     * 
     * @return maskedCreditCard
     */
    public java.lang.String getMaskedCreditCard() {
        return maskedCreditCard;
    }


    /**
     * Sets the maskedCreditCard value for this EPaymentDetail.
     * 
     * @param maskedCreditCard
     */
    public void setMaskedCreditCard(java.lang.String maskedCreditCard) {
        this.maskedCreditCard = maskedCreditCard;
    }


    /**
     * Gets the creditCardExpirationDate value for this EPaymentDetail.
     * 
     * @return creditCardExpirationDate
     */
    public java.lang.String getCreditCardExpirationDate() {
        return creditCardExpirationDate;
    }


    /**
     * Sets the creditCardExpirationDate value for this EPaymentDetail.
     * 
     * @param creditCardExpirationDate
     */
    public void setCreditCardExpirationDate(java.lang.String creditCardExpirationDate) {
        this.creditCardExpirationDate = creditCardExpirationDate;
    }


    /**
     * Gets the amount value for this EPaymentDetail.
     * 
     * @return amount
     */
    public com.fedex.ship.stub.Money getAmount() {
        return amount;
    }


    /**
     * Sets the amount value for this EPaymentDetail.
     * 
     * @param amount
     */
    public void setAmount(com.fedex.ship.stub.Money amount) {
        this.amount = amount;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof EPaymentDetail)) return false;
        EPaymentDetail other = (EPaymentDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.id==null && other.getId()==null) || 
             (this.id!=null &&
              this.id.equals(other.getId()))) &&
            ((this.EPaymentProcessor==null && other.getEPaymentProcessor()==null) || 
             (this.EPaymentProcessor!=null &&
              this.EPaymentProcessor.equals(other.getEPaymentProcessor()))) &&
            ((this.EPaymentMode==null && other.getEPaymentMode()==null) || 
             (this.EPaymentMode!=null &&
              this.EPaymentMode.equals(other.getEPaymentMode()))) &&
            ((this.maskedCreditCard==null && other.getMaskedCreditCard()==null) || 
             (this.maskedCreditCard!=null &&
              this.maskedCreditCard.equals(other.getMaskedCreditCard()))) &&
            ((this.creditCardExpirationDate==null && other.getCreditCardExpirationDate()==null) || 
             (this.creditCardExpirationDate!=null &&
              this.creditCardExpirationDate.equals(other.getCreditCardExpirationDate()))) &&
            ((this.amount==null && other.getAmount()==null) || 
             (this.amount!=null &&
              this.amount.equals(other.getAmount())));
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
        if (getId() != null) {
            _hashCode += getId().hashCode();
        }
        if (getEPaymentProcessor() != null) {
            _hashCode += getEPaymentProcessor().hashCode();
        }
        if (getEPaymentMode() != null) {
            _hashCode += getEPaymentMode().hashCode();
        }
        if (getMaskedCreditCard() != null) {
            _hashCode += getMaskedCreditCard().hashCode();
        }
        if (getCreditCardExpirationDate() != null) {
            _hashCode += getCreditCardExpirationDate().hashCode();
        }
        if (getAmount() != null) {
            _hashCode += getAmount().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(EPaymentDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EPaymentDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("id");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Id"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("EPaymentProcessor");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EPaymentProcessor"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EPaymentProcessorType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("EPaymentMode");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EPaymentMode"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EPaymentModeType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("maskedCreditCard");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "MaskedCreditCard"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("creditCardExpirationDate");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CreditCardExpirationDate"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("amount");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Amount"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
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
