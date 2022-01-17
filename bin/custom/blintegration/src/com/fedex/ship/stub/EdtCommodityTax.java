/**
 * EdtCommodityTax.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class EdtCommodityTax  implements java.io.Serializable {
    private java.lang.String harmonizedCode;

    private com.fedex.ship.stub.EdtTaxDetail[] taxes;

    private com.fedex.ship.stub.Money total;

    public EdtCommodityTax() {
    }

    public EdtCommodityTax(
           java.lang.String harmonizedCode,
           com.fedex.ship.stub.EdtTaxDetail[] taxes,
           com.fedex.ship.stub.Money total) {
           this.harmonizedCode = harmonizedCode;
           this.taxes = taxes;
           this.total = total;
    }


    /**
     * Gets the harmonizedCode value for this EdtCommodityTax.
     * 
     * @return harmonizedCode
     */
    public java.lang.String getHarmonizedCode() {
        return harmonizedCode;
    }


    /**
     * Sets the harmonizedCode value for this EdtCommodityTax.
     * 
     * @param harmonizedCode
     */
    public void setHarmonizedCode(java.lang.String harmonizedCode) {
        this.harmonizedCode = harmonizedCode;
    }


    /**
     * Gets the taxes value for this EdtCommodityTax.
     * 
     * @return taxes
     */
    public com.fedex.ship.stub.EdtTaxDetail[] getTaxes() {
        return taxes;
    }


    /**
     * Sets the taxes value for this EdtCommodityTax.
     * 
     * @param taxes
     */
    public void setTaxes(com.fedex.ship.stub.EdtTaxDetail[] taxes) {
        this.taxes = taxes;
    }

    public com.fedex.ship.stub.EdtTaxDetail getTaxes(int i) {
        return this.taxes[i];
    }

    public void setTaxes(int i, com.fedex.ship.stub.EdtTaxDetail _value) {
        this.taxes[i] = _value;
    }


    /**
     * Gets the total value for this EdtCommodityTax.
     * 
     * @return total
     */
    public com.fedex.ship.stub.Money getTotal() {
        return total;
    }


    /**
     * Sets the total value for this EdtCommodityTax.
     * 
     * @param total
     */
    public void setTotal(com.fedex.ship.stub.Money total) {
        this.total = total;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof EdtCommodityTax)) return false;
        EdtCommodityTax other = (EdtCommodityTax) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.harmonizedCode==null && other.getHarmonizedCode()==null) || 
             (this.harmonizedCode!=null &&
              this.harmonizedCode.equals(other.getHarmonizedCode()))) &&
            ((this.taxes==null && other.getTaxes()==null) || 
             (this.taxes!=null &&
              java.util.Arrays.equals(this.taxes, other.getTaxes()))) &&
            ((this.total==null && other.getTotal()==null) || 
             (this.total!=null &&
              this.total.equals(other.getTotal())));
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
        if (getHarmonizedCode() != null) {
            _hashCode += getHarmonizedCode().hashCode();
        }
        if (getTaxes() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getTaxes());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getTaxes(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getTotal() != null) {
            _hashCode += getTotal().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(EdtCommodityTax.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EdtCommodityTax"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("harmonizedCode");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "HarmonizedCode"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("taxes");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Taxes"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EdtTaxDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("total");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Total"));
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
