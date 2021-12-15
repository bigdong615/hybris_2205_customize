/**
 * EnterpriseDocumentType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class EnterpriseDocumentType implements java.io.Serializable {
    private java.lang.String _value_;
    private static java.util.HashMap _table_ = new java.util.HashMap();

    // Constructor
    protected EnterpriseDocumentType(java.lang.String value) {
        _value_ = value;
        _table_.put(_value_,this);
    }

    public static final java.lang.String _AIR_WAYBILL = "AIR_WAYBILL";
    public static final java.lang.String _CERTIFICATE_OF_ORIGIN = "CERTIFICATE_OF_ORIGIN";
    public static final java.lang.String _COMMERCIAL_INVOICE = "COMMERCIAL_INVOICE";
    public static final java.lang.String _PRO_FORMA_INVOICE = "PRO_FORMA_INVOICE";
    public static final java.lang.String _USMCA_CERTIFICATION_OF_ORIGIN = "USMCA_CERTIFICATION_OF_ORIGIN";
    public static final java.lang.String _USMCA_COMMERCIAL_INVOICE_CERTIFICATION_OF_ORIGIN = "USMCA_COMMERCIAL_INVOICE_CERTIFICATION_OF_ORIGIN";
    public static final EnterpriseDocumentType AIR_WAYBILL = new EnterpriseDocumentType(_AIR_WAYBILL);
    public static final EnterpriseDocumentType CERTIFICATE_OF_ORIGIN = new EnterpriseDocumentType(_CERTIFICATE_OF_ORIGIN);
    public static final EnterpriseDocumentType COMMERCIAL_INVOICE = new EnterpriseDocumentType(_COMMERCIAL_INVOICE);
    public static final EnterpriseDocumentType PRO_FORMA_INVOICE = new EnterpriseDocumentType(_PRO_FORMA_INVOICE);
    public static final EnterpriseDocumentType USMCA_CERTIFICATION_OF_ORIGIN = new EnterpriseDocumentType(_USMCA_CERTIFICATION_OF_ORIGIN);
    public static final EnterpriseDocumentType USMCA_COMMERCIAL_INVOICE_CERTIFICATION_OF_ORIGIN = new EnterpriseDocumentType(_USMCA_COMMERCIAL_INVOICE_CERTIFICATION_OF_ORIGIN);
    public java.lang.String getValue() { return _value_;}
    public static EnterpriseDocumentType fromValue(java.lang.String value)
          throws java.lang.IllegalArgumentException {
        EnterpriseDocumentType enumeration = (EnterpriseDocumentType)
            _table_.get(value);
        if (enumeration==null) throw new java.lang.IllegalArgumentException();
        return enumeration;
    }
    public static EnterpriseDocumentType fromString(java.lang.String value)
          throws java.lang.IllegalArgumentException {
        return fromValue(value);
    }
    public boolean equals(java.lang.Object obj) {return (obj == this);}
    public int hashCode() { return toString().hashCode();}
    public java.lang.String toString() { return _value_;}
    public java.lang.Object readResolve() throws java.io.ObjectStreamException { return fromValue(_value_);}
    public static org.apache.axis.encoding.Serializer getSerializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new org.apache.axis.encoding.ser.EnumSerializer(
            _javaType, _xmlType);
    }
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           java.lang.String mechType, 
           java.lang.Class _javaType,  
           javax.xml.namespace.QName _xmlType) {
        return 
          new org.apache.axis.encoding.ser.EnumDeserializer(
            _javaType, _xmlType);
    }
    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(EnterpriseDocumentType.class);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "EnterpriseDocumentType"));
    }
    /**
     * Return type metadata object
     */
    public static org.apache.axis.description.TypeDesc getTypeDesc() {
        return typeDesc;
    }

}
