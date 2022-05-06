/**
 * ServiceDescription.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class ServiceDescription  implements java.io.Serializable {
    private java.lang.String serviceType;

    private java.lang.String code;

    /* Branded, translated, and/or localized names for this service. */
    private com.fedex.ship.stub.ProductName[] names;

    private java.lang.String description;

    private java.lang.String astraDescription;

    public ServiceDescription() {
    }

    public ServiceDescription(
           java.lang.String serviceType,
           java.lang.String code,
           com.fedex.ship.stub.ProductName[] names,
           java.lang.String description,
           java.lang.String astraDescription) {
           this.serviceType = serviceType;
           this.code = code;
           this.names = names;
           this.description = description;
           this.astraDescription = astraDescription;
    }


    /**
     * Gets the serviceType value for this ServiceDescription.
     * 
     * @return serviceType
     */
    public java.lang.String getServiceType() {
        return serviceType;
    }


    /**
     * Sets the serviceType value for this ServiceDescription.
     * 
     * @param serviceType
     */
    public void setServiceType(java.lang.String serviceType) {
        this.serviceType = serviceType;
    }


    /**
     * Gets the code value for this ServiceDescription.
     * 
     * @return code
     */
    public java.lang.String getCode() {
        return code;
    }


    /**
     * Sets the code value for this ServiceDescription.
     * 
     * @param code
     */
    public void setCode(java.lang.String code) {
        this.code = code;
    }


    /**
     * Gets the names value for this ServiceDescription.
     * 
     * @return names   * Branded, translated, and/or localized names for this service.
     */
    public com.fedex.ship.stub.ProductName[] getNames() {
        return names;
    }


    /**
     * Sets the names value for this ServiceDescription.
     * 
     * @param names   * Branded, translated, and/or localized names for this service.
     */
    public void setNames(com.fedex.ship.stub.ProductName[] names) {
        this.names = names;
    }

    public com.fedex.ship.stub.ProductName getNames(int i) {
        return this.names[i];
    }

    public void setNames(int i, com.fedex.ship.stub.ProductName _value) {
        this.names[i] = _value;
    }


    /**
     * Gets the description value for this ServiceDescription.
     * 
     * @return description
     */
    public java.lang.String getDescription() {
        return description;
    }


    /**
     * Sets the description value for this ServiceDescription.
     * 
     * @param description
     */
    public void setDescription(java.lang.String description) {
        this.description = description;
    }


    /**
     * Gets the astraDescription value for this ServiceDescription.
     * 
     * @return astraDescription
     */
    public java.lang.String getAstraDescription() {
        return astraDescription;
    }


    /**
     * Sets the astraDescription value for this ServiceDescription.
     * 
     * @param astraDescription
     */
    public void setAstraDescription(java.lang.String astraDescription) {
        this.astraDescription = astraDescription;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof ServiceDescription)) return false;
        ServiceDescription other = (ServiceDescription) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.serviceType==null && other.getServiceType()==null) || 
             (this.serviceType!=null &&
              this.serviceType.equals(other.getServiceType()))) &&
            ((this.code==null && other.getCode()==null) || 
             (this.code!=null &&
              this.code.equals(other.getCode()))) &&
            ((this.names==null && other.getNames()==null) || 
             (this.names!=null &&
              java.util.Arrays.equals(this.names, other.getNames()))) &&
            ((this.description==null && other.getDescription()==null) || 
             (this.description!=null &&
              this.description.equals(other.getDescription()))) &&
            ((this.astraDescription==null && other.getAstraDescription()==null) || 
             (this.astraDescription!=null &&
              this.astraDescription.equals(other.getAstraDescription())));
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
        if (getServiceType() != null) {
            _hashCode += getServiceType().hashCode();
        }
        if (getCode() != null) {
            _hashCode += getCode().hashCode();
        }
        if (getNames() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getNames());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getNames(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getDescription() != null) {
            _hashCode += getDescription().hashCode();
        }
        if (getAstraDescription() != null) {
            _hashCode += getAstraDescription().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(ServiceDescription.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ServiceDescription"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("serviceType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ServiceType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("code");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Code"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("names");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Names"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ProductName"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("description");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Description"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("astraDescription");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AstraDescription"));
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
