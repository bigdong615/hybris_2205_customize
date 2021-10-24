/**
 * TrackingDocumentFormat.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class TrackingDocumentFormat  implements java.io.Serializable {
    private  TrackingDocumentDispositionDetail[] dispositions;

    private  TrackingDocumentGroupingType grouping;

    private  TrackingDocumentImageType imageType;

    /* The localization for the generated document. */
    private  Localization localization;

    public TrackingDocumentFormat() {
    }

    public TrackingDocumentFormat(
            TrackingDocumentDispositionDetail[] dispositions,
            TrackingDocumentGroupingType grouping,
            TrackingDocumentImageType imageType,
            Localization localization) {
           this.dispositions = dispositions;
           this.grouping = grouping;
           this.imageType = imageType;
           this.localization = localization;
    }


    /**
     * Gets the dispositions value for this TrackingDocumentFormat.
     * 
     * @return dispositions
     */
    public  TrackingDocumentDispositionDetail[] getDispositions() {
        return dispositions;
    }


    /**
     * Sets the dispositions value for this TrackingDocumentFormat.
     * 
     * @param dispositions
     */
    public void setDispositions( TrackingDocumentDispositionDetail[] dispositions) {
        this.dispositions = dispositions;
    }

    public  TrackingDocumentDispositionDetail getDispositions(int i) {
        return this.dispositions[i];
    }

    public void setDispositions(int i,  TrackingDocumentDispositionDetail _value) {
        this.dispositions[i] = _value;
    }


    /**
     * Gets the grouping value for this TrackingDocumentFormat.
     * 
     * @return grouping
     */
    public  TrackingDocumentGroupingType getGrouping() {
        return grouping;
    }


    /**
     * Sets the grouping value for this TrackingDocumentFormat.
     * 
     * @param grouping
     */
    public void setGrouping( TrackingDocumentGroupingType grouping) {
        this.grouping = grouping;
    }


    /**
     * Gets the imageType value for this TrackingDocumentFormat.
     * 
     * @return imageType
     */
    public  TrackingDocumentImageType getImageType() {
        return imageType;
    }


    /**
     * Sets the imageType value for this TrackingDocumentFormat.
     * 
     * @param imageType
     */
    public void setImageType( TrackingDocumentImageType imageType) {
        this.imageType = imageType;
    }


    /**
     * Gets the localization value for this TrackingDocumentFormat.
     * 
     * @return localization   * The localization for the generated document.
     */
    public  Localization getLocalization() {
        return localization;
    }


    /**
     * Sets the localization value for this TrackingDocumentFormat.
     * 
     * @param localization   * The localization for the generated document.
     */
    public void setLocalization( Localization localization) {
        this.localization = localization;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof TrackingDocumentFormat)) return false;
        TrackingDocumentFormat other = (TrackingDocumentFormat) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.dispositions==null && other.getDispositions()==null) ||
             (this.dispositions!=null &&
              java.util.Arrays.equals(this.dispositions, other.getDispositions()))) &&
            ((this.grouping==null && other.getGrouping()==null) ||
             (this.grouping!=null &&
              this.grouping.equals(other.getGrouping()))) &&
            ((this.imageType==null && other.getImageType()==null) ||
             (this.imageType!=null &&
              this.imageType.equals(other.getImageType()))) &&
            ((this.localization==null && other.getLocalization()==null) ||
             (this.localization!=null &&
              this.localization.equals(other.getLocalization())));
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
        if (getDispositions() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getDispositions());
                 i++) {
                Object obj = java.lang.reflect.Array.get(getDispositions(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getGrouping() != null) {
            _hashCode += getGrouping().hashCode();
        }
        if (getImageType() != null) {
            _hashCode += getImageType().hashCode();
        }
        if (getLocalization() != null) {
            _hashCode += getLocalization().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(TrackingDocumentFormat.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDocumentFormat"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dispositions");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Dispositions"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDocumentDispositionDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("grouping");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Grouping"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDocumentGroupingType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("imageType");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ImageType"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDocumentImageType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("localization");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Localization"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Localization"));
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
           String mechType,
           Class _javaType,
           javax.xml.namespace.QName _xmlType) {
        return
          new  org.apache.axis.encoding.ser.BeanSerializer(
            _javaType, _xmlType, typeDesc);
    }

    /**
     * Get Custom Deserializer
     */
    public static org.apache.axis.encoding.Deserializer getDeserializer(
           String mechType,
           Class _javaType,
           javax.xml.namespace.QName _xmlType) {
        return 
          new  org.apache.axis.encoding.ser.BeanDeserializer(
            _javaType, _xmlType, typeDesc);
    }

}
