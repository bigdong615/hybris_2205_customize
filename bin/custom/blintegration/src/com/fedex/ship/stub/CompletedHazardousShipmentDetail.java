/**
 * CompletedHazardousShipmentDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;


/**
 * Computed shipment level hazardous commodity information.
 */
public class CompletedHazardousShipmentDetail  implements java.io.Serializable {
    private com.fedex.ship.stub.CompletedHazardousSummaryDetail hazardousSummaryDetail;

    private com.fedex.ship.stub.ShipmentDryIceDetail dryIceDetail;

    /* This contains the ADR License information, which identifies
     * the license number and ADR category under which the customer is allowed
     * to ship. */
    private com.fedex.ship.stub.AdrLicenseDetail adrLicense;

    public CompletedHazardousShipmentDetail() {
    }

    public CompletedHazardousShipmentDetail(
           com.fedex.ship.stub.CompletedHazardousSummaryDetail hazardousSummaryDetail,
           com.fedex.ship.stub.ShipmentDryIceDetail dryIceDetail,
           com.fedex.ship.stub.AdrLicenseDetail adrLicense) {
           this.hazardousSummaryDetail = hazardousSummaryDetail;
           this.dryIceDetail = dryIceDetail;
           this.adrLicense = adrLicense;
    }


    /**
     * Gets the hazardousSummaryDetail value for this CompletedHazardousShipmentDetail.
     * 
     * @return hazardousSummaryDetail
     */
    public com.fedex.ship.stub.CompletedHazardousSummaryDetail getHazardousSummaryDetail() {
        return hazardousSummaryDetail;
    }


    /**
     * Sets the hazardousSummaryDetail value for this CompletedHazardousShipmentDetail.
     * 
     * @param hazardousSummaryDetail
     */
    public void setHazardousSummaryDetail(com.fedex.ship.stub.CompletedHazardousSummaryDetail hazardousSummaryDetail) {
        this.hazardousSummaryDetail = hazardousSummaryDetail;
    }


    /**
     * Gets the dryIceDetail value for this CompletedHazardousShipmentDetail.
     * 
     * @return dryIceDetail
     */
    public com.fedex.ship.stub.ShipmentDryIceDetail getDryIceDetail() {
        return dryIceDetail;
    }


    /**
     * Sets the dryIceDetail value for this CompletedHazardousShipmentDetail.
     * 
     * @param dryIceDetail
     */
    public void setDryIceDetail(com.fedex.ship.stub.ShipmentDryIceDetail dryIceDetail) {
        this.dryIceDetail = dryIceDetail;
    }


    /**
     * Gets the adrLicense value for this CompletedHazardousShipmentDetail.
     * 
     * @return adrLicense   * This contains the ADR License information, which identifies
     * the license number and ADR category under which the customer is allowed
     * to ship.
     */
    public com.fedex.ship.stub.AdrLicenseDetail getAdrLicense() {
        return adrLicense;
    }


    /**
     * Sets the adrLicense value for this CompletedHazardousShipmentDetail.
     * 
     * @param adrLicense   * This contains the ADR License information, which identifies
     * the license number and ADR category under which the customer is allowed
     * to ship.
     */
    public void setAdrLicense(com.fedex.ship.stub.AdrLicenseDetail adrLicense) {
        this.adrLicense = adrLicense;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof CompletedHazardousShipmentDetail)) return false;
        CompletedHazardousShipmentDetail other = (CompletedHazardousShipmentDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.hazardousSummaryDetail==null && other.getHazardousSummaryDetail()==null) || 
             (this.hazardousSummaryDetail!=null &&
              this.hazardousSummaryDetail.equals(other.getHazardousSummaryDetail()))) &&
            ((this.dryIceDetail==null && other.getDryIceDetail()==null) || 
             (this.dryIceDetail!=null &&
              this.dryIceDetail.equals(other.getDryIceDetail()))) &&
            ((this.adrLicense==null && other.getAdrLicense()==null) || 
             (this.adrLicense!=null &&
              this.adrLicense.equals(other.getAdrLicense())));
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
        if (getHazardousSummaryDetail() != null) {
            _hashCode += getHazardousSummaryDetail().hashCode();
        }
        if (getDryIceDetail() != null) {
            _hashCode += getDryIceDetail().hashCode();
        }
        if (getAdrLicense() != null) {
            _hashCode += getAdrLicense().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(CompletedHazardousShipmentDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedHazardousShipmentDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("hazardousSummaryDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "HazardousSummaryDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedHazardousSummaryDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dryIceDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DryIceDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipmentDryIceDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("adrLicense");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AdrLicense"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AdrLicenseDetail"));
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
