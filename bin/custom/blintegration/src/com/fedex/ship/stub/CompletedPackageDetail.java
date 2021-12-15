/**
 * CompletedPackageDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class CompletedPackageDetail  implements java.io.Serializable {
    private org.apache.axis.types.PositiveInteger sequenceNumber;

    private com.fedex.ship.stub.TrackingId[] trackingIds;

    /* An identifier of each group of identical packages. */
    private org.apache.axis.types.NonNegativeInteger groupNumber;

    private com.fedex.ship.stub.OversizeClassType oversizeClass;

    /* All package-level rating data for this package, which may include
     * data for multiple rate types. */
    private com.fedex.ship.stub.PackageRating packageRating;

    private com.fedex.ship.stub.SpecialServiceDescription[] specialServiceDescriptions;

    private com.fedex.ship.stub.PackageOperationalDetail operationalDetail;

    private com.fedex.ship.stub.ShippingDocument label;

    /* All package-level shipping documents (other than labels and
     * barcodes). For use in loads after January, 2008. */
    private com.fedex.ship.stub.ShippingDocument[] packageDocuments;

    /* Specifies the information associated with this package that
     * has COD special service in a ground shipment. */
    private com.fedex.ship.stub.CodReturnPackageDetail codReturnDetail;

    /* Actual signature option applied, to allow for cases in which
     * the original value conflicted with other service features in the shipment. */
    private com.fedex.ship.stub.SignatureOptionType signatureOption;

    private com.fedex.ship.stub.Weight dryIceWeight;

    /* Documents the kinds and quantities of all hazardous commodities
     * in the current package, using updated hazardous commodity description
     * data. */
    private com.fedex.ship.stub.CompletedHazardousPackageDetail hazardousPackageDetail;

    public CompletedPackageDetail() {
    }

    public CompletedPackageDetail(
           org.apache.axis.types.PositiveInteger sequenceNumber,
           com.fedex.ship.stub.TrackingId[] trackingIds,
           org.apache.axis.types.NonNegativeInteger groupNumber,
           com.fedex.ship.stub.OversizeClassType oversizeClass,
           com.fedex.ship.stub.PackageRating packageRating,
           com.fedex.ship.stub.SpecialServiceDescription[] specialServiceDescriptions,
           com.fedex.ship.stub.PackageOperationalDetail operationalDetail,
           com.fedex.ship.stub.ShippingDocument label,
           com.fedex.ship.stub.ShippingDocument[] packageDocuments,
           com.fedex.ship.stub.CodReturnPackageDetail codReturnDetail,
           com.fedex.ship.stub.SignatureOptionType signatureOption,
           com.fedex.ship.stub.Weight dryIceWeight,
           com.fedex.ship.stub.CompletedHazardousPackageDetail hazardousPackageDetail) {
           this.sequenceNumber = sequenceNumber;
           this.trackingIds = trackingIds;
           this.groupNumber = groupNumber;
           this.oversizeClass = oversizeClass;
           this.packageRating = packageRating;
           this.specialServiceDescriptions = specialServiceDescriptions;
           this.operationalDetail = operationalDetail;
           this.label = label;
           this.packageDocuments = packageDocuments;
           this.codReturnDetail = codReturnDetail;
           this.signatureOption = signatureOption;
           this.dryIceWeight = dryIceWeight;
           this.hazardousPackageDetail = hazardousPackageDetail;
    }


    /**
     * Gets the sequenceNumber value for this CompletedPackageDetail.
     * 
     * @return sequenceNumber
     */
    public org.apache.axis.types.PositiveInteger getSequenceNumber() {
        return sequenceNumber;
    }


    /**
     * Sets the sequenceNumber value for this CompletedPackageDetail.
     * 
     * @param sequenceNumber
     */
    public void setSequenceNumber(org.apache.axis.types.PositiveInteger sequenceNumber) {
        this.sequenceNumber = sequenceNumber;
    }


    /**
     * Gets the trackingIds value for this CompletedPackageDetail.
     * 
     * @return trackingIds
     */
    public com.fedex.ship.stub.TrackingId[] getTrackingIds() {
        return trackingIds;
    }


    /**
     * Sets the trackingIds value for this CompletedPackageDetail.
     * 
     * @param trackingIds
     */
    public void setTrackingIds(com.fedex.ship.stub.TrackingId[] trackingIds) {
        this.trackingIds = trackingIds;
    }

    public com.fedex.ship.stub.TrackingId getTrackingIds(int i) {
        return this.trackingIds[i];
    }

    public void setTrackingIds(int i, com.fedex.ship.stub.TrackingId _value) {
        this.trackingIds[i] = _value;
    }


    /**
     * Gets the groupNumber value for this CompletedPackageDetail.
     * 
     * @return groupNumber   * An identifier of each group of identical packages.
     */
    public org.apache.axis.types.NonNegativeInteger getGroupNumber() {
        return groupNumber;
    }


    /**
     * Sets the groupNumber value for this CompletedPackageDetail.
     * 
     * @param groupNumber   * An identifier of each group of identical packages.
     */
    public void setGroupNumber(org.apache.axis.types.NonNegativeInteger groupNumber) {
        this.groupNumber = groupNumber;
    }


    /**
     * Gets the oversizeClass value for this CompletedPackageDetail.
     * 
     * @return oversizeClass
     */
    public com.fedex.ship.stub.OversizeClassType getOversizeClass() {
        return oversizeClass;
    }


    /**
     * Sets the oversizeClass value for this CompletedPackageDetail.
     * 
     * @param oversizeClass
     */
    public void setOversizeClass(com.fedex.ship.stub.OversizeClassType oversizeClass) {
        this.oversizeClass = oversizeClass;
    }


    /**
     * Gets the packageRating value for this CompletedPackageDetail.
     * 
     * @return packageRating   * All package-level rating data for this package, which may include
     * data for multiple rate types.
     */
    public com.fedex.ship.stub.PackageRating getPackageRating() {
        return packageRating;
    }


    /**
     * Sets the packageRating value for this CompletedPackageDetail.
     * 
     * @param packageRating   * All package-level rating data for this package, which may include
     * data for multiple rate types.
     */
    public void setPackageRating(com.fedex.ship.stub.PackageRating packageRating) {
        this.packageRating = packageRating;
    }


    /**
     * Gets the specialServiceDescriptions value for this CompletedPackageDetail.
     * 
     * @return specialServiceDescriptions
     */
    public com.fedex.ship.stub.SpecialServiceDescription[] getSpecialServiceDescriptions() {
        return specialServiceDescriptions;
    }


    /**
     * Sets the specialServiceDescriptions value for this CompletedPackageDetail.
     * 
     * @param specialServiceDescriptions
     */
    public void setSpecialServiceDescriptions(com.fedex.ship.stub.SpecialServiceDescription[] specialServiceDescriptions) {
        this.specialServiceDescriptions = specialServiceDescriptions;
    }

    public com.fedex.ship.stub.SpecialServiceDescription getSpecialServiceDescriptions(int i) {
        return this.specialServiceDescriptions[i];
    }

    public void setSpecialServiceDescriptions(int i, com.fedex.ship.stub.SpecialServiceDescription _value) {
        this.specialServiceDescriptions[i] = _value;
    }


    /**
     * Gets the operationalDetail value for this CompletedPackageDetail.
     * 
     * @return operationalDetail
     */
    public com.fedex.ship.stub.PackageOperationalDetail getOperationalDetail() {
        return operationalDetail;
    }


    /**
     * Sets the operationalDetail value for this CompletedPackageDetail.
     * 
     * @param operationalDetail
     */
    public void setOperationalDetail(com.fedex.ship.stub.PackageOperationalDetail operationalDetail) {
        this.operationalDetail = operationalDetail;
    }


    /**
     * Gets the label value for this CompletedPackageDetail.
     * 
     * @return label
     */
    public com.fedex.ship.stub.ShippingDocument getLabel() {
        return label;
    }


    /**
     * Sets the label value for this CompletedPackageDetail.
     * 
     * @param label
     */
    public void setLabel(com.fedex.ship.stub.ShippingDocument label) {
        this.label = label;
    }


    /**
     * Gets the packageDocuments value for this CompletedPackageDetail.
     * 
     * @return packageDocuments   * All package-level shipping documents (other than labels and
     * barcodes). For use in loads after January, 2008.
     */
    public com.fedex.ship.stub.ShippingDocument[] getPackageDocuments() {
        return packageDocuments;
    }


    /**
     * Sets the packageDocuments value for this CompletedPackageDetail.
     * 
     * @param packageDocuments   * All package-level shipping documents (other than labels and
     * barcodes). For use in loads after January, 2008.
     */
    public void setPackageDocuments(com.fedex.ship.stub.ShippingDocument[] packageDocuments) {
        this.packageDocuments = packageDocuments;
    }

    public com.fedex.ship.stub.ShippingDocument getPackageDocuments(int i) {
        return this.packageDocuments[i];
    }

    public void setPackageDocuments(int i, com.fedex.ship.stub.ShippingDocument _value) {
        this.packageDocuments[i] = _value;
    }


    /**
     * Gets the codReturnDetail value for this CompletedPackageDetail.
     * 
     * @return codReturnDetail   * Specifies the information associated with this package that
     * has COD special service in a ground shipment.
     */
    public com.fedex.ship.stub.CodReturnPackageDetail getCodReturnDetail() {
        return codReturnDetail;
    }


    /**
     * Sets the codReturnDetail value for this CompletedPackageDetail.
     * 
     * @param codReturnDetail   * Specifies the information associated with this package that
     * has COD special service in a ground shipment.
     */
    public void setCodReturnDetail(com.fedex.ship.stub.CodReturnPackageDetail codReturnDetail) {
        this.codReturnDetail = codReturnDetail;
    }


    /**
     * Gets the signatureOption value for this CompletedPackageDetail.
     * 
     * @return signatureOption   * Actual signature option applied, to allow for cases in which
     * the original value conflicted with other service features in the shipment.
     */
    public com.fedex.ship.stub.SignatureOptionType getSignatureOption() {
        return signatureOption;
    }


    /**
     * Sets the signatureOption value for this CompletedPackageDetail.
     * 
     * @param signatureOption   * Actual signature option applied, to allow for cases in which
     * the original value conflicted with other service features in the shipment.
     */
    public void setSignatureOption(com.fedex.ship.stub.SignatureOptionType signatureOption) {
        this.signatureOption = signatureOption;
    }


    /**
     * Gets the dryIceWeight value for this CompletedPackageDetail.
     * 
     * @return dryIceWeight
     */
    public com.fedex.ship.stub.Weight getDryIceWeight() {
        return dryIceWeight;
    }


    /**
     * Sets the dryIceWeight value for this CompletedPackageDetail.
     * 
     * @param dryIceWeight
     */
    public void setDryIceWeight(com.fedex.ship.stub.Weight dryIceWeight) {
        this.dryIceWeight = dryIceWeight;
    }


    /**
     * Gets the hazardousPackageDetail value for this CompletedPackageDetail.
     * 
     * @return hazardousPackageDetail   * Documents the kinds and quantities of all hazardous commodities
     * in the current package, using updated hazardous commodity description
     * data.
     */
    public com.fedex.ship.stub.CompletedHazardousPackageDetail getHazardousPackageDetail() {
        return hazardousPackageDetail;
    }


    /**
     * Sets the hazardousPackageDetail value for this CompletedPackageDetail.
     * 
     * @param hazardousPackageDetail   * Documents the kinds and quantities of all hazardous commodities
     * in the current package, using updated hazardous commodity description
     * data.
     */
    public void setHazardousPackageDetail(com.fedex.ship.stub.CompletedHazardousPackageDetail hazardousPackageDetail) {
        this.hazardousPackageDetail = hazardousPackageDetail;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof CompletedPackageDetail)) return false;
        CompletedPackageDetail other = (CompletedPackageDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.sequenceNumber==null && other.getSequenceNumber()==null) || 
             (this.sequenceNumber!=null &&
              this.sequenceNumber.equals(other.getSequenceNumber()))) &&
            ((this.trackingIds==null && other.getTrackingIds()==null) || 
             (this.trackingIds!=null &&
              java.util.Arrays.equals(this.trackingIds, other.getTrackingIds()))) &&
            ((this.groupNumber==null && other.getGroupNumber()==null) || 
             (this.groupNumber!=null &&
              this.groupNumber.equals(other.getGroupNumber()))) &&
            ((this.oversizeClass==null && other.getOversizeClass()==null) || 
             (this.oversizeClass!=null &&
              this.oversizeClass.equals(other.getOversizeClass()))) &&
            ((this.packageRating==null && other.getPackageRating()==null) || 
             (this.packageRating!=null &&
              this.packageRating.equals(other.getPackageRating()))) &&
            ((this.specialServiceDescriptions==null && other.getSpecialServiceDescriptions()==null) || 
             (this.specialServiceDescriptions!=null &&
              java.util.Arrays.equals(this.specialServiceDescriptions, other.getSpecialServiceDescriptions()))) &&
            ((this.operationalDetail==null && other.getOperationalDetail()==null) || 
             (this.operationalDetail!=null &&
              this.operationalDetail.equals(other.getOperationalDetail()))) &&
            ((this.label==null && other.getLabel()==null) || 
             (this.label!=null &&
              this.label.equals(other.getLabel()))) &&
            ((this.packageDocuments==null && other.getPackageDocuments()==null) || 
             (this.packageDocuments!=null &&
              java.util.Arrays.equals(this.packageDocuments, other.getPackageDocuments()))) &&
            ((this.codReturnDetail==null && other.getCodReturnDetail()==null) || 
             (this.codReturnDetail!=null &&
              this.codReturnDetail.equals(other.getCodReturnDetail()))) &&
            ((this.signatureOption==null && other.getSignatureOption()==null) || 
             (this.signatureOption!=null &&
              this.signatureOption.equals(other.getSignatureOption()))) &&
            ((this.dryIceWeight==null && other.getDryIceWeight()==null) || 
             (this.dryIceWeight!=null &&
              this.dryIceWeight.equals(other.getDryIceWeight()))) &&
            ((this.hazardousPackageDetail==null && other.getHazardousPackageDetail()==null) || 
             (this.hazardousPackageDetail!=null &&
              this.hazardousPackageDetail.equals(other.getHazardousPackageDetail())));
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
        if (getSequenceNumber() != null) {
            _hashCode += getSequenceNumber().hashCode();
        }
        if (getTrackingIds() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getTrackingIds());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getTrackingIds(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getGroupNumber() != null) {
            _hashCode += getGroupNumber().hashCode();
        }
        if (getOversizeClass() != null) {
            _hashCode += getOversizeClass().hashCode();
        }
        if (getPackageRating() != null) {
            _hashCode += getPackageRating().hashCode();
        }
        if (getSpecialServiceDescriptions() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getSpecialServiceDescriptions());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getSpecialServiceDescriptions(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getOperationalDetail() != null) {
            _hashCode += getOperationalDetail().hashCode();
        }
        if (getLabel() != null) {
            _hashCode += getLabel().hashCode();
        }
        if (getPackageDocuments() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getPackageDocuments());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getPackageDocuments(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getCodReturnDetail() != null) {
            _hashCode += getCodReturnDetail().hashCode();
        }
        if (getSignatureOption() != null) {
            _hashCode += getSignatureOption().hashCode();
        }
        if (getDryIceWeight() != null) {
            _hashCode += getDryIceWeight().hashCode();
        }
        if (getHazardousPackageDetail() != null) {
            _hashCode += getHazardousPackageDetail().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(CompletedPackageDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedPackageDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("sequenceNumber");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SequenceNumber"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "positiveInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("trackingIds");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TrackingIds"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "TrackingId"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("groupNumber");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "GroupNumber"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "nonNegativeInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("oversizeClass");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "OversizeClass"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "OversizeClassType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("packageRating");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PackageRating"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PackageRating"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("specialServiceDescriptions");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SpecialServiceDescriptions"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SpecialServiceDescription"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("operationalDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "OperationalDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PackageOperationalDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("label");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Label"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocument"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("packageDocuments");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PackageDocuments"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocument"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("codReturnDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CodReturnDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CodReturnPackageDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("signatureOption");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SignatureOption"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SignatureOptionType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dryIceWeight");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DryIceWeight"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Weight"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("hazardousPackageDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "HazardousPackageDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CompletedHazardousPackageDetail"));
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
