/**
 * ShippingDocumentSpecification.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;


/**
 * Contains all data required for additional (non-label) shipping
 * documents to be produced in conjunction with a specific shipment.
 */
public class ShippingDocumentSpecification  implements java.io.Serializable {
    /* Indicates the types of shipping documents requested by the
     * shipper. */
    private com.fedex.ship.stub.RequestedShippingDocumentType[] shippingDocumentTypes;

    private com.fedex.ship.stub.CertificateOfOriginDetail certificateOfOrigin;

    private com.fedex.ship.stub.CommercialInvoiceDetail commercialInvoiceDetail;

    private com.fedex.ship.stub.UsmcaCommercialInvoiceCertificationOfOriginDetail usmcaCommercialInvoiceCertificationOfOriginDetail;

    /* Specifies the production of each package-level custom document
     * (the same specification is used for all packages). */
    private com.fedex.ship.stub.CustomDocumentDetail[] customPackageDocumentDetail;

    /* Specifies the production of a shipment-level custom document. */
    private com.fedex.ship.stub.CustomDocumentDetail[] customShipmentDocumentDetail;

    private com.fedex.ship.stub.ExportDeclarationDetail exportDeclarationDetail;

    private com.fedex.ship.stub.GeneralAgencyAgreementDetail generalAgencyAgreementDetail;

    private com.fedex.ship.stub.UsmcaCertificationOfOriginDetail usmcaCertificationOfOriginDetail;

    /* Specifies the production of the OP-900 document for hazardous
     * materials packages. */
    private com.fedex.ship.stub.Op900Detail op900Detail;

    /* Specifies the production of the 1421c document for dangerous
     * goods shipment. */
    private com.fedex.ship.stub.DangerousGoodsShippersDeclarationDetail dangerousGoodsShippersDeclarationDetail;

    /* Specifies the production of the OP-900 document for hazardous
     * materials. */
    private com.fedex.ship.stub.FreightAddressLabelDetail freightAddressLabelDetail;

    private com.fedex.ship.stub.FreightBillOfLadingDetail freightBillOfLadingDetail;

    /* Specifies the production of the return instructions document. */
    private com.fedex.ship.stub.ReturnInstructionsDetail returnInstructionsDetail;

    public ShippingDocumentSpecification() {
    }

    public ShippingDocumentSpecification(
           com.fedex.ship.stub.RequestedShippingDocumentType[] shippingDocumentTypes,
           com.fedex.ship.stub.CertificateOfOriginDetail certificateOfOrigin,
           com.fedex.ship.stub.CommercialInvoiceDetail commercialInvoiceDetail,
           com.fedex.ship.stub.UsmcaCommercialInvoiceCertificationOfOriginDetail usmcaCommercialInvoiceCertificationOfOriginDetail,
           com.fedex.ship.stub.CustomDocumentDetail[] customPackageDocumentDetail,
           com.fedex.ship.stub.CustomDocumentDetail[] customShipmentDocumentDetail,
           com.fedex.ship.stub.ExportDeclarationDetail exportDeclarationDetail,
           com.fedex.ship.stub.GeneralAgencyAgreementDetail generalAgencyAgreementDetail,
           com.fedex.ship.stub.UsmcaCertificationOfOriginDetail usmcaCertificationOfOriginDetail,
           com.fedex.ship.stub.Op900Detail op900Detail,
           com.fedex.ship.stub.DangerousGoodsShippersDeclarationDetail dangerousGoodsShippersDeclarationDetail,
           com.fedex.ship.stub.FreightAddressLabelDetail freightAddressLabelDetail,
           com.fedex.ship.stub.FreightBillOfLadingDetail freightBillOfLadingDetail,
           com.fedex.ship.stub.ReturnInstructionsDetail returnInstructionsDetail) {
           this.shippingDocumentTypes = shippingDocumentTypes;
           this.certificateOfOrigin = certificateOfOrigin;
           this.commercialInvoiceDetail = commercialInvoiceDetail;
           this.usmcaCommercialInvoiceCertificationOfOriginDetail = usmcaCommercialInvoiceCertificationOfOriginDetail;
           this.customPackageDocumentDetail = customPackageDocumentDetail;
           this.customShipmentDocumentDetail = customShipmentDocumentDetail;
           this.exportDeclarationDetail = exportDeclarationDetail;
           this.generalAgencyAgreementDetail = generalAgencyAgreementDetail;
           this.usmcaCertificationOfOriginDetail = usmcaCertificationOfOriginDetail;
           this.op900Detail = op900Detail;
           this.dangerousGoodsShippersDeclarationDetail = dangerousGoodsShippersDeclarationDetail;
           this.freightAddressLabelDetail = freightAddressLabelDetail;
           this.freightBillOfLadingDetail = freightBillOfLadingDetail;
           this.returnInstructionsDetail = returnInstructionsDetail;
    }


    /**
     * Gets the shippingDocumentTypes value for this ShippingDocumentSpecification.
     * 
     * @return shippingDocumentTypes   * Indicates the types of shipping documents requested by the
     * shipper.
     */
    public com.fedex.ship.stub.RequestedShippingDocumentType[] getShippingDocumentTypes() {
        return shippingDocumentTypes;
    }


    /**
     * Sets the shippingDocumentTypes value for this ShippingDocumentSpecification.
     * 
     * @param shippingDocumentTypes   * Indicates the types of shipping documents requested by the
     * shipper.
     */
    public void setShippingDocumentTypes(com.fedex.ship.stub.RequestedShippingDocumentType[] shippingDocumentTypes) {
        this.shippingDocumentTypes = shippingDocumentTypes;
    }

    public com.fedex.ship.stub.RequestedShippingDocumentType getShippingDocumentTypes(int i) {
        return this.shippingDocumentTypes[i];
    }

    public void setShippingDocumentTypes(int i, com.fedex.ship.stub.RequestedShippingDocumentType _value) {
        this.shippingDocumentTypes[i] = _value;
    }


    /**
     * Gets the certificateOfOrigin value for this ShippingDocumentSpecification.
     * 
     * @return certificateOfOrigin
     */
    public com.fedex.ship.stub.CertificateOfOriginDetail getCertificateOfOrigin() {
        return certificateOfOrigin;
    }


    /**
     * Sets the certificateOfOrigin value for this ShippingDocumentSpecification.
     * 
     * @param certificateOfOrigin
     */
    public void setCertificateOfOrigin(com.fedex.ship.stub.CertificateOfOriginDetail certificateOfOrigin) {
        this.certificateOfOrigin = certificateOfOrigin;
    }


    /**
     * Gets the commercialInvoiceDetail value for this ShippingDocumentSpecification.
     * 
     * @return commercialInvoiceDetail
     */
    public com.fedex.ship.stub.CommercialInvoiceDetail getCommercialInvoiceDetail() {
        return commercialInvoiceDetail;
    }


    /**
     * Sets the commercialInvoiceDetail value for this ShippingDocumentSpecification.
     * 
     * @param commercialInvoiceDetail
     */
    public void setCommercialInvoiceDetail(com.fedex.ship.stub.CommercialInvoiceDetail commercialInvoiceDetail) {
        this.commercialInvoiceDetail = commercialInvoiceDetail;
    }


    /**
     * Gets the usmcaCommercialInvoiceCertificationOfOriginDetail value for this ShippingDocumentSpecification.
     * 
     * @return usmcaCommercialInvoiceCertificationOfOriginDetail
     */
    public com.fedex.ship.stub.UsmcaCommercialInvoiceCertificationOfOriginDetail getUsmcaCommercialInvoiceCertificationOfOriginDetail() {
        return usmcaCommercialInvoiceCertificationOfOriginDetail;
    }


    /**
     * Sets the usmcaCommercialInvoiceCertificationOfOriginDetail value for this ShippingDocumentSpecification.
     * 
     * @param usmcaCommercialInvoiceCertificationOfOriginDetail
     */
    public void setUsmcaCommercialInvoiceCertificationOfOriginDetail(com.fedex.ship.stub.UsmcaCommercialInvoiceCertificationOfOriginDetail usmcaCommercialInvoiceCertificationOfOriginDetail) {
        this.usmcaCommercialInvoiceCertificationOfOriginDetail = usmcaCommercialInvoiceCertificationOfOriginDetail;
    }


    /**
     * Gets the customPackageDocumentDetail value for this ShippingDocumentSpecification.
     * 
     * @return customPackageDocumentDetail   * Specifies the production of each package-level custom document
     * (the same specification is used for all packages).
     */
    public com.fedex.ship.stub.CustomDocumentDetail[] getCustomPackageDocumentDetail() {
        return customPackageDocumentDetail;
    }


    /**
     * Sets the customPackageDocumentDetail value for this ShippingDocumentSpecification.
     * 
     * @param customPackageDocumentDetail   * Specifies the production of each package-level custom document
     * (the same specification is used for all packages).
     */
    public void setCustomPackageDocumentDetail(com.fedex.ship.stub.CustomDocumentDetail[] customPackageDocumentDetail) {
        this.customPackageDocumentDetail = customPackageDocumentDetail;
    }

    public com.fedex.ship.stub.CustomDocumentDetail getCustomPackageDocumentDetail(int i) {
        return this.customPackageDocumentDetail[i];
    }

    public void setCustomPackageDocumentDetail(int i, com.fedex.ship.stub.CustomDocumentDetail _value) {
        this.customPackageDocumentDetail[i] = _value;
    }


    /**
     * Gets the customShipmentDocumentDetail value for this ShippingDocumentSpecification.
     * 
     * @return customShipmentDocumentDetail   * Specifies the production of a shipment-level custom document.
     */
    public com.fedex.ship.stub.CustomDocumentDetail[] getCustomShipmentDocumentDetail() {
        return customShipmentDocumentDetail;
    }


    /**
     * Sets the customShipmentDocumentDetail value for this ShippingDocumentSpecification.
     * 
     * @param customShipmentDocumentDetail   * Specifies the production of a shipment-level custom document.
     */
    public void setCustomShipmentDocumentDetail(com.fedex.ship.stub.CustomDocumentDetail[] customShipmentDocumentDetail) {
        this.customShipmentDocumentDetail = customShipmentDocumentDetail;
    }

    public com.fedex.ship.stub.CustomDocumentDetail getCustomShipmentDocumentDetail(int i) {
        return this.customShipmentDocumentDetail[i];
    }

    public void setCustomShipmentDocumentDetail(int i, com.fedex.ship.stub.CustomDocumentDetail _value) {
        this.customShipmentDocumentDetail[i] = _value;
    }


    /**
     * Gets the exportDeclarationDetail value for this ShippingDocumentSpecification.
     * 
     * @return exportDeclarationDetail
     */
    public com.fedex.ship.stub.ExportDeclarationDetail getExportDeclarationDetail() {
        return exportDeclarationDetail;
    }


    /**
     * Sets the exportDeclarationDetail value for this ShippingDocumentSpecification.
     * 
     * @param exportDeclarationDetail
     */
    public void setExportDeclarationDetail(com.fedex.ship.stub.ExportDeclarationDetail exportDeclarationDetail) {
        this.exportDeclarationDetail = exportDeclarationDetail;
    }


    /**
     * Gets the generalAgencyAgreementDetail value for this ShippingDocumentSpecification.
     * 
     * @return generalAgencyAgreementDetail
     */
    public com.fedex.ship.stub.GeneralAgencyAgreementDetail getGeneralAgencyAgreementDetail() {
        return generalAgencyAgreementDetail;
    }


    /**
     * Sets the generalAgencyAgreementDetail value for this ShippingDocumentSpecification.
     * 
     * @param generalAgencyAgreementDetail
     */
    public void setGeneralAgencyAgreementDetail(com.fedex.ship.stub.GeneralAgencyAgreementDetail generalAgencyAgreementDetail) {
        this.generalAgencyAgreementDetail = generalAgencyAgreementDetail;
    }


    /**
     * Gets the usmcaCertificationOfOriginDetail value for this ShippingDocumentSpecification.
     * 
     * @return usmcaCertificationOfOriginDetail
     */
    public com.fedex.ship.stub.UsmcaCertificationOfOriginDetail getUsmcaCertificationOfOriginDetail() {
        return usmcaCertificationOfOriginDetail;
    }


    /**
     * Sets the usmcaCertificationOfOriginDetail value for this ShippingDocumentSpecification.
     * 
     * @param usmcaCertificationOfOriginDetail
     */
    public void setUsmcaCertificationOfOriginDetail(com.fedex.ship.stub.UsmcaCertificationOfOriginDetail usmcaCertificationOfOriginDetail) {
        this.usmcaCertificationOfOriginDetail = usmcaCertificationOfOriginDetail;
    }


    /**
     * Gets the op900Detail value for this ShippingDocumentSpecification.
     * 
     * @return op900Detail   * Specifies the production of the OP-900 document for hazardous
     * materials packages.
     */
    public com.fedex.ship.stub.Op900Detail getOp900Detail() {
        return op900Detail;
    }


    /**
     * Sets the op900Detail value for this ShippingDocumentSpecification.
     * 
     * @param op900Detail   * Specifies the production of the OP-900 document for hazardous
     * materials packages.
     */
    public void setOp900Detail(com.fedex.ship.stub.Op900Detail op900Detail) {
        this.op900Detail = op900Detail;
    }


    /**
     * Gets the dangerousGoodsShippersDeclarationDetail value for this ShippingDocumentSpecification.
     * 
     * @return dangerousGoodsShippersDeclarationDetail   * Specifies the production of the 1421c document for dangerous
     * goods shipment.
     */
    public com.fedex.ship.stub.DangerousGoodsShippersDeclarationDetail getDangerousGoodsShippersDeclarationDetail() {
        return dangerousGoodsShippersDeclarationDetail;
    }


    /**
     * Sets the dangerousGoodsShippersDeclarationDetail value for this ShippingDocumentSpecification.
     * 
     * @param dangerousGoodsShippersDeclarationDetail   * Specifies the production of the 1421c document for dangerous
     * goods shipment.
     */
    public void setDangerousGoodsShippersDeclarationDetail(com.fedex.ship.stub.DangerousGoodsShippersDeclarationDetail dangerousGoodsShippersDeclarationDetail) {
        this.dangerousGoodsShippersDeclarationDetail = dangerousGoodsShippersDeclarationDetail;
    }


    /**
     * Gets the freightAddressLabelDetail value for this ShippingDocumentSpecification.
     * 
     * @return freightAddressLabelDetail   * Specifies the production of the OP-900 document for hazardous
     * materials.
     */
    public com.fedex.ship.stub.FreightAddressLabelDetail getFreightAddressLabelDetail() {
        return freightAddressLabelDetail;
    }


    /**
     * Sets the freightAddressLabelDetail value for this ShippingDocumentSpecification.
     * 
     * @param freightAddressLabelDetail   * Specifies the production of the OP-900 document for hazardous
     * materials.
     */
    public void setFreightAddressLabelDetail(com.fedex.ship.stub.FreightAddressLabelDetail freightAddressLabelDetail) {
        this.freightAddressLabelDetail = freightAddressLabelDetail;
    }


    /**
     * Gets the freightBillOfLadingDetail value for this ShippingDocumentSpecification.
     * 
     * @return freightBillOfLadingDetail
     */
    public com.fedex.ship.stub.FreightBillOfLadingDetail getFreightBillOfLadingDetail() {
        return freightBillOfLadingDetail;
    }


    /**
     * Sets the freightBillOfLadingDetail value for this ShippingDocumentSpecification.
     * 
     * @param freightBillOfLadingDetail
     */
    public void setFreightBillOfLadingDetail(com.fedex.ship.stub.FreightBillOfLadingDetail freightBillOfLadingDetail) {
        this.freightBillOfLadingDetail = freightBillOfLadingDetail;
    }


    /**
     * Gets the returnInstructionsDetail value for this ShippingDocumentSpecification.
     * 
     * @return returnInstructionsDetail   * Specifies the production of the return instructions document.
     */
    public com.fedex.ship.stub.ReturnInstructionsDetail getReturnInstructionsDetail() {
        return returnInstructionsDetail;
    }


    /**
     * Sets the returnInstructionsDetail value for this ShippingDocumentSpecification.
     * 
     * @param returnInstructionsDetail   * Specifies the production of the return instructions document.
     */
    public void setReturnInstructionsDetail(com.fedex.ship.stub.ReturnInstructionsDetail returnInstructionsDetail) {
        this.returnInstructionsDetail = returnInstructionsDetail;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof ShippingDocumentSpecification)) return false;
        ShippingDocumentSpecification other = (ShippingDocumentSpecification) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.shippingDocumentTypes==null && other.getShippingDocumentTypes()==null) || 
             (this.shippingDocumentTypes!=null &&
              java.util.Arrays.equals(this.shippingDocumentTypes, other.getShippingDocumentTypes()))) &&
            ((this.certificateOfOrigin==null && other.getCertificateOfOrigin()==null) || 
             (this.certificateOfOrigin!=null &&
              this.certificateOfOrigin.equals(other.getCertificateOfOrigin()))) &&
            ((this.commercialInvoiceDetail==null && other.getCommercialInvoiceDetail()==null) || 
             (this.commercialInvoiceDetail!=null &&
              this.commercialInvoiceDetail.equals(other.getCommercialInvoiceDetail()))) &&
            ((this.usmcaCommercialInvoiceCertificationOfOriginDetail==null && other.getUsmcaCommercialInvoiceCertificationOfOriginDetail()==null) || 
             (this.usmcaCommercialInvoiceCertificationOfOriginDetail!=null &&
              this.usmcaCommercialInvoiceCertificationOfOriginDetail.equals(other.getUsmcaCommercialInvoiceCertificationOfOriginDetail()))) &&
            ((this.customPackageDocumentDetail==null && other.getCustomPackageDocumentDetail()==null) || 
             (this.customPackageDocumentDetail!=null &&
              java.util.Arrays.equals(this.customPackageDocumentDetail, other.getCustomPackageDocumentDetail()))) &&
            ((this.customShipmentDocumentDetail==null && other.getCustomShipmentDocumentDetail()==null) || 
             (this.customShipmentDocumentDetail!=null &&
              java.util.Arrays.equals(this.customShipmentDocumentDetail, other.getCustomShipmentDocumentDetail()))) &&
            ((this.exportDeclarationDetail==null && other.getExportDeclarationDetail()==null) || 
             (this.exportDeclarationDetail!=null &&
              this.exportDeclarationDetail.equals(other.getExportDeclarationDetail()))) &&
            ((this.generalAgencyAgreementDetail==null && other.getGeneralAgencyAgreementDetail()==null) || 
             (this.generalAgencyAgreementDetail!=null &&
              this.generalAgencyAgreementDetail.equals(other.getGeneralAgencyAgreementDetail()))) &&
            ((this.usmcaCertificationOfOriginDetail==null && other.getUsmcaCertificationOfOriginDetail()==null) || 
             (this.usmcaCertificationOfOriginDetail!=null &&
              this.usmcaCertificationOfOriginDetail.equals(other.getUsmcaCertificationOfOriginDetail()))) &&
            ((this.op900Detail==null && other.getOp900Detail()==null) || 
             (this.op900Detail!=null &&
              this.op900Detail.equals(other.getOp900Detail()))) &&
            ((this.dangerousGoodsShippersDeclarationDetail==null && other.getDangerousGoodsShippersDeclarationDetail()==null) || 
             (this.dangerousGoodsShippersDeclarationDetail!=null &&
              this.dangerousGoodsShippersDeclarationDetail.equals(other.getDangerousGoodsShippersDeclarationDetail()))) &&
            ((this.freightAddressLabelDetail==null && other.getFreightAddressLabelDetail()==null) || 
             (this.freightAddressLabelDetail!=null &&
              this.freightAddressLabelDetail.equals(other.getFreightAddressLabelDetail()))) &&
            ((this.freightBillOfLadingDetail==null && other.getFreightBillOfLadingDetail()==null) || 
             (this.freightBillOfLadingDetail!=null &&
              this.freightBillOfLadingDetail.equals(other.getFreightBillOfLadingDetail()))) &&
            ((this.returnInstructionsDetail==null && other.getReturnInstructionsDetail()==null) || 
             (this.returnInstructionsDetail!=null &&
              this.returnInstructionsDetail.equals(other.getReturnInstructionsDetail())));
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
        if (getShippingDocumentTypes() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getShippingDocumentTypes());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getShippingDocumentTypes(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getCertificateOfOrigin() != null) {
            _hashCode += getCertificateOfOrigin().hashCode();
        }
        if (getCommercialInvoiceDetail() != null) {
            _hashCode += getCommercialInvoiceDetail().hashCode();
        }
        if (getUsmcaCommercialInvoiceCertificationOfOriginDetail() != null) {
            _hashCode += getUsmcaCommercialInvoiceCertificationOfOriginDetail().hashCode();
        }
        if (getCustomPackageDocumentDetail() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getCustomPackageDocumentDetail());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getCustomPackageDocumentDetail(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getCustomShipmentDocumentDetail() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getCustomShipmentDocumentDetail());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getCustomShipmentDocumentDetail(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getExportDeclarationDetail() != null) {
            _hashCode += getExportDeclarationDetail().hashCode();
        }
        if (getGeneralAgencyAgreementDetail() != null) {
            _hashCode += getGeneralAgencyAgreementDetail().hashCode();
        }
        if (getUsmcaCertificationOfOriginDetail() != null) {
            _hashCode += getUsmcaCertificationOfOriginDetail().hashCode();
        }
        if (getOp900Detail() != null) {
            _hashCode += getOp900Detail().hashCode();
        }
        if (getDangerousGoodsShippersDeclarationDetail() != null) {
            _hashCode += getDangerousGoodsShippersDeclarationDetail().hashCode();
        }
        if (getFreightAddressLabelDetail() != null) {
            _hashCode += getFreightAddressLabelDetail().hashCode();
        }
        if (getFreightBillOfLadingDetail() != null) {
            _hashCode += getFreightBillOfLadingDetail().hashCode();
        }
        if (getReturnInstructionsDetail() != null) {
            _hashCode += getReturnInstructionsDetail().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(ShippingDocumentSpecification.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentSpecification"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("shippingDocumentTypes");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShippingDocumentTypes"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RequestedShippingDocumentType"));
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("certificateOfOrigin");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CertificateOfOrigin"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CertificateOfOriginDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("commercialInvoiceDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CommercialInvoiceDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CommercialInvoiceDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("usmcaCommercialInvoiceCertificationOfOriginDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UsmcaCommercialInvoiceCertificationOfOriginDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UsmcaCommercialInvoiceCertificationOfOriginDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("customPackageDocumentDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomPackageDocumentDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomDocumentDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("customShipmentDocumentDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomShipmentDocumentDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomDocumentDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("exportDeclarationDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ExportDeclarationDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ExportDeclarationDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("generalAgencyAgreementDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "GeneralAgencyAgreementDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "GeneralAgencyAgreementDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("usmcaCertificationOfOriginDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UsmcaCertificationOfOriginDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "UsmcaCertificationOfOriginDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("op900Detail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Op900Detail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Op900Detail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dangerousGoodsShippersDeclarationDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DangerousGoodsShippersDeclarationDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DangerousGoodsShippersDeclarationDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("freightAddressLabelDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightAddressLabelDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightAddressLabelDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("freightBillOfLadingDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightBillOfLadingDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightBillOfLadingDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("returnInstructionsDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ReturnInstructionsDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ReturnInstructionsDetail"));
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
