/**
 * CustomsClearanceDetail.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class CustomsClearanceDetail  implements java.io.Serializable {
    private com.fedex.ship.stub.BrokerDetail[] brokers;

    /* Interacts both with properties of the shipment and contractual
     * relationship with the shipper. */
    private com.fedex.ship.stub.ClearanceBrokerageType clearanceBrokerage;

    private com.fedex.ship.stub.CustomsOptionDetail customsOptions;

    private com.fedex.ship.stub.Party importerOfRecord;

    /* Specifies how the recipient is identified for customs purposes;
     * the requirements on this information vary with destination country. */
    private com.fedex.ship.stub.RecipientCustomsId recipientCustomsId;

    private com.fedex.ship.stub.Payment dutiesPayment;

    private com.fedex.ship.stub.InternationalDocumentContentType documentContent;

    /* This represents the total commodity value of the shipment plus
     * the charges required by the destination country. */
    private com.fedex.ship.stub.Money customsValue;

    /* Identifies responsibilities with respect to loss, damage, etc. */
    private com.fedex.ship.stub.FreightOnValueType freightOnValue;

    /* Documents amount paid to third party for coverage of shipment
     * content. */
    private com.fedex.ship.stub.Money insuranceCharges;

    private java.lang.Boolean partiesToTransactionAreRelated;

    private com.fedex.ship.stub.CommercialInvoice commercialInvoice;

    private com.fedex.ship.stub.Commodity[] commodities;

    private com.fedex.ship.stub.ExportDetail exportDetail;

    private com.fedex.ship.stub.RegulatoryControlType[] regulatoryControls;

    private com.fedex.ship.stub.CustomsDeclarationStatementDetail declarationStatementDetail;

    public CustomsClearanceDetail() {
    }

    public CustomsClearanceDetail(
           com.fedex.ship.stub.BrokerDetail[] brokers,
           com.fedex.ship.stub.ClearanceBrokerageType clearanceBrokerage,
           com.fedex.ship.stub.CustomsOptionDetail customsOptions,
           com.fedex.ship.stub.Party importerOfRecord,
           com.fedex.ship.stub.RecipientCustomsId recipientCustomsId,
           com.fedex.ship.stub.Payment dutiesPayment,
           com.fedex.ship.stub.InternationalDocumentContentType documentContent,
           com.fedex.ship.stub.Money customsValue,
           com.fedex.ship.stub.FreightOnValueType freightOnValue,
           com.fedex.ship.stub.Money insuranceCharges,
           java.lang.Boolean partiesToTransactionAreRelated,
           com.fedex.ship.stub.CommercialInvoice commercialInvoice,
           com.fedex.ship.stub.Commodity[] commodities,
           com.fedex.ship.stub.ExportDetail exportDetail,
           com.fedex.ship.stub.RegulatoryControlType[] regulatoryControls,
           com.fedex.ship.stub.CustomsDeclarationStatementDetail declarationStatementDetail) {
           this.brokers = brokers;
           this.clearanceBrokerage = clearanceBrokerage;
           this.customsOptions = customsOptions;
           this.importerOfRecord = importerOfRecord;
           this.recipientCustomsId = recipientCustomsId;
           this.dutiesPayment = dutiesPayment;
           this.documentContent = documentContent;
           this.customsValue = customsValue;
           this.freightOnValue = freightOnValue;
           this.insuranceCharges = insuranceCharges;
           this.partiesToTransactionAreRelated = partiesToTransactionAreRelated;
           this.commercialInvoice = commercialInvoice;
           this.commodities = commodities;
           this.exportDetail = exportDetail;
           this.regulatoryControls = regulatoryControls;
           this.declarationStatementDetail = declarationStatementDetail;
    }


    /**
     * Gets the brokers value for this CustomsClearanceDetail.
     * 
     * @return brokers
     */
    public com.fedex.ship.stub.BrokerDetail[] getBrokers() {
        return brokers;
    }


    /**
     * Sets the brokers value for this CustomsClearanceDetail.
     * 
     * @param brokers
     */
    public void setBrokers(com.fedex.ship.stub.BrokerDetail[] brokers) {
        this.brokers = brokers;
    }

    public com.fedex.ship.stub.BrokerDetail getBrokers(int i) {
        return this.brokers[i];
    }

    public void setBrokers(int i, com.fedex.ship.stub.BrokerDetail _value) {
        this.brokers[i] = _value;
    }


    /**
     * Gets the clearanceBrokerage value for this CustomsClearanceDetail.
     * 
     * @return clearanceBrokerage   * Interacts both with properties of the shipment and contractual
     * relationship with the shipper.
     */
    public com.fedex.ship.stub.ClearanceBrokerageType getClearanceBrokerage() {
        return clearanceBrokerage;
    }


    /**
     * Sets the clearanceBrokerage value for this CustomsClearanceDetail.
     * 
     * @param clearanceBrokerage   * Interacts both with properties of the shipment and contractual
     * relationship with the shipper.
     */
    public void setClearanceBrokerage(com.fedex.ship.stub.ClearanceBrokerageType clearanceBrokerage) {
        this.clearanceBrokerage = clearanceBrokerage;
    }


    /**
     * Gets the customsOptions value for this CustomsClearanceDetail.
     * 
     * @return customsOptions
     */
    public com.fedex.ship.stub.CustomsOptionDetail getCustomsOptions() {
        return customsOptions;
    }


    /**
     * Sets the customsOptions value for this CustomsClearanceDetail.
     * 
     * @param customsOptions
     */
    public void setCustomsOptions(com.fedex.ship.stub.CustomsOptionDetail customsOptions) {
        this.customsOptions = customsOptions;
    }


    /**
     * Gets the importerOfRecord value for this CustomsClearanceDetail.
     * 
     * @return importerOfRecord
     */
    public com.fedex.ship.stub.Party getImporterOfRecord() {
        return importerOfRecord;
    }


    /**
     * Sets the importerOfRecord value for this CustomsClearanceDetail.
     * 
     * @param importerOfRecord
     */
    public void setImporterOfRecord(com.fedex.ship.stub.Party importerOfRecord) {
        this.importerOfRecord = importerOfRecord;
    }


    /**
     * Gets the recipientCustomsId value for this CustomsClearanceDetail.
     * 
     * @return recipientCustomsId   * Specifies how the recipient is identified for customs purposes;
     * the requirements on this information vary with destination country.
     */
    public com.fedex.ship.stub.RecipientCustomsId getRecipientCustomsId() {
        return recipientCustomsId;
    }


    /**
     * Sets the recipientCustomsId value for this CustomsClearanceDetail.
     * 
     * @param recipientCustomsId   * Specifies how the recipient is identified for customs purposes;
     * the requirements on this information vary with destination country.
     */
    public void setRecipientCustomsId(com.fedex.ship.stub.RecipientCustomsId recipientCustomsId) {
        this.recipientCustomsId = recipientCustomsId;
    }


    /**
     * Gets the dutiesPayment value for this CustomsClearanceDetail.
     * 
     * @return dutiesPayment
     */
    public com.fedex.ship.stub.Payment getDutiesPayment() {
        return dutiesPayment;
    }


    /**
     * Sets the dutiesPayment value for this CustomsClearanceDetail.
     * 
     * @param dutiesPayment
     */
    public void setDutiesPayment(com.fedex.ship.stub.Payment dutiesPayment) {
        this.dutiesPayment = dutiesPayment;
    }


    /**
     * Gets the documentContent value for this CustomsClearanceDetail.
     * 
     * @return documentContent
     */
    public com.fedex.ship.stub.InternationalDocumentContentType getDocumentContent() {
        return documentContent;
    }


    /**
     * Sets the documentContent value for this CustomsClearanceDetail.
     * 
     * @param documentContent
     */
    public void setDocumentContent(com.fedex.ship.stub.InternationalDocumentContentType documentContent) {
        this.documentContent = documentContent;
    }


    /**
     * Gets the customsValue value for this CustomsClearanceDetail.
     * 
     * @return customsValue   * This represents the total commodity value of the shipment plus
     * the charges required by the destination country.
     */
    public com.fedex.ship.stub.Money getCustomsValue() {
        return customsValue;
    }


    /**
     * Sets the customsValue value for this CustomsClearanceDetail.
     * 
     * @param customsValue   * This represents the total commodity value of the shipment plus
     * the charges required by the destination country.
     */
    public void setCustomsValue(com.fedex.ship.stub.Money customsValue) {
        this.customsValue = customsValue;
    }


    /**
     * Gets the freightOnValue value for this CustomsClearanceDetail.
     * 
     * @return freightOnValue   * Identifies responsibilities with respect to loss, damage, etc.
     */
    public com.fedex.ship.stub.FreightOnValueType getFreightOnValue() {
        return freightOnValue;
    }


    /**
     * Sets the freightOnValue value for this CustomsClearanceDetail.
     * 
     * @param freightOnValue   * Identifies responsibilities with respect to loss, damage, etc.
     */
    public void setFreightOnValue(com.fedex.ship.stub.FreightOnValueType freightOnValue) {
        this.freightOnValue = freightOnValue;
    }


    /**
     * Gets the insuranceCharges value for this CustomsClearanceDetail.
     * 
     * @return insuranceCharges   * Documents amount paid to third party for coverage of shipment
     * content.
     */
    public com.fedex.ship.stub.Money getInsuranceCharges() {
        return insuranceCharges;
    }


    /**
     * Sets the insuranceCharges value for this CustomsClearanceDetail.
     * 
     * @param insuranceCharges   * Documents amount paid to third party for coverage of shipment
     * content.
     */
    public void setInsuranceCharges(com.fedex.ship.stub.Money insuranceCharges) {
        this.insuranceCharges = insuranceCharges;
    }


    /**
     * Gets the partiesToTransactionAreRelated value for this CustomsClearanceDetail.
     * 
     * @return partiesToTransactionAreRelated
     */
    public java.lang.Boolean getPartiesToTransactionAreRelated() {
        return partiesToTransactionAreRelated;
    }


    /**
     * Sets the partiesToTransactionAreRelated value for this CustomsClearanceDetail.
     * 
     * @param partiesToTransactionAreRelated
     */
    public void setPartiesToTransactionAreRelated(java.lang.Boolean partiesToTransactionAreRelated) {
        this.partiesToTransactionAreRelated = partiesToTransactionAreRelated;
    }


    /**
     * Gets the commercialInvoice value for this CustomsClearanceDetail.
     * 
     * @return commercialInvoice
     */
    public com.fedex.ship.stub.CommercialInvoice getCommercialInvoice() {
        return commercialInvoice;
    }


    /**
     * Sets the commercialInvoice value for this CustomsClearanceDetail.
     * 
     * @param commercialInvoice
     */
    public void setCommercialInvoice(com.fedex.ship.stub.CommercialInvoice commercialInvoice) {
        this.commercialInvoice = commercialInvoice;
    }


    /**
     * Gets the commodities value for this CustomsClearanceDetail.
     * 
     * @return commodities
     */
    public com.fedex.ship.stub.Commodity[] getCommodities() {
        return commodities;
    }


    /**
     * Sets the commodities value for this CustomsClearanceDetail.
     * 
     * @param commodities
     */
    public void setCommodities(com.fedex.ship.stub.Commodity[] commodities) {
        this.commodities = commodities;
    }

    public com.fedex.ship.stub.Commodity getCommodities(int i) {
        return this.commodities[i];
    }

    public void setCommodities(int i, com.fedex.ship.stub.Commodity _value) {
        this.commodities[i] = _value;
    }


    /**
     * Gets the exportDetail value for this CustomsClearanceDetail.
     * 
     * @return exportDetail
     */
    public com.fedex.ship.stub.ExportDetail getExportDetail() {
        return exportDetail;
    }


    /**
     * Sets the exportDetail value for this CustomsClearanceDetail.
     * 
     * @param exportDetail
     */
    public void setExportDetail(com.fedex.ship.stub.ExportDetail exportDetail) {
        this.exportDetail = exportDetail;
    }


    /**
     * Gets the regulatoryControls value for this CustomsClearanceDetail.
     * 
     * @return regulatoryControls
     */
    public com.fedex.ship.stub.RegulatoryControlType[] getRegulatoryControls() {
        return regulatoryControls;
    }


    /**
     * Sets the regulatoryControls value for this CustomsClearanceDetail.
     * 
     * @param regulatoryControls
     */
    public void setRegulatoryControls(com.fedex.ship.stub.RegulatoryControlType[] regulatoryControls) {
        this.regulatoryControls = regulatoryControls;
    }

    public com.fedex.ship.stub.RegulatoryControlType getRegulatoryControls(int i) {
        return this.regulatoryControls[i];
    }

    public void setRegulatoryControls(int i, com.fedex.ship.stub.RegulatoryControlType _value) {
        this.regulatoryControls[i] = _value;
    }


    /**
     * Gets the declarationStatementDetail value for this CustomsClearanceDetail.
     * 
     * @return declarationStatementDetail
     */
    public com.fedex.ship.stub.CustomsDeclarationStatementDetail getDeclarationStatementDetail() {
        return declarationStatementDetail;
    }


    /**
     * Sets the declarationStatementDetail value for this CustomsClearanceDetail.
     * 
     * @param declarationStatementDetail
     */
    public void setDeclarationStatementDetail(com.fedex.ship.stub.CustomsDeclarationStatementDetail declarationStatementDetail) {
        this.declarationStatementDetail = declarationStatementDetail;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof CustomsClearanceDetail)) return false;
        CustomsClearanceDetail other = (CustomsClearanceDetail) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true && 
            ((this.brokers==null && other.getBrokers()==null) || 
             (this.brokers!=null &&
              java.util.Arrays.equals(this.brokers, other.getBrokers()))) &&
            ((this.clearanceBrokerage==null && other.getClearanceBrokerage()==null) || 
             (this.clearanceBrokerage!=null &&
              this.clearanceBrokerage.equals(other.getClearanceBrokerage()))) &&
            ((this.customsOptions==null && other.getCustomsOptions()==null) || 
             (this.customsOptions!=null &&
              this.customsOptions.equals(other.getCustomsOptions()))) &&
            ((this.importerOfRecord==null && other.getImporterOfRecord()==null) || 
             (this.importerOfRecord!=null &&
              this.importerOfRecord.equals(other.getImporterOfRecord()))) &&
            ((this.recipientCustomsId==null && other.getRecipientCustomsId()==null) || 
             (this.recipientCustomsId!=null &&
              this.recipientCustomsId.equals(other.getRecipientCustomsId()))) &&
            ((this.dutiesPayment==null && other.getDutiesPayment()==null) || 
             (this.dutiesPayment!=null &&
              this.dutiesPayment.equals(other.getDutiesPayment()))) &&
            ((this.documentContent==null && other.getDocumentContent()==null) || 
             (this.documentContent!=null &&
              this.documentContent.equals(other.getDocumentContent()))) &&
            ((this.customsValue==null && other.getCustomsValue()==null) || 
             (this.customsValue!=null &&
              this.customsValue.equals(other.getCustomsValue()))) &&
            ((this.freightOnValue==null && other.getFreightOnValue()==null) || 
             (this.freightOnValue!=null &&
              this.freightOnValue.equals(other.getFreightOnValue()))) &&
            ((this.insuranceCharges==null && other.getInsuranceCharges()==null) || 
             (this.insuranceCharges!=null &&
              this.insuranceCharges.equals(other.getInsuranceCharges()))) &&
            ((this.partiesToTransactionAreRelated==null && other.getPartiesToTransactionAreRelated()==null) || 
             (this.partiesToTransactionAreRelated!=null &&
              this.partiesToTransactionAreRelated.equals(other.getPartiesToTransactionAreRelated()))) &&
            ((this.commercialInvoice==null && other.getCommercialInvoice()==null) || 
             (this.commercialInvoice!=null &&
              this.commercialInvoice.equals(other.getCommercialInvoice()))) &&
            ((this.commodities==null && other.getCommodities()==null) || 
             (this.commodities!=null &&
              java.util.Arrays.equals(this.commodities, other.getCommodities()))) &&
            ((this.exportDetail==null && other.getExportDetail()==null) || 
             (this.exportDetail!=null &&
              this.exportDetail.equals(other.getExportDetail()))) &&
            ((this.regulatoryControls==null && other.getRegulatoryControls()==null) || 
             (this.regulatoryControls!=null &&
              java.util.Arrays.equals(this.regulatoryControls, other.getRegulatoryControls()))) &&
            ((this.declarationStatementDetail==null && other.getDeclarationStatementDetail()==null) || 
             (this.declarationStatementDetail!=null &&
              this.declarationStatementDetail.equals(other.getDeclarationStatementDetail())));
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
        if (getBrokers() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getBrokers());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getBrokers(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getClearanceBrokerage() != null) {
            _hashCode += getClearanceBrokerage().hashCode();
        }
        if (getCustomsOptions() != null) {
            _hashCode += getCustomsOptions().hashCode();
        }
        if (getImporterOfRecord() != null) {
            _hashCode += getImporterOfRecord().hashCode();
        }
        if (getRecipientCustomsId() != null) {
            _hashCode += getRecipientCustomsId().hashCode();
        }
        if (getDutiesPayment() != null) {
            _hashCode += getDutiesPayment().hashCode();
        }
        if (getDocumentContent() != null) {
            _hashCode += getDocumentContent().hashCode();
        }
        if (getCustomsValue() != null) {
            _hashCode += getCustomsValue().hashCode();
        }
        if (getFreightOnValue() != null) {
            _hashCode += getFreightOnValue().hashCode();
        }
        if (getInsuranceCharges() != null) {
            _hashCode += getInsuranceCharges().hashCode();
        }
        if (getPartiesToTransactionAreRelated() != null) {
            _hashCode += getPartiesToTransactionAreRelated().hashCode();
        }
        if (getCommercialInvoice() != null) {
            _hashCode += getCommercialInvoice().hashCode();
        }
        if (getCommodities() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getCommodities());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getCommodities(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getExportDetail() != null) {
            _hashCode += getExportDetail().hashCode();
        }
        if (getRegulatoryControls() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getRegulatoryControls());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getRegulatoryControls(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getDeclarationStatementDetail() != null) {
            _hashCode += getDeclarationStatementDetail().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(CustomsClearanceDetail.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomsClearanceDetail"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("brokers");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Brokers"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "BrokerDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("clearanceBrokerage");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ClearanceBrokerage"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ClearanceBrokerageType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("customsOptions");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomsOptions"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomsOptionDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("importerOfRecord");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ImporterOfRecord"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Party"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("recipientCustomsId");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RecipientCustomsId"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RecipientCustomsId"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dutiesPayment");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DutiesPayment"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Payment"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("documentContent");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DocumentContent"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "InternationalDocumentContentType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("customsValue");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomsValue"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("freightOnValue");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightOnValue"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightOnValueType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("insuranceCharges");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "InsuranceCharges"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("partiesToTransactionAreRelated");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PartiesToTransactionAreRelated"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "boolean"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("commercialInvoice");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CommercialInvoice"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CommercialInvoice"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("commodities");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Commodities"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Commodity"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("exportDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ExportDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ExportDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("regulatoryControls");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RegulatoryControls"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RegulatoryControlType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("declarationStatementDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "DeclarationStatementDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomsDeclarationStatementDetail"));
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
