/**
 * RequestedPackageLineItem.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public class RequestedPackageLineItem  implements java.io.Serializable {
    /* A unique identifier of each requested package line item. */
    private org.apache.axis.types.PositiveInteger sequenceNumber;

    /* An identifier of each group of identical packages. */
    private org.apache.axis.types.NonNegativeInteger groupNumber;

    /* Used as the number or count of identical packages in a group. */
    private org.apache.axis.types.NonNegativeInteger groupPackageCount;

    private com.fedex.ship.stub.VariableHandlingChargeDetail variableHandlingChargeDetail;

    /* Specifies the declared value for carriage of the package. The
     * declared value for carriage represents the maximum liability of FedEx
     * in connection with a shipment, including, but not limited to, any
     * loss, damage, delay, mis-delivery, nondelivery, misinformation, any
     * failure to provide information, or mis-delivery of information relating
     * to the package. */
    private com.fedex.ship.stub.Money insuredValue;

    /* Represents the gross weight of the package line item. */
    private com.fedex.ship.stub.Weight weight;

    private com.fedex.ship.stub.Dimensions dimensions;

    /* Provides additional detail on how the customer has physically
     * packaged this item. As of June 2009, required for packages moving
     * under international and SmartPost services. */
    private com.fedex.ship.stub.PhysicalPackagingType physicalPackaging;

    private com.fedex.ship.stub.AssociatedFreightLineItemDetail[] associatedFreightLineItems;

    /* Human-readable text describing the package. */
    private java.lang.String itemDescription;

    /* Human-readable text describing the contents of the package
     * to be used for clearance purposes. */
    private java.lang.String itemDescriptionForClearance;

    private com.fedex.ship.stub.CustomerReference[] customerReferences;

    private com.fedex.ship.stub.PackageSpecialServicesRequested specialServicesRequested;

    /* Details the contents of the package. */
    private com.fedex.ship.stub.ContentRecord[] contentRecords;

    private com.fedex.ship.stub.ShipperConveyanceDetail conveyanceDetail;

    public RequestedPackageLineItem() {
    }

    public RequestedPackageLineItem(
           org.apache.axis.types.PositiveInteger sequenceNumber,
           org.apache.axis.types.NonNegativeInteger groupNumber,
           org.apache.axis.types.NonNegativeInteger groupPackageCount,
           com.fedex.ship.stub.VariableHandlingChargeDetail variableHandlingChargeDetail,
           com.fedex.ship.stub.Money insuredValue,
           com.fedex.ship.stub.Weight weight,
           com.fedex.ship.stub.Dimensions dimensions,
           com.fedex.ship.stub.PhysicalPackagingType physicalPackaging,
           com.fedex.ship.stub.AssociatedFreightLineItemDetail[] associatedFreightLineItems,
           java.lang.String itemDescription,
           java.lang.String itemDescriptionForClearance,
           com.fedex.ship.stub.CustomerReference[] customerReferences,
           com.fedex.ship.stub.PackageSpecialServicesRequested specialServicesRequested,
           com.fedex.ship.stub.ContentRecord[] contentRecords,
           com.fedex.ship.stub.ShipperConveyanceDetail conveyanceDetail) {
           this.sequenceNumber = sequenceNumber;
           this.groupNumber = groupNumber;
           this.groupPackageCount = groupPackageCount;
           this.variableHandlingChargeDetail = variableHandlingChargeDetail;
           this.insuredValue = insuredValue;
           this.weight = weight;
           this.dimensions = dimensions;
           this.physicalPackaging = physicalPackaging;
           this.associatedFreightLineItems = associatedFreightLineItems;
           this.itemDescription = itemDescription;
           this.itemDescriptionForClearance = itemDescriptionForClearance;
           this.customerReferences = customerReferences;
           this.specialServicesRequested = specialServicesRequested;
           this.contentRecords = contentRecords;
           this.conveyanceDetail = conveyanceDetail;
    }


    /**
     * Gets the sequenceNumber value for this RequestedPackageLineItem.
     * 
     * @return sequenceNumber   * A unique identifier of each requested package line item.
     */
    public org.apache.axis.types.PositiveInteger getSequenceNumber() {
        return sequenceNumber;
    }


    /**
     * Sets the sequenceNumber value for this RequestedPackageLineItem.
     * 
     * @param sequenceNumber   * A unique identifier of each requested package line item.
     */
    public void setSequenceNumber(org.apache.axis.types.PositiveInteger sequenceNumber) {
        this.sequenceNumber = sequenceNumber;
    }


    /**
     * Gets the groupNumber value for this RequestedPackageLineItem.
     * 
     * @return groupNumber   * An identifier of each group of identical packages.
     */
    public org.apache.axis.types.NonNegativeInteger getGroupNumber() {
        return groupNumber;
    }


    /**
     * Sets the groupNumber value for this RequestedPackageLineItem.
     * 
     * @param groupNumber   * An identifier of each group of identical packages.
     */
    public void setGroupNumber(org.apache.axis.types.NonNegativeInteger groupNumber) {
        this.groupNumber = groupNumber;
    }


    /**
     * Gets the groupPackageCount value for this RequestedPackageLineItem.
     * 
     * @return groupPackageCount   * Used as the number or count of identical packages in a group.
     */
    public org.apache.axis.types.NonNegativeInteger getGroupPackageCount() {
        return groupPackageCount;
    }


    /**
     * Sets the groupPackageCount value for this RequestedPackageLineItem.
     * 
     * @param groupPackageCount   * Used as the number or count of identical packages in a group.
     */
    public void setGroupPackageCount(org.apache.axis.types.NonNegativeInteger groupPackageCount) {
        this.groupPackageCount = groupPackageCount;
    }


    /**
     * Gets the variableHandlingChargeDetail value for this RequestedPackageLineItem.
     * 
     * @return variableHandlingChargeDetail
     */
    public com.fedex.ship.stub.VariableHandlingChargeDetail getVariableHandlingChargeDetail() {
        return variableHandlingChargeDetail;
    }


    /**
     * Sets the variableHandlingChargeDetail value for this RequestedPackageLineItem.
     * 
     * @param variableHandlingChargeDetail
     */
    public void setVariableHandlingChargeDetail(com.fedex.ship.stub.VariableHandlingChargeDetail variableHandlingChargeDetail) {
        this.variableHandlingChargeDetail = variableHandlingChargeDetail;
    }


    /**
     * Gets the insuredValue value for this RequestedPackageLineItem.
     * 
     * @return insuredValue   * Specifies the declared value for carriage of the package. The
     * declared value for carriage represents the maximum liability of FedEx
     * in connection with a shipment, including, but not limited to, any
     * loss, damage, delay, mis-delivery, nondelivery, misinformation, any
     * failure to provide information, or mis-delivery of information relating
     * to the package.
     */
    public com.fedex.ship.stub.Money getInsuredValue() {
        return insuredValue;
    }


    /**
     * Sets the insuredValue value for this RequestedPackageLineItem.
     * 
     * @param insuredValue   * Specifies the declared value for carriage of the package. The
     * declared value for carriage represents the maximum liability of FedEx
     * in connection with a shipment, including, but not limited to, any
     * loss, damage, delay, mis-delivery, nondelivery, misinformation, any
     * failure to provide information, or mis-delivery of information relating
     * to the package.
     */
    public void setInsuredValue(com.fedex.ship.stub.Money insuredValue) {
        this.insuredValue = insuredValue;
    }


    /**
     * Gets the weight value for this RequestedPackageLineItem.
     * 
     * @return weight   * Represents the gross weight of the package line item.
     */
    public com.fedex.ship.stub.Weight getWeight() {
        return weight;
    }


    /**
     * Sets the weight value for this RequestedPackageLineItem.
     * 
     * @param weight   * Represents the gross weight of the package line item.
     */
    public void setWeight(com.fedex.ship.stub.Weight weight) {
        this.weight = weight;
    }


    /**
     * Gets the dimensions value for this RequestedPackageLineItem.
     * 
     * @return dimensions
     */
    public com.fedex.ship.stub.Dimensions getDimensions() {
        return dimensions;
    }


    /**
     * Sets the dimensions value for this RequestedPackageLineItem.
     * 
     * @param dimensions
     */
    public void setDimensions(com.fedex.ship.stub.Dimensions dimensions) {
        this.dimensions = dimensions;
    }


    /**
     * Gets the physicalPackaging value for this RequestedPackageLineItem.
     * 
     * @return physicalPackaging   * Provides additional detail on how the customer has physically
     * packaged this item. As of June 2009, required for packages moving
     * under international and SmartPost services.
     */
    public com.fedex.ship.stub.PhysicalPackagingType getPhysicalPackaging() {
        return physicalPackaging;
    }


    /**
     * Sets the physicalPackaging value for this RequestedPackageLineItem.
     * 
     * @param physicalPackaging   * Provides additional detail on how the customer has physically
     * packaged this item. As of June 2009, required for packages moving
     * under international and SmartPost services.
     */
    public void setPhysicalPackaging(com.fedex.ship.stub.PhysicalPackagingType physicalPackaging) {
        this.physicalPackaging = physicalPackaging;
    }


    /**
     * Gets the associatedFreightLineItems value for this RequestedPackageLineItem.
     * 
     * @return associatedFreightLineItems
     */
    public com.fedex.ship.stub.AssociatedFreightLineItemDetail[] getAssociatedFreightLineItems() {
        return associatedFreightLineItems;
    }


    /**
     * Sets the associatedFreightLineItems value for this RequestedPackageLineItem.
     * 
     * @param associatedFreightLineItems
     */
    public void setAssociatedFreightLineItems(com.fedex.ship.stub.AssociatedFreightLineItemDetail[] associatedFreightLineItems) {
        this.associatedFreightLineItems = associatedFreightLineItems;
    }

    public com.fedex.ship.stub.AssociatedFreightLineItemDetail getAssociatedFreightLineItems(int i) {
        return this.associatedFreightLineItems[i];
    }

    public void setAssociatedFreightLineItems(int i, com.fedex.ship.stub.AssociatedFreightLineItemDetail _value) {
        this.associatedFreightLineItems[i] = _value;
    }


    /**
     * Gets the itemDescription value for this RequestedPackageLineItem.
     * 
     * @return itemDescription   * Human-readable text describing the package.
     */
    public java.lang.String getItemDescription() {
        return itemDescription;
    }


    /**
     * Sets the itemDescription value for this RequestedPackageLineItem.
     * 
     * @param itemDescription   * Human-readable text describing the package.
     */
    public void setItemDescription(java.lang.String itemDescription) {
        this.itemDescription = itemDescription;
    }


    /**
     * Gets the itemDescriptionForClearance value for this RequestedPackageLineItem.
     * 
     * @return itemDescriptionForClearance   * Human-readable text describing the contents of the package
     * to be used for clearance purposes.
     */
    public java.lang.String getItemDescriptionForClearance() {
        return itemDescriptionForClearance;
    }


    /**
     * Sets the itemDescriptionForClearance value for this RequestedPackageLineItem.
     * 
     * @param itemDescriptionForClearance   * Human-readable text describing the contents of the package
     * to be used for clearance purposes.
     */
    public void setItemDescriptionForClearance(java.lang.String itemDescriptionForClearance) {
        this.itemDescriptionForClearance = itemDescriptionForClearance;
    }


    /**
     * Gets the customerReferences value for this RequestedPackageLineItem.
     * 
     * @return customerReferences
     */
    public com.fedex.ship.stub.CustomerReference[] getCustomerReferences() {
        return customerReferences;
    }


    /**
     * Sets the customerReferences value for this RequestedPackageLineItem.
     * 
     * @param customerReferences
     */
    public void setCustomerReferences(com.fedex.ship.stub.CustomerReference[] customerReferences) {
        this.customerReferences = customerReferences;
    }

    public com.fedex.ship.stub.CustomerReference getCustomerReferences(int i) {
        return this.customerReferences[i];
    }

    public void setCustomerReferences(int i, com.fedex.ship.stub.CustomerReference _value) {
        this.customerReferences[i] = _value;
    }


    /**
     * Gets the specialServicesRequested value for this RequestedPackageLineItem.
     * 
     * @return specialServicesRequested
     */
    public com.fedex.ship.stub.PackageSpecialServicesRequested getSpecialServicesRequested() {
        return specialServicesRequested;
    }


    /**
     * Sets the specialServicesRequested value for this RequestedPackageLineItem.
     * 
     * @param specialServicesRequested
     */
    public void setSpecialServicesRequested(com.fedex.ship.stub.PackageSpecialServicesRequested specialServicesRequested) {
        this.specialServicesRequested = specialServicesRequested;
    }


    /**
     * Gets the contentRecords value for this RequestedPackageLineItem.
     * 
     * @return contentRecords   * Details the contents of the package.
     */
    public com.fedex.ship.stub.ContentRecord[] getContentRecords() {
        return contentRecords;
    }


    /**
     * Sets the contentRecords value for this RequestedPackageLineItem.
     * 
     * @param contentRecords   * Details the contents of the package.
     */
    public void setContentRecords(com.fedex.ship.stub.ContentRecord[] contentRecords) {
        this.contentRecords = contentRecords;
    }

    public com.fedex.ship.stub.ContentRecord getContentRecords(int i) {
        return this.contentRecords[i];
    }

    public void setContentRecords(int i, com.fedex.ship.stub.ContentRecord _value) {
        this.contentRecords[i] = _value;
    }


    /**
     * Gets the conveyanceDetail value for this RequestedPackageLineItem.
     * 
     * @return conveyanceDetail
     */
    public com.fedex.ship.stub.ShipperConveyanceDetail getConveyanceDetail() {
        return conveyanceDetail;
    }


    /**
     * Sets the conveyanceDetail value for this RequestedPackageLineItem.
     * 
     * @param conveyanceDetail
     */
    public void setConveyanceDetail(com.fedex.ship.stub.ShipperConveyanceDetail conveyanceDetail) {
        this.conveyanceDetail = conveyanceDetail;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof RequestedPackageLineItem)) return false;
        RequestedPackageLineItem other = (RequestedPackageLineItem) obj;
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
            ((this.groupNumber==null && other.getGroupNumber()==null) || 
             (this.groupNumber!=null &&
              this.groupNumber.equals(other.getGroupNumber()))) &&
            ((this.groupPackageCount==null && other.getGroupPackageCount()==null) || 
             (this.groupPackageCount!=null &&
              this.groupPackageCount.equals(other.getGroupPackageCount()))) &&
            ((this.variableHandlingChargeDetail==null && other.getVariableHandlingChargeDetail()==null) || 
             (this.variableHandlingChargeDetail!=null &&
              this.variableHandlingChargeDetail.equals(other.getVariableHandlingChargeDetail()))) &&
            ((this.insuredValue==null && other.getInsuredValue()==null) || 
             (this.insuredValue!=null &&
              this.insuredValue.equals(other.getInsuredValue()))) &&
            ((this.weight==null && other.getWeight()==null) || 
             (this.weight!=null &&
              this.weight.equals(other.getWeight()))) &&
            ((this.dimensions==null && other.getDimensions()==null) || 
             (this.dimensions!=null &&
              this.dimensions.equals(other.getDimensions()))) &&
            ((this.physicalPackaging==null && other.getPhysicalPackaging()==null) || 
             (this.physicalPackaging!=null &&
              this.physicalPackaging.equals(other.getPhysicalPackaging()))) &&
            ((this.associatedFreightLineItems==null && other.getAssociatedFreightLineItems()==null) || 
             (this.associatedFreightLineItems!=null &&
              java.util.Arrays.equals(this.associatedFreightLineItems, other.getAssociatedFreightLineItems()))) &&
            ((this.itemDescription==null && other.getItemDescription()==null) || 
             (this.itemDescription!=null &&
              this.itemDescription.equals(other.getItemDescription()))) &&
            ((this.itemDescriptionForClearance==null && other.getItemDescriptionForClearance()==null) || 
             (this.itemDescriptionForClearance!=null &&
              this.itemDescriptionForClearance.equals(other.getItemDescriptionForClearance()))) &&
            ((this.customerReferences==null && other.getCustomerReferences()==null) || 
             (this.customerReferences!=null &&
              java.util.Arrays.equals(this.customerReferences, other.getCustomerReferences()))) &&
            ((this.specialServicesRequested==null && other.getSpecialServicesRequested()==null) || 
             (this.specialServicesRequested!=null &&
              this.specialServicesRequested.equals(other.getSpecialServicesRequested()))) &&
            ((this.contentRecords==null && other.getContentRecords()==null) || 
             (this.contentRecords!=null &&
              java.util.Arrays.equals(this.contentRecords, other.getContentRecords()))) &&
            ((this.conveyanceDetail==null && other.getConveyanceDetail()==null) || 
             (this.conveyanceDetail!=null &&
              this.conveyanceDetail.equals(other.getConveyanceDetail())));
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
        if (getGroupNumber() != null) {
            _hashCode += getGroupNumber().hashCode();
        }
        if (getGroupPackageCount() != null) {
            _hashCode += getGroupPackageCount().hashCode();
        }
        if (getVariableHandlingChargeDetail() != null) {
            _hashCode += getVariableHandlingChargeDetail().hashCode();
        }
        if (getInsuredValue() != null) {
            _hashCode += getInsuredValue().hashCode();
        }
        if (getWeight() != null) {
            _hashCode += getWeight().hashCode();
        }
        if (getDimensions() != null) {
            _hashCode += getDimensions().hashCode();
        }
        if (getPhysicalPackaging() != null) {
            _hashCode += getPhysicalPackaging().hashCode();
        }
        if (getAssociatedFreightLineItems() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getAssociatedFreightLineItems());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getAssociatedFreightLineItems(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getItemDescription() != null) {
            _hashCode += getItemDescription().hashCode();
        }
        if (getItemDescriptionForClearance() != null) {
            _hashCode += getItemDescriptionForClearance().hashCode();
        }
        if (getCustomerReferences() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getCustomerReferences());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getCustomerReferences(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getSpecialServicesRequested() != null) {
            _hashCode += getSpecialServicesRequested().hashCode();
        }
        if (getContentRecords() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getContentRecords());
                 i++) {
                java.lang.Object obj = java.lang.reflect.Array.get(getContentRecords(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getConveyanceDetail() != null) {
            _hashCode += getConveyanceDetail().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(RequestedPackageLineItem.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "RequestedPackageLineItem"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("sequenceNumber");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SequenceNumber"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "positiveInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("groupNumber");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "GroupNumber"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "nonNegativeInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("groupPackageCount");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "GroupPackageCount"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "nonNegativeInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("variableHandlingChargeDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "VariableHandlingChargeDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "VariableHandlingChargeDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("insuredValue");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "InsuredValue"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Money"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("weight");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Weight"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Weight"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("dimensions");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Dimensions"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Dimensions"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("physicalPackaging");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PhysicalPackaging"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PhysicalPackagingType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("associatedFreightLineItems");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AssociatedFreightLineItems"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "AssociatedFreightLineItemDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("itemDescription");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ItemDescription"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("itemDescriptionForClearance");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ItemDescriptionForClearance"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("customerReferences");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomerReferences"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "CustomerReference"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("specialServicesRequested");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "SpecialServicesRequested"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PackageSpecialServicesRequested"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("contentRecords");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ContentRecords"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ContentRecord"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("conveyanceDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ConveyanceDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipperConveyanceDetail"));
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
