/**
 * FreightShipmentLineItem.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;


/**
 * Description of an individual commodity or class of content in a
 * shipment.
 */
public class FreightShipmentLineItem  implements java.io.Serializable {
    /* A unique identifier assigned to this line item. */
    private java.lang.String id;

    /* Freight class for this line item. */
    private com.fedex.ship.stub.FreightClassType freightClass;

    /* FEDEX INTERNAL USE ONLY: for FedEx system that estimate freight
     * class from customer-provided dimensions and weight. */
    private java.lang.Boolean classProvidedByCustomer;

    /* Number of individual handling units to which this line applies.
     * (NOTE: Total of line-item-level handling units may not balance to
     * shipment-level total handling units.) */
    private org.apache.axis.types.NonNegativeInteger handlingUnits;

    /* Specification of handling-unit packaging for this commodity
     * or class line. */
    private com.fedex.ship.stub.PhysicalPackagingType packaging;

    /* Number of pieces for this commodity or class line. */
    private org.apache.axis.types.NonNegativeInteger pieces;

    /* NMFC Code for commodity. */
    private java.lang.String nmfcCode;

    /* Indicates the kind of hazardous material content in this line
     * item. */
    private com.fedex.ship.stub.HazardousCommodityOptionType hazardousMaterials;

    /* For printed reference per line item. */
    private java.lang.String purchaseOrderNumber;

    /* Customer-provided description for this commodity or class line. */
    private java.lang.String description;

    /* Weight for this commodity or class line. */
    private com.fedex.ship.stub.Weight weight;

    private com.fedex.ship.stub.Dimensions dimensions;

    /* Volume (cubic measure) for this commodity or class line. */
    private com.fedex.ship.stub.Volume volume;

    public FreightShipmentLineItem() {
    }

    public FreightShipmentLineItem(
           java.lang.String id,
           com.fedex.ship.stub.FreightClassType freightClass,
           java.lang.Boolean classProvidedByCustomer,
           org.apache.axis.types.NonNegativeInteger handlingUnits,
           com.fedex.ship.stub.PhysicalPackagingType packaging,
           org.apache.axis.types.NonNegativeInteger pieces,
           java.lang.String nmfcCode,
           com.fedex.ship.stub.HazardousCommodityOptionType hazardousMaterials,
           java.lang.String purchaseOrderNumber,
           java.lang.String description,
           com.fedex.ship.stub.Weight weight,
           com.fedex.ship.stub.Dimensions dimensions,
           com.fedex.ship.stub.Volume volume) {
           this.id = id;
           this.freightClass = freightClass;
           this.classProvidedByCustomer = classProvidedByCustomer;
           this.handlingUnits = handlingUnits;
           this.packaging = packaging;
           this.pieces = pieces;
           this.nmfcCode = nmfcCode;
           this.hazardousMaterials = hazardousMaterials;
           this.purchaseOrderNumber = purchaseOrderNumber;
           this.description = description;
           this.weight = weight;
           this.dimensions = dimensions;
           this.volume = volume;
    }


    /**
     * Gets the id value for this FreightShipmentLineItem.
     * 
     * @return id   * A unique identifier assigned to this line item.
     */
    public java.lang.String getId() {
        return id;
    }


    /**
     * Sets the id value for this FreightShipmentLineItem.
     * 
     * @param id   * A unique identifier assigned to this line item.
     */
    public void setId(java.lang.String id) {
        this.id = id;
    }


    /**
     * Gets the freightClass value for this FreightShipmentLineItem.
     * 
     * @return freightClass   * Freight class for this line item.
     */
    public com.fedex.ship.stub.FreightClassType getFreightClass() {
        return freightClass;
    }


    /**
     * Sets the freightClass value for this FreightShipmentLineItem.
     * 
     * @param freightClass   * Freight class for this line item.
     */
    public void setFreightClass(com.fedex.ship.stub.FreightClassType freightClass) {
        this.freightClass = freightClass;
    }


    /**
     * Gets the classProvidedByCustomer value for this FreightShipmentLineItem.
     * 
     * @return classProvidedByCustomer   * FEDEX INTERNAL USE ONLY: for FedEx system that estimate freight
     * class from customer-provided dimensions and weight.
     */
    public java.lang.Boolean getClassProvidedByCustomer() {
        return classProvidedByCustomer;
    }


    /**
     * Sets the classProvidedByCustomer value for this FreightShipmentLineItem.
     * 
     * @param classProvidedByCustomer   * FEDEX INTERNAL USE ONLY: for FedEx system that estimate freight
     * class from customer-provided dimensions and weight.
     */
    public void setClassProvidedByCustomer(java.lang.Boolean classProvidedByCustomer) {
        this.classProvidedByCustomer = classProvidedByCustomer;
    }


    /**
     * Gets the handlingUnits value for this FreightShipmentLineItem.
     * 
     * @return handlingUnits   * Number of individual handling units to which this line applies.
     * (NOTE: Total of line-item-level handling units may not balance to
     * shipment-level total handling units.)
     */
    public org.apache.axis.types.NonNegativeInteger getHandlingUnits() {
        return handlingUnits;
    }


    /**
     * Sets the handlingUnits value for this FreightShipmentLineItem.
     * 
     * @param handlingUnits   * Number of individual handling units to which this line applies.
     * (NOTE: Total of line-item-level handling units may not balance to
     * shipment-level total handling units.)
     */
    public void setHandlingUnits(org.apache.axis.types.NonNegativeInteger handlingUnits) {
        this.handlingUnits = handlingUnits;
    }


    /**
     * Gets the packaging value for this FreightShipmentLineItem.
     * 
     * @return packaging   * Specification of handling-unit packaging for this commodity
     * or class line.
     */
    public com.fedex.ship.stub.PhysicalPackagingType getPackaging() {
        return packaging;
    }


    /**
     * Sets the packaging value for this FreightShipmentLineItem.
     * 
     * @param packaging   * Specification of handling-unit packaging for this commodity
     * or class line.
     */
    public void setPackaging(com.fedex.ship.stub.PhysicalPackagingType packaging) {
        this.packaging = packaging;
    }


    /**
     * Gets the pieces value for this FreightShipmentLineItem.
     * 
     * @return pieces   * Number of pieces for this commodity or class line.
     */
    public org.apache.axis.types.NonNegativeInteger getPieces() {
        return pieces;
    }


    /**
     * Sets the pieces value for this FreightShipmentLineItem.
     * 
     * @param pieces   * Number of pieces for this commodity or class line.
     */
    public void setPieces(org.apache.axis.types.NonNegativeInteger pieces) {
        this.pieces = pieces;
    }


    /**
     * Gets the nmfcCode value for this FreightShipmentLineItem.
     * 
     * @return nmfcCode   * NMFC Code for commodity.
     */
    public java.lang.String getNmfcCode() {
        return nmfcCode;
    }


    /**
     * Sets the nmfcCode value for this FreightShipmentLineItem.
     * 
     * @param nmfcCode   * NMFC Code for commodity.
     */
    public void setNmfcCode(java.lang.String nmfcCode) {
        this.nmfcCode = nmfcCode;
    }


    /**
     * Gets the hazardousMaterials value for this FreightShipmentLineItem.
     * 
     * @return hazardousMaterials   * Indicates the kind of hazardous material content in this line
     * item.
     */
    public com.fedex.ship.stub.HazardousCommodityOptionType getHazardousMaterials() {
        return hazardousMaterials;
    }


    /**
     * Sets the hazardousMaterials value for this FreightShipmentLineItem.
     * 
     * @param hazardousMaterials   * Indicates the kind of hazardous material content in this line
     * item.
     */
    public void setHazardousMaterials(com.fedex.ship.stub.HazardousCommodityOptionType hazardousMaterials) {
        this.hazardousMaterials = hazardousMaterials;
    }


    /**
     * Gets the purchaseOrderNumber value for this FreightShipmentLineItem.
     * 
     * @return purchaseOrderNumber   * For printed reference per line item.
     */
    public java.lang.String getPurchaseOrderNumber() {
        return purchaseOrderNumber;
    }


    /**
     * Sets the purchaseOrderNumber value for this FreightShipmentLineItem.
     * 
     * @param purchaseOrderNumber   * For printed reference per line item.
     */
    public void setPurchaseOrderNumber(java.lang.String purchaseOrderNumber) {
        this.purchaseOrderNumber = purchaseOrderNumber;
    }


    /**
     * Gets the description value for this FreightShipmentLineItem.
     * 
     * @return description   * Customer-provided description for this commodity or class line.
     */
    public java.lang.String getDescription() {
        return description;
    }


    /**
     * Sets the description value for this FreightShipmentLineItem.
     * 
     * @param description   * Customer-provided description for this commodity or class line.
     */
    public void setDescription(java.lang.String description) {
        this.description = description;
    }


    /**
     * Gets the weight value for this FreightShipmentLineItem.
     * 
     * @return weight   * Weight for this commodity or class line.
     */
    public com.fedex.ship.stub.Weight getWeight() {
        return weight;
    }


    /**
     * Sets the weight value for this FreightShipmentLineItem.
     * 
     * @param weight   * Weight for this commodity or class line.
     */
    public void setWeight(com.fedex.ship.stub.Weight weight) {
        this.weight = weight;
    }


    /**
     * Gets the dimensions value for this FreightShipmentLineItem.
     * 
     * @return dimensions
     */
    public com.fedex.ship.stub.Dimensions getDimensions() {
        return dimensions;
    }


    /**
     * Sets the dimensions value for this FreightShipmentLineItem.
     * 
     * @param dimensions
     */
    public void setDimensions(com.fedex.ship.stub.Dimensions dimensions) {
        this.dimensions = dimensions;
    }


    /**
     * Gets the volume value for this FreightShipmentLineItem.
     * 
     * @return volume   * Volume (cubic measure) for this commodity or class line.
     */
    public com.fedex.ship.stub.Volume getVolume() {
        return volume;
    }


    /**
     * Sets the volume value for this FreightShipmentLineItem.
     * 
     * @param volume   * Volume (cubic measure) for this commodity or class line.
     */
    public void setVolume(com.fedex.ship.stub.Volume volume) {
        this.volume = volume;
    }

    private java.lang.Object __equalsCalc = null;
    public synchronized boolean equals(java.lang.Object obj) {
        if (!(obj instanceof FreightShipmentLineItem)) return false;
        FreightShipmentLineItem other = (FreightShipmentLineItem) obj;
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
            ((this.freightClass==null && other.getFreightClass()==null) || 
             (this.freightClass!=null &&
              this.freightClass.equals(other.getFreightClass()))) &&
            ((this.classProvidedByCustomer==null && other.getClassProvidedByCustomer()==null) || 
             (this.classProvidedByCustomer!=null &&
              this.classProvidedByCustomer.equals(other.getClassProvidedByCustomer()))) &&
            ((this.handlingUnits==null && other.getHandlingUnits()==null) || 
             (this.handlingUnits!=null &&
              this.handlingUnits.equals(other.getHandlingUnits()))) &&
            ((this.packaging==null && other.getPackaging()==null) || 
             (this.packaging!=null &&
              this.packaging.equals(other.getPackaging()))) &&
            ((this.pieces==null && other.getPieces()==null) || 
             (this.pieces!=null &&
              this.pieces.equals(other.getPieces()))) &&
            ((this.nmfcCode==null && other.getNmfcCode()==null) || 
             (this.nmfcCode!=null &&
              this.nmfcCode.equals(other.getNmfcCode()))) &&
            ((this.hazardousMaterials==null && other.getHazardousMaterials()==null) || 
             (this.hazardousMaterials!=null &&
              this.hazardousMaterials.equals(other.getHazardousMaterials()))) &&
            ((this.purchaseOrderNumber==null && other.getPurchaseOrderNumber()==null) || 
             (this.purchaseOrderNumber!=null &&
              this.purchaseOrderNumber.equals(other.getPurchaseOrderNumber()))) &&
            ((this.description==null && other.getDescription()==null) || 
             (this.description!=null &&
              this.description.equals(other.getDescription()))) &&
            ((this.weight==null && other.getWeight()==null) || 
             (this.weight!=null &&
              this.weight.equals(other.getWeight()))) &&
            ((this.dimensions==null && other.getDimensions()==null) || 
             (this.dimensions!=null &&
              this.dimensions.equals(other.getDimensions()))) &&
            ((this.volume==null && other.getVolume()==null) || 
             (this.volume!=null &&
              this.volume.equals(other.getVolume())));
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
        if (getFreightClass() != null) {
            _hashCode += getFreightClass().hashCode();
        }
        if (getClassProvidedByCustomer() != null) {
            _hashCode += getClassProvidedByCustomer().hashCode();
        }
        if (getHandlingUnits() != null) {
            _hashCode += getHandlingUnits().hashCode();
        }
        if (getPackaging() != null) {
            _hashCode += getPackaging().hashCode();
        }
        if (getPieces() != null) {
            _hashCode += getPieces().hashCode();
        }
        if (getNmfcCode() != null) {
            _hashCode += getNmfcCode().hashCode();
        }
        if (getHazardousMaterials() != null) {
            _hashCode += getHazardousMaterials().hashCode();
        }
        if (getPurchaseOrderNumber() != null) {
            _hashCode += getPurchaseOrderNumber().hashCode();
        }
        if (getDescription() != null) {
            _hashCode += getDescription().hashCode();
        }
        if (getWeight() != null) {
            _hashCode += getWeight().hashCode();
        }
        if (getDimensions() != null) {
            _hashCode += getDimensions().hashCode();
        }
        if (getVolume() != null) {
            _hashCode += getVolume().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(FreightShipmentLineItem.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightShipmentLineItem"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("id");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Id"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("freightClass");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightClass"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "FreightClassType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("classProvidedByCustomer");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ClassProvidedByCustomer"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "boolean"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("handlingUnits");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "HandlingUnits"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "nonNegativeInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("packaging");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Packaging"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PhysicalPackagingType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("pieces");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Pieces"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "nonNegativeInteger"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("nmfcCode");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "NmfcCode"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("hazardousMaterials");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "HazardousMaterials"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "HazardousCommodityOptionType"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("purchaseOrderNumber");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "PurchaseOrderNumber"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("description");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Description"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
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
        elemField.setFieldName("volume");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Volume"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "Volume"));
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
