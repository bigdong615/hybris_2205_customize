/**
 * Address.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;


/**
 * Descriptive data for a physical location. May be used as an actual
 * physical address (place to which one could go), or as a container
 * of "address parts" which should be handled as a unit (such as a city-state-ZIP
 * combination within the US).
 */
public class Address  implements java.io.Serializable {
    /* Combination of number, street name, etc. At least one line
     * is required for a valid physical address; empty lines should not be
     * included. */
    private String[] streetLines;

    /* Name of city, town, etc. */
    private String city;

    /* Identifying abbreviation for US state, Canada province, etc.
     * Format and presence of this field will vary, depending on country. */
    private String stateOrProvinceCode;

    /* Identification of a region (usually small) for mail/package
     * delivery. Format and presence of this field will vary, depending on
     * country. */
    private String postalCode;

    /* Relevant only to addresses in Puerto Rico. */
    private String urbanizationCode;

    /* The two-letter code used to identify a country. */
    private String countryCode;

    /* The fully spelt out name of a country. */
    private String countryName;

    /* Indicates whether this address residential (as opposed to commercial). */
    private Boolean residential;

    /* The geographic coordinates cooresponding to this address. */
    private String geographicCoordinates;

    public Address() {
    }

    public Address(
           String[] streetLines,
           String city,
           String stateOrProvinceCode,
           String postalCode,
           String urbanizationCode,
           String countryCode,
           String countryName,
           Boolean residential,
           String geographicCoordinates) {
           this.streetLines = streetLines;
           this.city = city;
           this.stateOrProvinceCode = stateOrProvinceCode;
           this.postalCode = postalCode;
           this.urbanizationCode = urbanizationCode;
           this.countryCode = countryCode;
           this.countryName = countryName;
           this.residential = residential;
           this.geographicCoordinates = geographicCoordinates;
    }


    /**
     * Gets the streetLines value for this Address.
     *
     * @return streetLines   * Combination of number, street name, etc. At least one line
     * is required for a valid physical address; empty lines should not be
     * included.
     */
    public String[] getStreetLines() {
        return streetLines;
    }


    /**
     * Sets the streetLines value for this Address.
     *
     * @param streetLines   * Combination of number, street name, etc. At least one line
     * is required for a valid physical address; empty lines should not be
     * included.
     */
    public void setStreetLines(String[] streetLines) {
        this.streetLines = streetLines;
    }

    public String getStreetLines(int i) {
        return this.streetLines[i];
    }

    public void setStreetLines(int i, String _value) {
        this.streetLines[i] = _value;
    }


    /**
     * Gets the city value for this Address.
     *
     * @return city   * Name of city, town, etc.
     */
    public String getCity() {
        return city;
    }


    /**
     * Sets the city value for this Address.
     *
     * @param city   * Name of city, town, etc.
     */
    public void setCity(String city) {
        this.city = city;
    }


    /**
     * Gets the stateOrProvinceCode value for this Address.
     *
     * @return stateOrProvinceCode   * Identifying abbreviation for US state, Canada province, etc.
     * Format and presence of this field will vary, depending on country.
     */
    public String getStateOrProvinceCode() {
        return stateOrProvinceCode;
    }


    /**
     * Sets the stateOrProvinceCode value for this Address.
     *
     * @param stateOrProvinceCode   * Identifying abbreviation for US state, Canada province, etc.
     * Format and presence of this field will vary, depending on country.
     */
    public void setStateOrProvinceCode(String stateOrProvinceCode) {
        this.stateOrProvinceCode = stateOrProvinceCode;
    }


    /**
     * Gets the postalCode value for this Address.
     *
     * @return postalCode   * Identification of a region (usually small) for mail/package
     * delivery. Format and presence of this field will vary, depending on
     * country.
     */
    public String getPostalCode() {
        return postalCode;
    }


    /**
     * Sets the postalCode value for this Address.
     *
     * @param postalCode   * Identification of a region (usually small) for mail/package
     * delivery. Format and presence of this field will vary, depending on
     * country.
     */
    public void setPostalCode(String postalCode) {
        this.postalCode = postalCode;
    }


    /**
     * Gets the urbanizationCode value for this Address.
     *
     * @return urbanizationCode   * Relevant only to addresses in Puerto Rico.
     */
    public String getUrbanizationCode() {
        return urbanizationCode;
    }


    /**
     * Sets the urbanizationCode value for this Address.
     *
     * @param urbanizationCode   * Relevant only to addresses in Puerto Rico.
     */
    public void setUrbanizationCode(String urbanizationCode) {
        this.urbanizationCode = urbanizationCode;
    }


    /**
     * Gets the countryCode value for this Address.
     *
     * @return countryCode   * The two-letter code used to identify a country.
     */
    public String getCountryCode() {
        return countryCode;
    }


    /**
     * Sets the countryCode value for this Address.
     *
     * @param countryCode   * The two-letter code used to identify a country.
     */
    public void setCountryCode(String countryCode) {
        this.countryCode = countryCode;
    }


    /**
     * Gets the countryName value for this Address.
     *
     * @return countryName   * The fully spelt out name of a country.
     */
    public String getCountryName() {
        return countryName;
    }


    /**
     * Sets the countryName value for this Address.
     *
     * @param countryName   * The fully spelt out name of a country.
     */
    public void setCountryName(String countryName) {
        this.countryName = countryName;
    }


    /**
     * Gets the residential value for this Address.
     *
     * @return residential   * Indicates whether this address residential (as opposed to commercial).
     */
    public Boolean getResidential() {
        return residential;
    }


    /**
     * Sets the residential value for this Address.
     *
     * @param residential   * Indicates whether this address residential (as opposed to commercial).
     */
    public void setResidential(Boolean residential) {
        this.residential = residential;
    }


    /**
     * Gets the geographicCoordinates value for this Address.
     *
     * @return geographicCoordinates   * The geographic coordinates cooresponding to this address.
     */
    public String getGeographicCoordinates() {
        return geographicCoordinates;
    }


    /**
     * Sets the geographicCoordinates value for this Address.
     *
     * @param geographicCoordinates   * The geographic coordinates cooresponding to this address.
     */
    public void setGeographicCoordinates(String geographicCoordinates) {
        this.geographicCoordinates = geographicCoordinates;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof Address)) return false;
        Address other = (Address) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.streetLines==null && other.getStreetLines()==null) ||
             (this.streetLines!=null &&
              java.util.Arrays.equals(this.streetLines, other.getStreetLines()))) &&
            ((this.city==null && other.getCity()==null) ||
             (this.city!=null &&
              this.city.equals(other.getCity()))) &&
            ((this.stateOrProvinceCode==null && other.getStateOrProvinceCode()==null) ||
             (this.stateOrProvinceCode!=null &&
              this.stateOrProvinceCode.equals(other.getStateOrProvinceCode()))) &&
            ((this.postalCode==null && other.getPostalCode()==null) ||
             (this.postalCode!=null &&
              this.postalCode.equals(other.getPostalCode()))) &&
            ((this.urbanizationCode==null && other.getUrbanizationCode()==null) ||
             (this.urbanizationCode!=null &&
              this.urbanizationCode.equals(other.getUrbanizationCode()))) &&
            ((this.countryCode==null && other.getCountryCode()==null) ||
             (this.countryCode!=null &&
              this.countryCode.equals(other.getCountryCode()))) &&
            ((this.countryName==null && other.getCountryName()==null) ||
             (this.countryName!=null &&
              this.countryName.equals(other.getCountryName()))) &&
            ((this.residential==null && other.getResidential()==null) ||
             (this.residential!=null &&
              this.residential.equals(other.getResidential()))) &&
            ((this.geographicCoordinates==null && other.getGeographicCoordinates()==null) ||
             (this.geographicCoordinates!=null &&
              this.geographicCoordinates.equals(other.getGeographicCoordinates())));
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
        if (getStreetLines() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getStreetLines());
                 i++) {
                Object obj = java.lang.reflect.Array.get(getStreetLines(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getCity() != null) {
            _hashCode += getCity().hashCode();
        }
        if (getStateOrProvinceCode() != null) {
            _hashCode += getStateOrProvinceCode().hashCode();
        }
        if (getPostalCode() != null) {
            _hashCode += getPostalCode().hashCode();
        }
        if (getUrbanizationCode() != null) {
            _hashCode += getUrbanizationCode().hashCode();
        }
        if (getCountryCode() != null) {
            _hashCode += getCountryCode().hashCode();
        }
        if (getCountryName() != null) {
            _hashCode += getCountryName().hashCode();
        }
        if (getResidential() != null) {
            _hashCode += getResidential().hashCode();
        }
        if (getGeographicCoordinates() != null) {
            _hashCode += getGeographicCoordinates().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(Address.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Address"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("streetLines");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "StreetLines"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("city");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "City"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("stateOrProvinceCode");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "StateOrProvinceCode"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("postalCode");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "PostalCode"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("urbanizationCode");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "UrbanizationCode"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("countryCode");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "CountryCode"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("countryName");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "CountryName"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("residential");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Residential"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "boolean"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("geographicCoordinates");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "GeographicCoordinates"));
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
