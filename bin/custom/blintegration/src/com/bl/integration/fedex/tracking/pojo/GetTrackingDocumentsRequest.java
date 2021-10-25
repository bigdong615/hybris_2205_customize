/**
 * GetTrackingDocumentsRequest.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public class GetTrackingDocumentsRequest  implements java.io.Serializable {
    /* Descriptive data to be used in authentication of the sender's
     * identity (and right to use FedEx web services). */
    private  WebAuthenticationDetail webAuthenticationDetail;

    private  ClientDetail clientDetail;

    private  TransactionDetail transactionDetail;

    private  VersionId version;

    private  TrackSelectionDetail[] selectionDetails;

    private  TrackingDocumentSpecification trackingDocumentSpecification;

    public GetTrackingDocumentsRequest() {
    }

    public GetTrackingDocumentsRequest(
            WebAuthenticationDetail webAuthenticationDetail,
            ClientDetail clientDetail,
            TransactionDetail transactionDetail,
            VersionId version,
            TrackSelectionDetail[] selectionDetails,
            TrackingDocumentSpecification trackingDocumentSpecification) {
           this.webAuthenticationDetail = webAuthenticationDetail;
           this.clientDetail = clientDetail;
           this.transactionDetail = transactionDetail;
           this.version = version;
           this.selectionDetails = selectionDetails;
           this.trackingDocumentSpecification = trackingDocumentSpecification;
    }


    /**
     * Gets the webAuthenticationDetail value for this GetTrackingDocumentsRequest.
     * 
     * @return webAuthenticationDetail   * Descriptive data to be used in authentication of the sender's
     * identity (and right to use FedEx web services).
     */
    public  WebAuthenticationDetail getWebAuthenticationDetail() {
        return webAuthenticationDetail;
    }


    /**
     * Sets the webAuthenticationDetail value for this GetTrackingDocumentsRequest.
     * 
     * @param webAuthenticationDetail   * Descriptive data to be used in authentication of the sender's
     * identity (and right to use FedEx web services).
     */
    public void setWebAuthenticationDetail( WebAuthenticationDetail webAuthenticationDetail) {
        this.webAuthenticationDetail = webAuthenticationDetail;
    }


    /**
     * Gets the clientDetail value for this GetTrackingDocumentsRequest.
     * 
     * @return clientDetail
     */
    public  ClientDetail getClientDetail() {
        return clientDetail;
    }


    /**
     * Sets the clientDetail value for this GetTrackingDocumentsRequest.
     * 
     * @param clientDetail
     */
    public void setClientDetail( ClientDetail clientDetail) {
        this.clientDetail = clientDetail;
    }


    /**
     * Gets the transactionDetail value for this GetTrackingDocumentsRequest.
     * 
     * @return transactionDetail
     */
    public  TransactionDetail getTransactionDetail() {
        return transactionDetail;
    }


    /**
     * Sets the transactionDetail value for this GetTrackingDocumentsRequest.
     * 
     * @param transactionDetail
     */
    public void setTransactionDetail( TransactionDetail transactionDetail) {
        this.transactionDetail = transactionDetail;
    }


    /**
     * Gets the version value for this GetTrackingDocumentsRequest.
     * 
     * @return version
     */
    public  VersionId getVersion() {
        return version;
    }


    /**
     * Sets the version value for this GetTrackingDocumentsRequest.
     * 
     * @param version
     */
    public void setVersion( VersionId version) {
        this.version = version;
    }


    /**
     * Gets the selectionDetails value for this GetTrackingDocumentsRequest.
     * 
     * @return selectionDetails
     */
    public  TrackSelectionDetail[] getSelectionDetails() {
        return selectionDetails;
    }


    /**
     * Sets the selectionDetails value for this GetTrackingDocumentsRequest.
     * 
     * @param selectionDetails
     */
    public void setSelectionDetails( TrackSelectionDetail[] selectionDetails) {
        this.selectionDetails = selectionDetails;
    }

    public  TrackSelectionDetail getSelectionDetails(int i) {
        return this.selectionDetails[i];
    }

    public void setSelectionDetails(int i,  TrackSelectionDetail _value) {
        this.selectionDetails[i] = _value;
    }


    /**
     * Gets the trackingDocumentSpecification value for this GetTrackingDocumentsRequest.
     * 
     * @return trackingDocumentSpecification
     */
    public  TrackingDocumentSpecification getTrackingDocumentSpecification() {
        return trackingDocumentSpecification;
    }


    /**
     * Sets the trackingDocumentSpecification value for this GetTrackingDocumentsRequest.
     * 
     * @param trackingDocumentSpecification
     */
    public void setTrackingDocumentSpecification( TrackingDocumentSpecification trackingDocumentSpecification) {
        this.trackingDocumentSpecification = trackingDocumentSpecification;
    }

    private Object __equalsCalc = null;
    public synchronized boolean equals(Object obj) {
        if (!(obj instanceof GetTrackingDocumentsRequest)) return false;
        GetTrackingDocumentsRequest other = (GetTrackingDocumentsRequest) obj;
        if (obj == null) return false;
        if (this == obj) return true;
        if (__equalsCalc != null) {
            return (__equalsCalc == obj);
        }
        __equalsCalc = obj;
        boolean _equals;
        _equals = true &&
            ((this.webAuthenticationDetail==null && other.getWebAuthenticationDetail()==null) ||
             (this.webAuthenticationDetail!=null &&
              this.webAuthenticationDetail.equals(other.getWebAuthenticationDetail()))) &&
            ((this.clientDetail==null && other.getClientDetail()==null) ||
             (this.clientDetail!=null &&
              this.clientDetail.equals(other.getClientDetail()))) &&
            ((this.transactionDetail==null && other.getTransactionDetail()==null) ||
             (this.transactionDetail!=null &&
              this.transactionDetail.equals(other.getTransactionDetail()))) &&
            ((this.version==null && other.getVersion()==null) ||
             (this.version!=null &&
              this.version.equals(other.getVersion()))) &&
            ((this.selectionDetails==null && other.getSelectionDetails()==null) ||
             (this.selectionDetails!=null &&
              java.util.Arrays.equals(this.selectionDetails, other.getSelectionDetails()))) &&
            ((this.trackingDocumentSpecification==null && other.getTrackingDocumentSpecification()==null) ||
             (this.trackingDocumentSpecification!=null &&
              this.trackingDocumentSpecification.equals(other.getTrackingDocumentSpecification())));
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
        if (getWebAuthenticationDetail() != null) {
            _hashCode += getWebAuthenticationDetail().hashCode();
        }
        if (getClientDetail() != null) {
            _hashCode += getClientDetail().hashCode();
        }
        if (getTransactionDetail() != null) {
            _hashCode += getTransactionDetail().hashCode();
        }
        if (getVersion() != null) {
            _hashCode += getVersion().hashCode();
        }
        if (getSelectionDetails() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getSelectionDetails());
                 i++) {
                Object obj = java.lang.reflect.Array.get(getSelectionDetails(), i);
                if (obj != null &&
                    !obj.getClass().isArray()) {
                    _hashCode += obj.hashCode();
                }
            }
        }
        if (getTrackingDocumentSpecification() != null) {
            _hashCode += getTrackingDocumentSpecification().hashCode();
        }
        __hashCodeCalc = false;
        return _hashCode;
    }

    // Type metadata
    private static org.apache.axis.description.TypeDesc typeDesc =
        new org.apache.axis.description.TypeDesc(GetTrackingDocumentsRequest.class, true);

    static {
        typeDesc.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "GetTrackingDocumentsRequest"));
        org.apache.axis.description.ElementDesc elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("webAuthenticationDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "WebAuthenticationDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "WebAuthenticationDetail"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("clientDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ClientDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "ClientDetail"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("transactionDetail");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TransactionDetail"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TransactionDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("version");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "Version"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "VersionId"));
        elemField.setNillable(false);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("selectionDetails");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "SelectionDetails"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackSelectionDetail"));
        elemField.setMinOccurs(0);
        elemField.setNillable(false);
        elemField.setMaxOccursUnbounded(true);
        typeDesc.addFieldDesc(elemField);
        elemField = new org.apache.axis.description.ElementDesc();
        elemField.setFieldName("trackingDocumentSpecification");
        elemField.setXmlName(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDocumentSpecification"));
        elemField.setXmlType(new javax.xml.namespace.QName("http://fedex.com/ws/track/v19", "TrackingDocumentSpecification"));
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
