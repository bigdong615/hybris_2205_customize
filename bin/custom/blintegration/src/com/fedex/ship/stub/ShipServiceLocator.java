/**
 * ShipServiceLocator.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

import de.hybris.platform.util.Config;

import com.bl.integration.constants.BlintegrationConstants;


public class ShipServiceLocator extends org.apache.axis.client.Service implements com.fedex.ship.stub.ShipService {

    public ShipServiceLocator() {
    }


    public ShipServiceLocator(final org.apache.axis.EngineConfiguration config) {
        super(config);
    }

    public ShipServiceLocator(final java.lang.String wsdlLoc, final javax.xml.namespace.QName sName) throws javax.xml.rpc.ServiceException {
        super(wsdlLoc, sName);
    }

    // Use to get a proxy class for ShipServicePort
	 //  private java.lang.String ShipServicePort_address = "https://wsbeta.fedex.com:443/web-services/ship";

	 private java.lang.String ShipServicePort_address = Config.getParameter(BlintegrationConstants.FEDEX_SHIPPER_URL);

    public java.lang.String getShipServicePortAddress() {
        return ShipServicePort_address;
    }

    // The WSDD service name defaults to the port name.
    private java.lang.String ShipServicePortWSDDServiceName = "ShipServicePort";

    public java.lang.String getShipServicePortWSDDServiceName() {
        return ShipServicePortWSDDServiceName;
    }

    public void setShipServicePortWSDDServiceName(final java.lang.String name) {
        ShipServicePortWSDDServiceName = name;
    }

    public com.fedex.ship.stub.ShipPortType getShipServicePort() throws javax.xml.rpc.ServiceException {
       java.net.URL endpoint;
        try {
			  //endpoint = new java.net.URL(ShipServicePort_address);
			  endpoint = new java.net.URL(Config.getParameter(BlintegrationConstants.FEDEX_SHIPPER_URL));
        }
        catch (final java.net.MalformedURLException e) {
            throw new javax.xml.rpc.ServiceException(e);
        }
        return getShipServicePort(endpoint);
    }

    public com.fedex.ship.stub.ShipPortType getShipServicePort(final java.net.URL portAddress) throws javax.xml.rpc.ServiceException {
        try {
            final com.fedex.ship.stub.ShipServiceSoapBindingStub _stub = new com.fedex.ship.stub.ShipServiceSoapBindingStub(portAddress, this);
            _stub.setPortName(getShipServicePortWSDDServiceName());
            return _stub;
        }
        catch (final org.apache.axis.AxisFault e) {
            return null;
        }
    }

    public void setShipServicePortEndpointAddress(final java.lang.String address) {
        ShipServicePort_address = address;
    }

    /**
     * For the given interface, get the stub implementation.
     * If this service has no port for the given interface,
     * then ServiceException is thrown.
     */
    public java.rmi.Remote getPort(final Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
        try {
            if (com.fedex.ship.stub.ShipPortType.class.isAssignableFrom(serviceEndpointInterface)) {
                final com.fedex.ship.stub.ShipServiceSoapBindingStub _stub = new com.fedex.ship.stub.ShipServiceSoapBindingStub(new java.net.URL(ShipServicePort_address), this);
                _stub.setPortName(getShipServicePortWSDDServiceName());
                return _stub;
            }
        }
        catch (final java.lang.Throwable t) {
            throw new javax.xml.rpc.ServiceException(t);
        }
        throw new javax.xml.rpc.ServiceException("There is no stub implementation for the interface:  " + (serviceEndpointInterface == null ? "null" : serviceEndpointInterface.getName()));
    }

    /**
     * For the given interface, get the stub implementation.
     * If this service has no port for the given interface,
     * then ServiceException is thrown.
     */
    public java.rmi.Remote getPort(final javax.xml.namespace.QName portName, final Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
        if (portName == null) {
            return getPort(serviceEndpointInterface);
        }
        final java.lang.String inputPortName = portName.getLocalPart();
        if ("ShipServicePort".equals(inputPortName)) {
            return getShipServicePort();
        }
        else  {
            final java.rmi.Remote _stub = getPort(serviceEndpointInterface);
            ((org.apache.axis.client.Stub) _stub).setPortName(portName);
            return _stub;
        }
    }

    public javax.xml.namespace.QName getServiceName() {
        return new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipService");
    }

    private java.util.HashSet ports = null;

    public java.util.Iterator getPorts() {
        if (ports == null) {
            ports = new java.util.HashSet();
            ports.add(new javax.xml.namespace.QName("http://fedex.com/ws/ship/v28", "ShipServicePort"));
        }
        return ports.iterator();
    }

    /**
    * Set the endpoint address for the specified port name.
    */
    public void setEndpointAddress(final java.lang.String portName, final java.lang.String address) throws javax.xml.rpc.ServiceException {

if ("ShipServicePort".equals(portName)) {
            setShipServicePortEndpointAddress(address);
        }
        else
{ // Unknown Port Name
            throw new javax.xml.rpc.ServiceException(" Cannot set Endpoint Address for Unknown Port" + portName);
        }
    }

    /**
    * Set the endpoint address for the specified port name.
    */
    public void setEndpointAddress(final javax.xml.namespace.QName portName, final java.lang.String address) throws javax.xml.rpc.ServiceException {
        setEndpointAddress(portName.getLocalPart(), address);
    }

}
