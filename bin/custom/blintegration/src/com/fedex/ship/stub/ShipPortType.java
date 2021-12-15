/**
 * ShipPortType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.fedex.ship.stub;

public interface ShipPortType extends java.rmi.Remote {
    public com.fedex.ship.stub.ProcessTagReply processTag(com.fedex.ship.stub.ProcessTagRequest processTagRequest) throws java.rmi.RemoteException;
    public com.fedex.ship.stub.ProcessShipmentReply processShipment(com.fedex.ship.stub.ProcessShipmentRequest processShipmentRequest) throws java.rmi.RemoteException;
    public com.fedex.ship.stub.ShipmentReply deleteTag(com.fedex.ship.stub.DeleteTagRequest deleteTagRequest) throws java.rmi.RemoteException;
    public com.fedex.ship.stub.ShipmentReply deleteShipment(com.fedex.ship.stub.DeleteShipmentRequest deleteShipmentRequest) throws java.rmi.RemoteException;
    public com.fedex.ship.stub.ShipmentReply validateShipment(com.fedex.ship.stub.ValidateShipmentRequest validateShipmentRequest) throws java.rmi.RemoteException;
}
