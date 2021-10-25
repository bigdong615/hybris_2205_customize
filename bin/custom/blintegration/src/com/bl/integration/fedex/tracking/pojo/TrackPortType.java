/**
 * TrackPortType.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.4 Apr 22, 2006 (06:55:48 PDT) WSDL2Java emitter.
 */

package com.bl.integration.fedex.tracking.pojo;

public interface TrackPortType extends java.rmi.Remote {
    public  TrackReply track( TrackRequest trackRequest) throws java.rmi.RemoteException;
    public  GetTrackingDocumentsReply getTrackingDocuments(
         GetTrackingDocumentsRequest getTrackingDocumentsRequest) throws java.rmi.RemoteException;
    public  SendNotificationsReply sendNotifications(
         SendNotificationsRequest sendNotificationsRequest) throws java.rmi.RemoteException;
}
