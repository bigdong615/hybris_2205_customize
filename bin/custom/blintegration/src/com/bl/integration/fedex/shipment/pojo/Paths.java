package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Paths{
    @JsonProperty("/v1/shipment") 
    public V1Shipment getV1Shipment() { 
		 return this.v1Shipment; } 
    public void setV1Shipment(V1Shipment v1Shipment) { 
		 this.v1Shipment = v1Shipment; } 
    V1Shipment v1Shipment;
    @JsonProperty("/v1/route/{trackingNumber}/cancellation") 
    public V1RouteTrackingNumberCancellation getV1RouteTrackingNumberCancellation() { 
		 return this.v1RouteTrackingNumberCancellation; } 
    public void setV1RouteTrackingNumberCancellation(V1RouteTrackingNumberCancellation v1RouteTrackingNumberCancellation) { 
		 this.v1RouteTrackingNumberCancellation = v1RouteTrackingNumberCancellation; } 
    V1RouteTrackingNumberCancellation v1RouteTrackingNumberCancellation;
    @JsonProperty("/v1/shipment/{trackingNumber}/cancellation") 
    public V1ShipmentTrackingNumberCancellation getV1ShipmentTrackingNumberCancellation() { 
		 return this.v1ShipmentTrackingNumberCancellation; } 
    public void setV1ShipmentTrackingNumberCancellation(V1ShipmentTrackingNumberCancellation v1ShipmentTrackingNumberCancellation) { 
		 this.v1ShipmentTrackingNumberCancellation = v1ShipmentTrackingNumberCancellation; } 
    V1ShipmentTrackingNumberCancellation v1ShipmentTrackingNumberCancellation;
}
