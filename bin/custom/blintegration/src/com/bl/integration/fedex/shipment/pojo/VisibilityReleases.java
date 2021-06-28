package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class VisibilityReleases{
    @JsonProperty("releaseTimestamp") 
    public long getReleaseTimestamp() { 
		 return this.releaseTimestamp; } 
    public void setReleaseTimestamp(long releaseTimestamp) { 
		 this.releaseTimestamp = releaseTimestamp; } 
    long releaseTimestamp;
    @JsonProperty("showShipperDisplayName") 
    public boolean getShowShipperDisplayName() { 
		 return this.showShipperDisplayName; } 
    public void setShowShipperDisplayName(boolean showShipperDisplayName) { 
		 this.showShipperDisplayName = showShipperDisplayName; } 
    boolean showShipperDisplayName;
    @JsonProperty("showShipperDisplayImage") 
    public boolean getShowShipperDisplayImage() { 
		 return this.showShipperDisplayImage; } 
    public void setShowShipperDisplayImage(boolean showShipperDisplayImage) { 
		 this.showShipperDisplayImage = showShipperDisplayImage; } 
    boolean showShipperDisplayImage;
    @JsonProperty("showShipmentDisplayName") 
    public boolean getShowShipmentDisplayName() { 
		 return this.showShipmentDisplayName; } 
    public void setShowShipmentDisplayName(boolean showShipmentDisplayName) { 
		 this.showShipmentDisplayName = showShipmentDisplayName; } 
    boolean showShipmentDisplayName;
    @JsonProperty("showShipmentDisplayImage") 
    public boolean getShowShipmentDisplayImage() { 
		 return this.showShipmentDisplayImage; } 
    public void setShowShipmentDisplayImage(boolean showShipmentDisplayImage) { 
		 this.showShipmentDisplayImage = showShipmentDisplayImage; } 
    boolean showShipmentDisplayImage;
    @JsonProperty("showPickupLocation") 
    public boolean getShowPickupLocation() { 
		 return this.showPickupLocation; } 
    public void setShowPickupLocation(boolean showPickupLocation) { 
		 this.showPickupLocation = showPickupLocation; } 
    boolean showPickupLocation;
}
