package com.bl.integration.fedex.shipment.pojo;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class FedExShipmentRequest{
    @JsonProperty("service") 
    public Service getService() { 
		 return this.service; } 
    public void setService(Service service) { 
		 this.service = service; } 
    Service service;
    @JsonProperty("category") 
    public String getCategory() { 
		 return this.category; } 
    public void setCategory(String category) { 
		 this.category = category; } 
    String category;
    @JsonProperty("pickupDetail") 
    public PickupDetail getPickupDetail() { 
		 return this.pickupDetail; } 
    public void setPickupDetail(PickupDetail pickupDetail) { 
		 this.pickupDetail = pickupDetail; } 
    PickupDetail pickupDetail;
    @JsonProperty("deliveryDetail") 
    public DeliveryDetail getDeliveryDetail() { 
		 return this.deliveryDetail; } 
    public void setDeliveryDetail(DeliveryDetail deliveryDetail) { 
		 this.deliveryDetail = deliveryDetail; } 
    DeliveryDetail deliveryDetail;
    @JsonProperty("shipper") 
    public Shipper getShipper() { 
		 return this.shipper; } 
    public void setShipper(Shipper shipper) { 
		 this.shipper = shipper; } 
    Shipper shipper;
    @JsonProperty("recipient") 
    public Recipient getRecipient() { 
		 return this.recipient; } 
    public void setRecipient(Recipient recipient) { 
		 this.recipient = recipient; } 
    Recipient recipient;
    @JsonProperty("totalDeclaredValue") 
    public TotalDeclaredValue getTotalDeclaredValue() { 
		 return this.totalDeclaredValue; } 
    public void setTotalDeclaredValue(TotalDeclaredValue totalDeclaredValue) { 
		 this.totalDeclaredValue = totalDeclaredValue; } 
    TotalDeclaredValue totalDeclaredValue;
    @JsonProperty("packages") 
    public List<Package> getPackages() { 
		 return this.packages; } 
    public void setPackages(List<Package> packages) { 
		 this.packages = packages; } 
    List<Package> packages;
    @JsonProperty("contentDescription") 
    public String getContentDescription() { 
		 return this.contentDescription; } 
    public void setContentDescription(String contentDescription) { 
		 this.contentDescription = contentDescription; } 
    String contentDescription;
    @JsonProperty("displayImage") 
    public DisplayImage getDisplayImage() { 
		 return this.displayImage; } 
    public void setDisplayImage(DisplayImage displayImage) { 
		 this.displayImage = displayImage; } 
    DisplayImage displayImage;
    @JsonProperty("externalReferences") 
    public ExternalReferences getExternalReferences() { 
		 return this.externalReferences; } 
    public void setExternalReferences(ExternalReferences externalReferences) { 
		 this.externalReferences = externalReferences; } 
    ExternalReferences externalReferences;
    @JsonProperty("notifications") 
    public Notifications getNotifications() { 
		 return this.notifications; } 
    public void setNotifications(Notifications notifications) { 
		 this.notifications = notifications; } 
    Notifications notifications;
    @JsonProperty("visibilityReleases") 
    public VisibilityReleases getVisibilityReleases() { 
		 return this.visibilityReleases; } 
    public void setVisibilityReleases(VisibilityReleases visibilityReleases) { 
		 this.visibilityReleases = visibilityReleases; } 
    VisibilityReleases visibilityReleases;
    @JsonProperty("trackingNumber") 
    public String getTrackingNumber() { 
		 return this.trackingNumber; } 
    public void setTrackingNumber(String trackingNumber) { 
		 this.trackingNumber = trackingNumber; } 
    String trackingNumber;
    @JsonProperty("shipmentCreateDate") 
    public Date getShipmentCreateDate() { 
		 return this.shipmentCreateDate; } 
    public void setShipmentCreateDate(Date shipmentCreateDate) { 
		 this.shipmentCreateDate = shipmentCreateDate; } 
    Date shipmentCreateDate;
    @JsonProperty("labelURL") 
    public String getLabelURL() { 
		 return this.labelURL; } 
    public void setLabelURL(String labelURL) { 
		 this.labelURL = labelURL; } 
    String labelURL;
    @JsonProperty("pricingZone") 
    public String getPricingZone() { 
		 return this.pricingZone; } 
    public void setPricingZone(String pricingZone) { 
		 this.pricingZone = pricingZone; } 
    String pricingZone;
    @JsonProperty("chargeAmount") 
    public ChargeAmount getChargeAmount() { 
		 return this.chargeAmount; } 
    public void setChargeAmount(ChargeAmount chargeAmount) { 
		 this.chargeAmount = chargeAmount; } 
    ChargeAmount chargeAmount;
    @JsonProperty("chargeDetails") 
    public List<ChargeDetail> getChargeDetails() { 
		 return this.chargeDetails; } 
    public void setChargeDetails(List<ChargeDetail> chargeDetails) { 
		 this.chargeDetails = chargeDetails; } 
    List<ChargeDetail> chargeDetails;
    @JsonProperty("totalWeight") 
    public TotalWeight getTotalWeight() { 
		 return this.totalWeight; } 
    public void setTotalWeight(TotalWeight totalWeight) { 
		 this.totalWeight = totalWeight; } 
    TotalWeight totalWeight;
    @JsonProperty("totalPackageCount") 
    public int getTotalPackageCount() { 
		 return this.totalPackageCount; } 
    public void setTotalPackageCount(int totalPackageCount) { 
		 this.totalPackageCount = totalPackageCount; } 
    int totalPackageCount;
    @JsonProperty("estPickupTime") 
    public Date getEstPickupTime() { 
		 return this.estPickupTime; } 
    public void setEstPickupTime(Date estPickupTime) { 
		 this.estPickupTime = estPickupTime; } 
    Date estPickupTime;
    @JsonProperty("estDeliveryTime") 
    public Date getEstDeliveryTime() { 
		 return this.estDeliveryTime; } 
    public void setEstDeliveryTime(Date estDeliveryTime) { 
		 this.estDeliveryTime = estDeliveryTime; } 
    Date estDeliveryTime;
    @JsonProperty("accountNumber") 
    public String getAccountNumber() { 
		 return this.accountNumber; } 
    public void setAccountNumber(String accountNumber) { 
		 this.accountNumber = accountNumber; } 
    String accountNumber;
    @JsonProperty("vendor") 
    public Vendor getVendor() { 
		 return this.vendor; } 
    public void setVendor(Vendor vendor) { 
		 this.vendor = vendor; } 
    Vendor vendor;
}
