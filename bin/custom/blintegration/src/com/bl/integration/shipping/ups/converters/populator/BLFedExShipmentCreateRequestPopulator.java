/**
 *
 */
package com.bl.integration.shipping.ups.converters.populator;

import de.hybris.platform.commercefacades.user.data.AddressData;

import java.util.ArrayList;
import java.util.List;

import com.bl.facades.shipment.data.FedExDeliveryDetailData;
import com.bl.facades.shipment.data.FedExEmailData;
import com.bl.facades.shipment.data.FedExExternalReferencesData;
import com.bl.facades.shipment.data.FedExItemData;
import com.bl.facades.shipment.data.FedExNotificationsData;
import com.bl.facades.shipment.data.FedExPackageData;
import com.bl.facades.shipment.data.FedExPickupDetailData;
import com.bl.facades.shipment.data.FedExRecipientData;
import com.bl.facades.shipment.data.FedExSMSlData;
import com.bl.facades.shipment.data.FedExServiceData;
import com.bl.facades.shipment.data.FedExShipperData;
import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.FedExTotalDeclaredValueData;
import com.bl.facades.shipment.data.FedExVisibilityReleasesData;
import com.bl.integration.fedex.shipment.pojo.Address;
import com.bl.integration.fedex.shipment.pojo.Contact;
import com.bl.integration.fedex.shipment.pojo.DeliveryDetail;
import com.bl.integration.fedex.shipment.pojo.Dimensions;
import com.bl.integration.fedex.shipment.pojo.DisplayImage;
import com.bl.integration.fedex.shipment.pojo.Email;
import com.bl.integration.fedex.shipment.pojo.ExternalReferences;
import com.bl.integration.fedex.shipment.pojo.FedExShipmentRequest;
import com.bl.integration.fedex.shipment.pojo.Item;
import com.bl.integration.fedex.shipment.pojo.Location;
import com.bl.integration.fedex.shipment.pojo.Notifications;
import com.bl.integration.fedex.shipment.pojo.Package;
import com.bl.integration.fedex.shipment.pojo.PickupDetail;
import com.bl.integration.fedex.shipment.pojo.Recipient;
import com.bl.integration.fedex.shipment.pojo.Restrictions;
import com.bl.integration.fedex.shipment.pojo.Service;
import com.bl.integration.fedex.shipment.pojo.Shipper;
import com.bl.integration.fedex.shipment.pojo.Sm;
import com.bl.integration.fedex.shipment.pojo.SupportContact;
import com.bl.integration.fedex.shipment.pojo.TotalDeclaredValue;
import com.bl.integration.fedex.shipment.pojo.VisibilityReleases;
import com.bl.integration.fedex.shipment.pojo.Weight;


/**
 * @author Aditi Sharma
 *
 */
public class BLFedExShipmentCreateRequestPopulator
{
	public FedExShipmentRequest convertToFedExShipmentRequest(final FedExShippingRequestData fedExShipmentReqData)
	{
		final FedExShipmentRequest fedExShipmentRequest = new FedExShipmentRequest();

		/** Creating Service Data **/
		final Service serviceType = new Service();
		final FedExServiceData serviceData = fedExShipmentReqData.getService();
		final Restrictions restrictionType = new Restrictions();
		restrictionType.setNoHAL(serviceData.getRestrictions().getNoHAL());
		restrictionType.setNoRecipientRedirect(serviceData.getRestrictions().getNoRecipientRedirect());
		restrictionType.setNoRemoteSignature(serviceData.getRestrictions().getNoRemoteSignature());

		serviceType.setServiceType(fedExShipmentReqData.getService().getServiceType());
		serviceType.setSignatureService(serviceData.getSignatureService());
		serviceType.setSpecialServices(serviceData.getSpecialServices());
		serviceType.setRestrictions(restrictionType);

		fedExShipmentRequest.setService(serviceType);

		/** Creating Category Data **/
		fedExShipmentRequest.setCategory(fedExShipmentReqData.getCategory());

		/** Creating PickupDetail Data **/
		final FedExPickupDetailData pickupDetailsData = fedExShipmentReqData.getPickupDetail();
		final PickupDetail pickupDetailType = new PickupDetail();

		final Location locationType = new Location();
		final AddressData pickupAddressData = pickupDetailsData.getLocation().getAddress();

		final List<String> streetNameList = new ArrayList<String>();
		streetNameList.add(pickupAddressData.getFirstName());
		streetNameList.add(pickupAddressData.getLastName());

		final Address pickupAddressType = new Address();
		pickupAddressType.setCity(pickupAddressData.getTown());
		pickupAddressType.setCountryCode(pickupAddressData.getCountry().getIsocode());
		pickupAddressType.setCounty(pickupAddressData.getCountry().getName());
		pickupAddressType.setPostalCode(pickupAddressData.getPostalCode());
		pickupAddressType.setStateOrProvinceCode(pickupAddressData.getRegion().getIsocode());
		pickupAddressType.setStreetLines(streetNameList);

		locationType.setAddress(pickupAddressType);
		locationType.setResidential(pickupDetailsData.getLocation().getResidential());
		locationType.setHoursOfOperationStart(pickupDetailsData.getLocation().getHoursOfOperationStart());
		locationType.setHoursOfOperationEnd(pickupDetailsData.getLocation().getHoursOfOperationEnd());

		pickupDetailType.setLocation(locationType);
		pickupDetailType.setInstructions(pickupDetailsData.getInstructions());
		pickupDetailType.setLocalTimeZone(pickupDetailsData.getLocalTimeZone());
		pickupDetailType.setReadyTime(pickupDetailsData.getReadyTime());

		fedExShipmentRequest.setPickupDetail(pickupDetailType);

		/** Creating Delivery Details Data **/

		final FedExDeliveryDetailData deliveryDetailData = fedExShipmentReqData.getDeliveryDetail();

		final DeliveryDetail deliveryDetailType = new DeliveryDetail();
		final AddressData delivertAddressData = deliveryDetailData.getLocation().getAddress();
		final Location deliveryLocation = new Location();
		final Address deliveryAddress = new Address();
		final List<String> deliveryStreetLine = new ArrayList<>();
		deliveryStreetLine.add(delivertAddressData.getLine1());
		deliveryStreetLine.add(delivertAddressData.getLine2());

		deliveryAddress.setStreetLines(deliveryStreetLine);
		deliveryAddress.setCity(delivertAddressData.getTown());
		deliveryAddress.setStateOrProvinceCode(delivertAddressData.getRegion().getIsocode());
		deliveryAddress.setCountryCode(delivertAddressData.getCountry().getIsocode());
		deliveryAddress.setCounty(delivertAddressData.getCountry().getName());
		deliveryAddress.setPostalCode(delivertAddressData.getPostalCode());

		deliveryLocation.setAddress(deliveryAddress);
		deliveryLocation.setResidential(true);
		deliveryLocation.setHoursOfOperationStart(1459897586589l);
		deliveryLocation.setHoursOfOperationEnd(1459897586589l);

		deliveryDetailType.setLocation(deliveryLocation);
		deliveryDetailType.setInstructions("Fragile");

		fedExShipmentRequest.setDeliveryDetail(deliveryDetailType);

		/** Creating Shipper Data **/
		final FedExShipperData shipperData = fedExShipmentReqData.getShipper();

		final Shipper shipperType = new Shipper();
		shipperType.setAccountNumber(shipperData.getAccountNumber());

		final DisplayImage displayImage = new DisplayImage();
		displayImage.setLarge(shipperData.getDisplayImage().getLarge());
		displayImage.setMedium(shipperData.getDisplayImage().getMedium());
		displayImage.setSmall(shipperData.getDisplayImage().getSmall());

		shipperType.setDisplayImage(displayImage);
		shipperType.setDisplayName(shipperData.getDisplayName());

		final Contact shipperContact = new Contact();
		shipperContact.setPersonName(shipperData.getContact().getFirstName());
		shipperContact.setCompanyName(shipperData.getContact().getCompanyName());
		shipperContact.setPhoneNumber(shipperData.getContact().getPhone());
		//contact.setPhoneExtension("4571");
		shipperContact.setEmailAddress(shipperData.getContact().getEmail());

		shipperType.setContact(shipperContact);

		final SupportContact shipperSupportContact = new SupportContact();
		shipperSupportContact.setPhoneNumber(shipperData.getSupportContact().getPhone());
		shipperSupportContact.setEmailAddress(shipperData.getSupportContact().getEmail());
		shipperSupportContact.setUrl(shipperData.getSupportContact().getEmail());

		shipperType.setSupportContact(shipperSupportContact);

		fedExShipmentRequest.setShipper(shipperType);

		/** Creating Recipient Data **/

		final FedExRecipientData recipientData = fedExShipmentReqData.getRecipient();
		final Recipient recipientType = new Recipient();

		final DisplayImage recipientDisplayImage = new DisplayImage();
		recipientDisplayImage.setLarge(recipientData.getDisplayImage().getLarge());
		recipientDisplayImage.setMedium(recipientData.getDisplayImage().getMedium());
		recipientDisplayImage.setSmall(recipientData.getDisplayImage().getSmall());

		recipientType.setDisplayImage(recipientDisplayImage);
		recipientType.setDisplayName(recipientData.getDisplayName());

		final Contact recipientContact = new Contact();
		recipientContact.setPersonName(recipientData.getContact().getFirstName());
		recipientContact.setPhoneNumber(recipientData.getContact().getPhone());
		recipientContact.setEmailAddress(recipientData.getContact().getEmail());

		recipientType.setContact(recipientContact);

		fedExShipmentRequest.setRecipient(recipientType);

		/** Creating Total Declared Data **/
		final FedExTotalDeclaredValueData totalDeclaredData = fedExShipmentReqData.getTotalDeclaredValue();
		final TotalDeclaredValue totalDeclaredType = new TotalDeclaredValue();
		totalDeclaredType.setCurrencyCode(totalDeclaredData.getCurrencyCode());
		totalDeclaredType.setAmount(totalDeclaredData.getAmount());

		fedExShipmentRequest.setTotalDeclaredValue(totalDeclaredType);

		/** Creating Package Data **/
		final FedExPackageData packageData = fedExShipmentReqData.getPackages().get(0);

		final List<Package> packagesList = new ArrayList<Package>();

		final Package packageType = new Package();

		final List<Item> items = new ArrayList<Item>();
		final FedExItemData itemData = packageData.getItems().get(0);

		final Item itemType = new Item();
		itemType.setName(itemData.getName());
		itemType.setDescription(itemData.getDescription());
		itemType.setQuantity(itemData.getQuantity());
		itemType.setSku(itemData.getSku());

		final DisplayImage packageDisplayImage = new DisplayImage();
		packageDisplayImage.setLarge(itemData.getDisplayImage().getLarge());
		packageDisplayImage.setMedium(itemData.getDisplayImage().getMedium());
		packageDisplayImage.setSmall(itemData.getDisplayImage().getSmall());

		itemType.setDisplayImage(packageDisplayImage);
		items.add(itemType);
		packageType.setItems(items);

		final Weight weight = new Weight();
		weight.setUnits(packageData.getWeight().getUnits());
		weight.setValue(packageData.getWeight().getValue());
		packageType.setWeight(weight);

		final Dimensions dimensions = new Dimensions();
		dimensions.setLength(packageData.getDimensions().getLength());
		dimensions.setWidth(packageData.getDimensions().getWidth());
		dimensions.setHeight(packageData.getDimensions().getHeight());
		dimensions.setUnits(packageData.getDimensions().getUnits());
		packageType.setDimensions(dimensions);

		packagesList.add(packageType);

		fedExShipmentRequest.setPackages(packagesList);

		/** Creating ContentDescription Data **/

		fedExShipmentRequest.setContentDescription(fedExShipmentReqData.getContentDescription());

		/** Creating External References Data **/
		final FedExExternalReferencesData externalRefData = fedExShipmentReqData.getExternalReferences();

		final ExternalReferences externalRefType = new ExternalReferences();
		externalRefType.setPoNumber(externalRefData.getPoNumber());
		externalRefType.setDeptNumber(externalRefData.getDeptNumber());
		externalRefType.setRmaNumber(externalRefData.getRmaNumber());
		externalRefType.setInvoiceNumber(externalRefData.getInvoiceNumber());
		externalRefType.setUrl(externalRefData.getUrl());
		externalRefType.setOther(externalRefData.getOther());

		fedExShipmentRequest.setExternalReferences(externalRefType);

		/** Creating Notification Data **/
		final FedExNotificationsData notificationData = fedExShipmentReqData.getNotifications();
		final Notifications notificationType = new Notifications();

		final List<Email> emailList = new ArrayList<Email>();
		final Email emailType = new Email();
		final FedExEmailData emailData = notificationData.getEmail().get(0);
		emailType.setRecipientType(emailData.getRecipientType());
		emailType.setNotifyOnShipment(emailData.getNotifyOnShipment());
		emailType.setNotifyOnInTransit(emailData.getNotifyOnInTransit());
		emailType.setNotifyOnNextStop(emailData.getNotifyOnNextStop());
		emailType.setNotifyOnException(emailData.getNotifyOnException());
		emailType.setNotifyOnDelivery(emailData.getNotifyOnDelivery());
		emailType.setLocale(emailData.getLocale());
		emailType.setEmailAddress(emailData.getEmailAddress());
		emailType.setRecipientOptInTimestamp(emailData.getRecipientOptInTimestamp());

		emailList.add(emailType);

		final List<Sm> smList = new ArrayList<Sm>();

		final Sm smsType = new Sm();
		final FedExSMSlData smsData = notificationData.getSms().get(0);
		smsType.setRecipientType(smsData.getRecipientType());
		smsType.setNotifyOnShipment(smsData.getNotifyOnShipment());
		smsType.setNotifyOnInTransit(smsData.getNotifyOnInTransit());
		smsType.setNotifyOnNextStop(smsData.getNotifyOnNextStop());
		smsType.setNotifyOnException(smsData.getNotifyOnException());
		smsType.setNotifyOnDelivery(smsData.getNotifyOnDelivery());
		smsType.setLocale(smsData.getLocale());
		smsType.setPhoneNumber(smsData.getPhoneNumber());
		smsType.setRecipientOptInTimestamp(smsData.getRecipientOptInTimestamp());
		smList.add(smsType);

		notificationType.setEmail(emailList);
		notificationType.setSms(smList);

		fedExShipmentRequest.setNotifications(notificationType);

		/** Creating Visibility Releases Data **/

		final FedExVisibilityReleasesData visibilityData = fedExShipmentReqData.getVisibilityReleases();

		final VisibilityReleases visibilityRelease = new VisibilityReleases();
		visibilityRelease.setReleaseTimestamp(visibilityData.getReleaseTimestamp());
		visibilityRelease.setShowShipperDisplayName(visibilityData.getShowShipperDisplayName());
		visibilityRelease.setShowShipperDisplayImage(visibilityData.getShowShipperDisplayImage());
		visibilityRelease.setShowShipmentDisplayName(visibilityData.getShowShipmentDisplayName());
		visibilityRelease.setShowShipmentDisplayImage(visibilityData.getShowShipmentDisplayImage());
		visibilityRelease.setShowPickupLocation(visibilityData.getShowPickupLocation());

		fedExShipmentRequest.setVisibilityReleases(visibilityRelease);
		return fedExShipmentRequest;

	}

}
