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
import com.bl.facades.shipment.data.FedExNotificationsData;
import com.bl.facades.shipment.data.FedExPackageData;
import com.bl.facades.shipment.data.FedExPickupDetailData;
import com.bl.facades.shipment.data.FedExRecipientData;
import com.bl.facades.shipment.data.FedExSMSlData;
import com.bl.facades.shipment.data.FedExServiceData;
import com.bl.facades.shipment.data.FedExShipperData;
import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.FedExTotalDeclaredValueData;
import com.bl.integration.fedex.shipment.pojo.Address;
import com.bl.integration.fedex.shipment.pojo.Contact;
import com.bl.integration.fedex.shipment.pojo.DeliveryDetail;
import com.bl.integration.fedex.shipment.pojo.Dimensions;
import com.bl.integration.fedex.shipment.pojo.Email;
import com.bl.integration.fedex.shipment.pojo.ExternalReferences;
import com.bl.integration.fedex.shipment.pojo.FedExShipmentRequest;
import com.bl.integration.fedex.shipment.pojo.Location;
import com.bl.integration.fedex.shipment.pojo.Notifications;
import com.bl.integration.fedex.shipment.pojo.Package;
import com.bl.integration.fedex.shipment.pojo.PickupDetail;
import com.bl.integration.fedex.shipment.pojo.Recipient;
import com.bl.integration.fedex.shipment.pojo.Service;
import com.bl.integration.fedex.shipment.pojo.Shipper;
import com.bl.integration.fedex.shipment.pojo.Sm;
import com.bl.integration.fedex.shipment.pojo.TotalDeclaredValue;
import com.bl.integration.fedex.shipment.pojo.Weight;


/**
 * this class is responsible to populate request data for fedEx
 *
 * @author Aditi Sharma
 *
 */
public class BLFedExShipmentCreateRequestPopulator
{
	public FedExShipmentRequest convertToFedExShipmentRequest(final FedExShippingRequestData fedExShipmentReqData)
	{
		final FedExShipmentRequest fedExShipmentRequest = new FedExShipmentRequest();

		fedExShipmentRequest.setAccountNumber(fedExShipmentReqData.getShipper().getAccountNumber());

		/** Creating Service Data **/
		final Service serviceType = new Service();
		final FedExServiceData serviceData = fedExShipmentReqData.getService();

		serviceType.setServiceType(fedExShipmentReqData.getService().getServiceType());
		serviceType.setSignatureService(serviceData.getSignatureService());
		fedExShipmentRequest.setService(serviceType);

		/** Creating Category Data **/
		fedExShipmentRequest.setCategory(fedExShipmentReqData.getCategory());

		/** Creating PickupDetail Data **/
		final FedExPickupDetailData pickupDetailsData = fedExShipmentReqData.getPickupDetail();
		final PickupDetail pickupDetailType = new PickupDetail();

		final Location locationType = new Location();
		final AddressData pickupAddressData = pickupDetailsData.getLocation().getAddress();

		if (pickupAddressData != null)
		{
			final List<String> streetNameList = new ArrayList<>();
			streetNameList.add(pickupAddressData.getLine1());

			final Address pickupAddressType = new Address();
			pickupAddressType.setCity(pickupAddressData.getTown());
			pickupAddressType.setCountryCode(pickupAddressData.getCountry().getIsocode());
			pickupAddressType.setCounty(pickupAddressData.getCountry().getName());
			pickupAddressType.setPostalCode(pickupAddressData.getPostalCode());
			pickupAddressType.setStateOrProvinceCode(pickupAddressData.getRegion().getIsocodeShort());
			pickupAddressType.setStreetLines(streetNameList);

			locationType.setAddress(pickupAddressType);
		}
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
		deliveryAddress.setStateOrProvinceCode(delivertAddressData.getRegion().getIsocodeShort());
		deliveryAddress.setCountryCode(delivertAddressData.getCountry().getIsocode());
		deliveryAddress.setCounty(delivertAddressData.getCountry().getName());
		deliveryAddress.setPostalCode(delivertAddressData.getPostalCode());

		deliveryLocation.setAddress(deliveryAddress);
		deliveryLocation.setResidential(true);
		deliveryLocation.setHoursOfOperationStart(1459897586589l);
		deliveryLocation.setHoursOfOperationEnd(1459897586589l);

		deliveryDetailType.setLocation(deliveryLocation);
		deliveryDetailType.setInstructions(deliveryDetailData.getInstructions());

		fedExShipmentRequest.setDeliveryDetail(deliveryDetailType);

		/** Creating Shipper Data **/
		final FedExShipperData shipperData = fedExShipmentReqData.getShipper();

		final Shipper shipperType = new Shipper();
		shipperType.setAccountNumber(shipperData.getAccountNumber());
		shipperType.setDisplayName(shipperData.getDisplayName());

		final AddressData shipperContactData = shipperData.getContact();
		final Contact shipperContact = new Contact();
		shipperContact.setPersonName(shipperContactData.getFirstName());
		shipperContact.setCompanyName(shipperContactData.getCompanyName());
		shipperContact.setPhoneNumber(shipperContactData.getPhone());

		shipperType.setContact(shipperContact);

		fedExShipmentRequest.setShipper(shipperType);

		/** Creating Total Declared Data **/
		final FedExTotalDeclaredValueData totalDeclaredData = fedExShipmentReqData.getTotalDeclaredValue();
		final TotalDeclaredValue totalDeclaredType = new TotalDeclaredValue();
		totalDeclaredType.setCurrencyCode(totalDeclaredData.getCurrencyCode());
		totalDeclaredType.setAmount(totalDeclaredData.getAmount());

		fedExShipmentRequest.setTotalDeclaredValue(totalDeclaredType);

		/** Creating Package Data **/
		final FedExPackageData packageData = fedExShipmentReqData.getPackages().get(0);

		final List<Package> packagesList = new ArrayList<>();

		final Package packageType = new Package();

		final Weight weight = new Weight();
		weight.setUnits(packageData.getWeight().getUnits());
		weight.setValue(packageData.getWeight().getValue());
		packageType.setWeight(weight);

		final Dimensions dimensions = new Dimensions();
		if (packageData.getDimensions() != null)
		{
			dimensions.setLength(packageData.getDimensions().getLength());
			dimensions.setWidth(packageData.getDimensions().getWidth());
			dimensions.setHeight(packageData.getDimensions().getHeight());
			dimensions.setUnits(packageData.getDimensions().getUnits());
		}
		packageType.setDimensions(dimensions);

		packagesList.add(packageType);

		fedExShipmentRequest.setPackages(packagesList);

		/** Creating Recipient Data **/
		final FedExRecipientData fedExRecipientData = fedExShipmentReqData.getRecipient();
		final AddressData recipientContactData = fedExRecipientData.getContact();

		final Recipient recipientType = new Recipient();
		final Contact recipientContact = new Contact();
		recipientContact.setPersonName(recipientContactData.getFirstName());
		recipientContact.setCompanyName(recipientContactData.getCompanyName());
		recipientContact.setPhoneNumber(recipientContactData.getPhone());
		recipientContact.setEmailAddress(recipientContactData.getEmail());

		recipientType.setContact(recipientContact);
		recipientType.setDisplayName(fedExRecipientData.getDisplayName());

		fedExShipmentRequest.setRecipient(recipientType);

		/** Creating External References Data **/
		final FedExExternalReferencesData externalRefData = fedExShipmentReqData.getExternalReferences();

		if (externalRefData != null)
		{
			final ExternalReferences externalRefType = new ExternalReferences();
			externalRefType.setPoNumber(externalRefData.getPoNumber());
			externalRefType.setDeptNumber(externalRefData.getDeptNumber());
			externalRefType.setRmaNumber(externalRefData.getRmaNumber());
			externalRefType.setInvoiceNumber(externalRefData.getInvoiceNumber());
			externalRefType.setUrl(externalRefData.getUrl());
			externalRefType.setOther(externalRefData.getOther());

			fedExShipmentRequest.setExternalReferences(externalRefType);
		}

		/** Creating Notification Data **/
		final FedExNotificationsData notificationData = fedExShipmentReqData.getNotifications();
		final Notifications notificationType = new Notifications();

		if (notificationData != null)
		{
			final List<Email> emailList = new ArrayList<>();
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

			final List<Sm> smList = new ArrayList<>();

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
		}

		fedExShipmentRequest.setNotifications(notificationType);

		return fedExShipmentRequest;

	}

}
