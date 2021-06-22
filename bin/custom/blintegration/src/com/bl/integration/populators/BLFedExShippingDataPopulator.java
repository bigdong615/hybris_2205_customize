/**
 *
 */
package com.bl.integration.populators;

import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;

import com.bl.facades.shipment.data.FedExDeliveryDetailData;
import com.bl.facades.shipment.data.FedExDimensionsData;
import com.bl.facades.shipment.data.FedExDisplayImageData;
import com.bl.facades.shipment.data.FedExEmailData;
import com.bl.facades.shipment.data.FedExExternalReferencesData;
import com.bl.facades.shipment.data.FedExItemData;
import com.bl.facades.shipment.data.FedExLocationData;
import com.bl.facades.shipment.data.FedExNotificationsData;
import com.bl.facades.shipment.data.FedExPackageData;
import com.bl.facades.shipment.data.FedExPickupDetailData;
import com.bl.facades.shipment.data.FedExRecipientData;
import com.bl.facades.shipment.data.FedExRestrictionData;
import com.bl.facades.shipment.data.FedExSMSlData;
import com.bl.facades.shipment.data.FedExServiceData;
import com.bl.facades.shipment.data.FedExShipperData;
import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.FedExTotalDeclaredValueData;
import com.bl.facades.shipment.data.FedExVisibilityReleasesData;
import com.bl.facades.shipment.data.FedExWeightData;


/**
 * @author Aditi Sharma
 *
 */
public class BLFedExShippingDataPopulator
{
	@Resource(name = "addressConverter")
	private Converter<AddressModel, AddressData> addressConverter;

	public FedExShippingRequestData populateFedExShipmentRequest(final ConsignmentModel consignment)
	{
		final FedExShippingRequestData fedExShippingRequestData = new FedExShippingRequestData();

		final FedExRestrictionData fedExRestrictionData = new FedExRestrictionData();
		fedExRestrictionData.setNoHAL(false);
		fedExRestrictionData.setNoRecipientRedirect(false);
		fedExRestrictionData.setNoRemoteSignature(false);

		/** Creating Service Data **/
		final List<Object> specialServices = new ArrayList<>();

		final FedExServiceData fedExServiceData = new FedExServiceData();
		fedExServiceData.setServiceType("LM");
		fedExServiceData.setSignatureService("ASR");
		fedExServiceData.setSpecialServices(specialServices);
		fedExServiceData.setRestrictions(fedExRestrictionData);

		fedExShippingRequestData.setService(fedExServiceData);

		/** Creating Category Data **/
		fedExShippingRequestData.setCategory("CONSUMER_GOODS");


		/** Creating Pickup Details Data **/
		final FedExPickupDetailData fedExPickupDetailData = new FedExPickupDetailData();

		final FedExLocationData fedExLocationData = new FedExLocationData();
		final AddressData addressData = new AddressData();
		addressData.setLine1("7900 Legacy Dr");
		addressData.setTown("Plano");

		final RegionData regionData = new RegionData();
		regionData.setIsocode("TX");
		addressData.setRegion(regionData);

		final CountryData countryData = new CountryData();
		countryData.setIsocode("US");
		addressData.setCountry(countryData);

		addressData.setPostalCode("75024");
		fedExLocationData.setAddress(addressData);
		fedExLocationData.setResidential(false);
		fedExLocationData.setHoursOfOperationStart(1459897586589l);
		fedExLocationData.setHoursOfOperationEnd(1459897586589l);

		fedExPickupDetailData.setLocation(fedExLocationData);
		fedExPickupDetailData.setReadyTime(1461700301796l);
		fedExPickupDetailData.setLocalTimeZone("US/Central");
		fedExPickupDetailData.setInstructions("Handle with care");

		fedExShippingRequestData.setPickupDetail(fedExPickupDetailData);

		/** Creating Delivery Details Data **/

		final FedExDeliveryDetailData fedExDeliveryDetailData = new FedExDeliveryDetailData();
		final FedExLocationData deliveryLocationData = new FedExLocationData();
		final AddressModel deliveryAddress = consignment.getOrder().getDeliveryAddress();

		final AddressData deliveryData = addressConverter.convert(deliveryAddress);
		final AddressData deliveryAddressData = new AddressData();
		deliveryAddressData.setLine1(deliveryData.getFirstName());
		deliveryAddressData.setTown(deliveryData.getTown());

		final RegionData deliveryRegionData = new RegionData();
		deliveryRegionData.setIsocodeShort(deliveryData.getRegion().getIsocodeShort());
		deliveryAddressData.setRegion(deliveryRegionData);

		final CountryData deliveryCountryData = new CountryData();
		deliveryCountryData.setIsocode(deliveryData.getCountry().getIsocode());
		deliveryCountryData.setName(deliveryData.getCountry().getName());
		deliveryAddressData.setCountry(deliveryCountryData);

		deliveryAddressData.setPostalCode(deliveryData.getPostalCode());
		deliveryLocationData.setAddress(deliveryAddressData);
		deliveryLocationData.setResidential(false);
		deliveryLocationData.setHoursOfOperationStart(1459897586589l);
		deliveryLocationData.setHoursOfOperationEnd(1459897586589l);

		fedExDeliveryDetailData.setInstructions("Fragile");
		fedExDeliveryDetailData.setLocation(deliveryLocationData);

		fedExShippingRequestData.setDeliveryDetail(fedExDeliveryDetailData);

		final FedExShipperData fedExShipperData = new FedExShipperData();
		fedExShipperData.setAccountNumber("AC0019212");

		final FedExDisplayImageData fedExDisplayImageData = new FedExDisplayImageData();
		fedExDisplayImageData.setLarge("http://content.retailer.com/logos/logo-12-l.jpg");
		fedExDisplayImageData.setMedium("http://content.retailer.com/logos/logo-12-m.jpg");
		fedExDisplayImageData.setSmall("http://content.retailer.com/logos/logo-12-s.jpg");
		fedExShipperData.setDisplayImage(fedExDisplayImageData);

		fedExShipperData.setDisplayName("Shane’s Shoe Store");

		final AddressData shipperContactData = new AddressData();
		shipperContactData.setFirstName("Shane");
		shipperContactData.setCompanyName("SHANES SHOES LLC");
		shipperContactData.setPhone("874-610-2122");
		shipperContactData.setEmail("shane.doe@retailer.com");
		fedExShipperData.setContact(shipperContactData);

		final AddressData shipperSupportData = new AddressData();
		shipperSupportData.setPhone("1-800-555-5121");
		shipperSupportData.setEmail("support@retailer.com");
		shipperSupportData.setUrl("www.retailer.com/support");
		fedExShipperData.setSupportContact(shipperSupportData);
		fedExShippingRequestData.setShipper(fedExShipperData);

		final FedExRecipientData fedExRecipientData = new FedExRecipientData();
		fedExRecipientData.setDisplayName("John Doe");

		final FedExDisplayImageData recipientImageData = new FedExDisplayImageData();
		recipientImageData.setLarge("http://retailer.com/profiles/234234/profileImage-large.jpg");
		recipientImageData.setMedium("http://retailer.com/profiles/234234/profileImage-medium.jpg");
		recipientImageData.setSmall("http://retailer.com/profiles/234234/profileImage-small.jpg");
		fedExRecipientData.setDisplayImage(recipientImageData);

		final AddressData recipientAddressData = new AddressData();
		recipientAddressData.setFirstName("John Doe");
		recipientAddressData.setPhone("978-335-3273");
		recipientAddressData.setEmail("john.doe@gmail.com");
		fedExRecipientData.setContact(recipientAddressData);
		fedExShippingRequestData.setRecipient(fedExRecipientData);

		final FedExTotalDeclaredValueData fedExTotalDeclaredValueData = new FedExTotalDeclaredValueData();
		fedExTotalDeclaredValueData.setCurrencyCode("USD");
		fedExTotalDeclaredValueData.setAmount(40);
		fedExShippingRequestData.setTotalDeclaredValue(fedExTotalDeclaredValueData);

		final List<FedExPackageData> packageList = new ArrayList<>();
		final FedExPackageData packageData = new FedExPackageData();

		final List<FedExItemData> items = new ArrayList<>();
		final FedExItemData itemData = new FedExItemData();
		itemData.setName("Shoe");
		itemData.setDescription("Brand Name Shoe");
		itemData.setQuantity(1);
		itemData.setSku("SKU0001290");

		final FedExDisplayImageData itemDisplayImageData = new FedExDisplayImageData();
		itemDisplayImageData.setLarge("http://retailer.com/images/shoe.jpg");
		itemDisplayImageData.setMedium("http://retailer.com/images/shoe-thumbnail.jpg");
		itemDisplayImageData.setSmall("http://retailer.com/images/shoe-thumbnail.jpg");
		itemData.setDisplayImage(itemDisplayImageData);
		items.add(itemData);
		packageList.add(packageData);
		packageData.setItems(items);

		final FedExDimensionsData fedExDimensionsData = new FedExDimensionsData();
		fedExDimensionsData.setHeight(6);
		fedExDimensionsData.setLength(8);
		fedExDimensionsData.setWidth(4);
		fedExDimensionsData.setUnits("IN");
		packageData.setDimensions(fedExDimensionsData);

		final FedExWeightData fedExWeightData = new FedExWeightData();
		fedExWeightData.setUnits("LB");
		fedExWeightData.setValue(2);
		packageData.setWeight(fedExWeightData);
		fedExShippingRequestData.setPackages(packageList);

		fedExShippingRequestData.setContentDescription("Brand Name Shoes");

		final FedExExternalReferencesData fedExExternalReferencesData = new FedExExternalReferencesData();
		fedExExternalReferencesData.setPoNumber("75024");
		fedExExternalReferencesData.setDeptNumber("001N");
		fedExExternalReferencesData.setRmaNumber("RMA00101");
		fedExExternalReferencesData.setInvoiceNumber("001C4453TX75024");
		fedExExternalReferencesData.setUrl("http://retailer.com/orders/75024");
		fedExExternalReferencesData.setOther("Other");
		fedExShippingRequestData.setExternalReferences(fedExExternalReferencesData);

		final FedExNotificationsData fedExNotificationsData = new FedExNotificationsData();
		final List<FedExEmailData> emailList = new ArrayList<>();
		final FedExEmailData fedExEmailData = new FedExEmailData();
		fedExEmailData.setRecipientType("RECIPIENT");
		fedExEmailData.setNotifyOnShipment(true);
		fedExEmailData.setNotifyOnInTransit(true);
		fedExEmailData.setNotifyOnNextStop(true);
		fedExEmailData.setNotifyOnException(true);
		fedExEmailData.setNotifyOnDelivery(true);
		fedExEmailData.setLocale("en");
		fedExEmailData.setEmailAddress("john.doe@gmail.com");
		fedExEmailData.setRecipientOptInTimestamp(1459897586589l);
		emailList.add(fedExEmailData);
		fedExNotificationsData.setEmail(emailList);

		final List<FedExSMSlData> smsList = new ArrayList<>();
		final FedExSMSlData fedExSMSlData = new FedExSMSlData();
		fedExSMSlData.setRecipientType("RECIPIENT");
		fedExSMSlData.setNotifyOnShipment(true);
		fedExSMSlData.setNotifyOnInTransit(false);
		fedExSMSlData.setNotifyOnNextStop(false);
		fedExSMSlData.setNotifyOnException(true);
		fedExSMSlData.setNotifyOnDelivery(true);
		fedExSMSlData.setLocale("en");
		fedExSMSlData.setPhoneNumber("555-555-3273");
		fedExSMSlData.setRecipientOptInTimestamp(1459897586589l);
		smsList.add(fedExSMSlData);
		fedExNotificationsData.setSms(smsList);
		fedExShippingRequestData.setNotifications(fedExNotificationsData);

		final FedExVisibilityReleasesData visibilityReleases = new FedExVisibilityReleasesData();
		visibilityReleases.setReleaseTimestamp(1459897586589l);
		visibilityReleases.setShowShipperDisplayName(true);
		visibilityReleases.setShowShipperDisplayImage(true);
		visibilityReleases.setShowShipmentDisplayName(true);
		visibilityReleases.setShowShipmentDisplayImage(true);
		visibilityReleases.setShowPickupLocation(true);

		fedExShippingRequestData.setVisibilityReleases(visibilityReleases);
		return fedExShippingRequestData;

	}

}
