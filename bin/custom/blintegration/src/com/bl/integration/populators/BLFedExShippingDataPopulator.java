/**
 *
 */
package com.bl.integration.populators;

import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.beans.factory.annotation.Value;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.facades.shipment.data.FedExDeliveryDetailData;
import com.bl.facades.shipment.data.FedExDimensionsData;
import com.bl.facades.shipment.data.FedExEmailData;
import com.bl.facades.shipment.data.FedExExternalReferencesData;
import com.bl.facades.shipment.data.FedExLocationData;
import com.bl.facades.shipment.data.FedExNotificationsData;
import com.bl.facades.shipment.data.FedExPackageData;
import com.bl.facades.shipment.data.FedExPickupDetailData;
import com.bl.facades.shipment.data.FedExSMSlData;
import com.bl.facades.shipment.data.FedExServiceData;
import com.bl.facades.shipment.data.FedExShipperData;
import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.FedExTotalDeclaredValueData;
import com.bl.facades.shipment.data.FedExWeightData;
import com.bl.integration.constants.BlintegrationConstants;


/**
 * @author Aditi Sharma
 *
 */
public class BLFedExShippingDataPopulator
{
	@Resource(name = "addressConverter")
	private Converter<AddressModel, AddressData> addressConverter;

	@Value("${blintegration.fedex.shipper.account.number}")
	private String shipperAccountNo;

	@Value("${blintegration.fedex.same.day.postalcode}")
	private String sameDayPostalCode;

	@Value("${blintegration.fedex.same.day.timezone}")
	private String sameDayTimeZone;

	@Value("${blintegration.fedex.next.day.line1}")
	private String rushLine1;

	@Value("${blintegration.fedex.next.day.town}")
	private String rushTown;

	@Value("${blintegration.fedex.next.day.region.code}")
	private String rushRegionCode;

	@Value("${blintegration.fedex.next.day.country.code}")
	private String rushCountryCode;

	@Value("${blintegration.fedex.next.day.postalcode}")
	private String rushPostalCode;

	@Value("${blintegration.fedex.next.day.timezone}")
	private String rushTimeZone;

	/**
	 * @param consignment
	 * @return
	 */
	public FedExShippingRequestData populateFedExShipmentRequest(final PackagingInfoModel packagingInfo)
	{
		final FedExShippingRequestData fedExShippingRequestData = new FedExShippingRequestData();

		final ConsignmentModel consignment = packagingInfo.getConsignment();

		/** Creating Service Data **/

		final FedExServiceData fedExServiceData = new FedExServiceData();
		if (consignment.getOptimizedShippingType() != null)
		{
			fedExServiceData.setServiceType(consignment.getOptimizedShippingType().getServiceTypeCode());
		}
		fedExServiceData.setSignatureService(BlintegrationConstants.SIGNATURE_SERVICE_CODE);

		fedExShippingRequestData.setService(fedExServiceData);

		/** Creating Category Data **/
		fedExShippingRequestData.setCategory(BlintegrationConstants.CATEGORY_CODE);


		/** Creating Pickup Details Data **/
		final FedExPickupDetailData fedExPickupDetailData = new FedExPickupDetailData();
		populatePickupDetails(consignment, fedExPickupDetailData);
		fedExShippingRequestData.setPickupDetail(fedExPickupDetailData);

		/** Creating Delivery Details Data **/

		final FedExDeliveryDetailData fedExDeliveryDetailData = new FedExDeliveryDetailData();
		populateDeliveryDetails(consignment, fedExDeliveryDetailData);
		fedExShippingRequestData.setDeliveryDetail(fedExDeliveryDetailData);

		/** Creating Shipper Details Data **/

		final FedExShipperData fedExShipperData = new FedExShipperData();
		fedExShipperData.setAccountNumber(shipperAccountNo);

		fedExShipperData.setDisplayName("San Carlos Warehouse / Waltham Warehouse");

		final AddressData shipperContactData = new AddressData();
		shipperContactData.setFirstName("Returns Department ");
		shipperContactData.setCompanyName("Borrowlenses");
		shipperContactData.setPhone("844-853-6737");
		fedExShipperData.setContact(shipperContactData);

		fedExShippingRequestData.setShipper(fedExShipperData);

		/** Creating Total Declared Value Data **/
		final FedExTotalDeclaredValueData fedExTotalDeclaredValueData = new FedExTotalDeclaredValueData();
		fedExTotalDeclaredValueData.setCurrencyCode("USD");
		if (consignment.getOrder().getDeliveryMode() instanceof BlRushDeliveryModeModel)
		{
			fedExTotalDeclaredValueData.setAmount(100);
		}
		fedExShippingRequestData.setTotalDeclaredValue(fedExTotalDeclaredValueData);

		/** Creating Package Data **/
		final List<FedExPackageData> packageList = new ArrayList<>();
		final FedExPackageData packageData = new FedExPackageData();

		final FedExDimensionsData fedExDimensionsData = new FedExDimensionsData();
		fedExDimensionsData.setHeight(Integer.valueOf(packagingInfo.getHeight()));
		fedExDimensionsData.setLength(Integer.valueOf(packagingInfo.getLength()));
		fedExDimensionsData.setWidth(Integer.valueOf(packagingInfo.getWidth()));
		fedExDimensionsData.setUnits(packagingInfo.getDimensionUnit());
		packageData.setDimensions(fedExDimensionsData);

		final FedExWeightData fedExWeightData = new FedExWeightData();
		fedExWeightData.setUnits(packagingInfo.getWeightUnit());
		fedExWeightData.setValue(Integer.valueOf(packagingInfo.getGrossWeight()));
		packageData.setWeight(fedExWeightData);
		fedExShippingRequestData.setPackages(packageList);

		/** Creating External References Data **/
		final FedExExternalReferencesData fedExExternalReferencesData = new FedExExternalReferencesData();
		fedExExternalReferencesData.setPoNumber("75024");
		fedExExternalReferencesData.setDeptNumber("001N");
		fedExExternalReferencesData.setRmaNumber("RMA00101");
		fedExExternalReferencesData.setInvoiceNumber("001C4453TX75024");
		fedExExternalReferencesData.setUrl("http://retailer.com/orders/75024");
		fedExExternalReferencesData.setOther("Other");
		fedExShippingRequestData.setExternalReferences(fedExExternalReferencesData);

		/** Creating Notification Data **/
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
		fedExSMSlData.setRecipientType(BlintegrationConstants.RECIPIENT_TYPE);
		fedExSMSlData.setNotifyOnShipment(false);
		fedExSMSlData.setNotifyOnInTransit(false);
		fedExSMSlData.setNotifyOnNextStop(true);
		fedExSMSlData.setNotifyOnException(true);
		fedExSMSlData.setNotifyOnDelivery(true);
		fedExSMSlData.setLocale("en");
		// Need to check
		fedExSMSlData.setPhoneNumber("555-555-3273");
		fedExSMSlData.setRecipientOptInTimestamp(consignment.getOrder().getCreationtime().getTime());
		smsList.add(fedExSMSlData);
		fedExNotificationsData.setSms(smsList);
		fedExShippingRequestData.setNotifications(fedExNotificationsData);

		return fedExShippingRequestData;

	}

	/**
	 * @param consignment
	 * @param fedExDeliveryDetailData
	 */
	private void populateDeliveryDetails(final ConsignmentModel consignment, final FedExDeliveryDetailData fedExDeliveryDetailData)
	{
		final FedExLocationData deliveryLocationData = new FedExLocationData();
		final AddressModel deliveryAddress = consignment.getOrder().getDeliveryAddress();

		final AddressData deliveryData = addressConverter.convert(deliveryAddress);
		final AddressData deliveryAddressData = new AddressData();
		deliveryAddressData.setLine1(deliveryData.getFirstName());
		deliveryAddressData.setTown(deliveryData.getTown());

		if (deliveryData.getRegion() != null && deliveryData.getRegion().getIsocodeShort() != null)
		{
			final RegionData deliveryRegionData = new RegionData();
			deliveryRegionData.setIsocodeShort(deliveryData.getRegion().getIsocodeShort());
			deliveryAddressData.setRegion(deliveryRegionData);
		}

		if (deliveryData.getCountry() != null && deliveryData.getCountry().getIsocode() != null)
		{
			final CountryData deliveryCountryData = new CountryData();
			deliveryCountryData.setIsocode(deliveryData.getCountry().getIsocode());
			deliveryCountryData.setName(deliveryData.getCountry().getName());
			deliveryAddressData.setCountry(deliveryCountryData);
		}

		deliveryAddressData.setPostalCode(deliveryData.getPostalCode());
		deliveryLocationData.setAddress(deliveryAddressData);
		deliveryLocationData.setResidential(true);

		if (consignment.getOrder().getDeliveryMode() instanceof ZoneDeliveryModeModel)
		{
			final long readyTime = getReadyTime(consignment.getOrder().getDeliveryMode());
			deliveryLocationData.setHoursOfOperationStart(readyTime);
			deliveryLocationData.setHoursOfOperationEnd(1459897586589l);
		}

		fedExDeliveryDetailData.setInstructions(consignment.getOrder().getDeliveryNotes());
		fedExDeliveryDetailData.setLocation(deliveryLocationData);
	}

	/**
	 * @param consignment
	 * @param fedExPickupDetailData
	 */
	private void populatePickupDetails(final ConsignmentModel consignment, final FedExPickupDetailData fedExPickupDetailData)
	{
		final DeliveryModeModel deliveryMode = consignment.getOrder().getDeliveryMode();

		final FedExLocationData fedExLocationData = new FedExLocationData();
		final AddressData addressData = new AddressData();
		if (deliveryMode instanceof ZoneDeliveryModeModel)
		{
			if (BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY
					.equals(((ZoneDeliveryModeModel) deliveryMode).getShippingGroup().getCode()))
			{
				AddressModel warehouseAddress = new AddressModel();
				if (consignment.getWarehouse() != null && consignment.getWarehouse().getPointsOfService() != null)
				{
					warehouseAddress = consignment.getWarehouse().getPointsOfService().iterator().next().getAddress();
				}
				addressData.setLine1(warehouseAddress.getLine1());
				addressData.setTown(warehouseAddress.getTown());

				if (warehouseAddress.getRegion() != null && warehouseAddress.getRegion().getIsocode() != null)
				{
					final RegionData regionData = new RegionData();
					regionData.setIsocodeShort(warehouseAddress.getRegion().getIsocodeShort());
					addressData.setRegion(regionData);
				}

				if (warehouseAddress.getCountry() != null && warehouseAddress.getCountry().getIsocode() != null)
				{
					final CountryData countryData = new CountryData();
					countryData.setIsocode(warehouseAddress.getCountry().getIsocode());
					addressData.setCountry(countryData);
				}

				addressData.setPostalCode(sameDayPostalCode);
				fedExLocationData.setAddress(addressData);
				fedExPickupDetailData.setLocalTimeZone(sameDayTimeZone);
			}
			else if (BlDeliveryModeLoggingConstants.NEXT_DAY_RUSH_DELIVERY
					.equals(((ZoneDeliveryModeModel) deliveryMode).getShippingGroup().getCode()))
			{
				addressData.setLine1(rushLine1);
				addressData.setTown(rushTown);

				final RegionData regionData = new RegionData();
				regionData.setIsocodeShort(rushRegionCode);
				addressData.setRegion(regionData);

				final CountryData countryData = new CountryData();
				countryData.setIsocode(rushCountryCode);
				addressData.setCountry(countryData);

				addressData.setPostalCode(rushPostalCode);
				fedExLocationData.setAddress(addressData);
				fedExPickupDetailData.setLocalTimeZone(rushTimeZone);
			}

			final long readyTime = getReadyTime(deliveryMode);
			fedExPickupDetailData.setReadyTime(readyTime);
			fedExLocationData.setHoursOfOperationStart(readyTime);
			fedExLocationData.setHoursOfOperationEnd(1459897586589l);
		}

		fedExLocationData.setResidential(false);
		fedExPickupDetailData.setLocation(fedExLocationData);
	}

	/**
	 * @param deliveryMode
	 * @return
	 */
	private long getReadyTime(final DeliveryModeModel deliveryMode)
	{
		final String cutOffTime = ((ZoneDeliveryModeModel) deliveryMode).getCutOffTime();
		return Long.parseLong(cutOffTime);
	}

}
