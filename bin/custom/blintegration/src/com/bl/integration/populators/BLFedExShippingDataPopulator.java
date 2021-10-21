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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import javax.annotation.Resource;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.facades.shipment.data.FedExDeliveryDetailData;
import com.bl.facades.shipment.data.FedExDimensionsData;
import com.bl.facades.shipment.data.FedExEmailData;
import com.bl.facades.shipment.data.FedExLocationData;
import com.bl.facades.shipment.data.FedExNotificationsData;
import com.bl.facades.shipment.data.FedExPackageData;
import com.bl.facades.shipment.data.FedExPickupDetailData;
import com.bl.facades.shipment.data.FedExRecipientData;
import com.bl.facades.shipment.data.FedExSMSlData;
import com.bl.facades.shipment.data.FedExServiceData;
import com.bl.facades.shipment.data.FedExShipperData;
import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.FedExTotalDeclaredValueData;
import com.bl.facades.shipment.data.FedExWeightData;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.logging.BlLogger;


/**
 * @author Aditi Sharma
 *
 */
public class BLFedExShippingDataPopulator
{
	private static final Logger LOG = Logger.getLogger(BLFedExShippingDataPopulator.class);

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

	@Value("${blintegration.fedex.total.declared.amount}")
	private int totalDeclaredAmount;

	@Value("${blintegration.fedex.signature.service.code}")
	private String signatureServiceCode;

	@Value("${blintegration.fedex.shipper.first.name}")
	private String shipperFirstName;

	@Value("${blintegration.fedex.shipper.company.name}")
	private String shipperCompanyName;

	@Value("${blintegration.fedex.shipper.contact.number}")
	private String shipperContactNumber;

	/**
	 * @param consignment
	 * @return
	 * @throws ParseException
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
		fedExServiceData.setSignatureService(signatureServiceCode);

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

		fedExShipperData.setDisplayName(consignment.getWarehouse().getName());

		final AddressData shipperContactData = new AddressData();
		shipperContactData.setFirstName(shipperFirstName);
		shipperContactData.setCompanyName(shipperCompanyName);
		shipperContactData.setPhone(shipperContactNumber);
		fedExShipperData.setContact(shipperContactData);

		fedExShippingRequestData.setShipper(fedExShipperData);

		/** Creating Recipient Data **/
		final AddressModel deliveryAddress = consignment.getOrder().getDeliveryAddress();

		final AddressData deliveryData = addressConverter.convert(deliveryAddress);

		final FedExRecipientData recipientData = new FedExRecipientData();
		final AddressData recipientAddressData = new AddressData();
		recipientAddressData.setFirstName(deliveryData.getFirstName());
		recipientAddressData.setCompanyName(deliveryData.getCompanyName());
		recipientAddressData.setPhone(deliveryData.getPhone());
		recipientAddressData.setEmail(deliveryData.getEmail());

		recipientData.setContact(shipperContactData);
		recipientData.setDisplayName(deliveryData.getFirstName());

		fedExShippingRequestData.setRecipient(recipientData);

		/** Creating Total Declared Value Data **/
		final FedExTotalDeclaredValueData fedExTotalDeclaredValueData = new FedExTotalDeclaredValueData();
		fedExTotalDeclaredValueData.setCurrencyCode(consignment.getOrder().getCurrency().getIsocode());
		fedExTotalDeclaredValueData.setAmount(totalDeclaredAmount);

		fedExShippingRequestData.setTotalDeclaredValue(fedExTotalDeclaredValueData);

		/** Creating Package Data **/
		final List<FedExPackageData> packageList = new ArrayList<>();
		final FedExPackageData packageData = new FedExPackageData();

		final FedExDimensionsData fedExDimensionsData = new FedExDimensionsData();
		fedExDimensionsData.setHeight(Integer.parseInt(packagingInfo.getHeight()));
		fedExDimensionsData.setLength(Integer.parseInt(packagingInfo.getLength()));
		fedExDimensionsData.setWidth(Integer.parseInt(packagingInfo.getWidth()));
		fedExDimensionsData.setUnits(packagingInfo.getDimensionUnit());
		packageData.setDimensions(fedExDimensionsData);

		final FedExWeightData fedExWeightData = new FedExWeightData();
		fedExWeightData.setUnits(packagingInfo.getWeightUnit());
		final String replace = packagingInfo.getGrossWeight().replace(".", "");
		fedExWeightData.setValue(Integer.valueOf(replace));
		packageData.setWeight(fedExWeightData);

		packageList.add(packageData);
		fedExShippingRequestData.setPackages(packageList);

		/** Creating Notification Data **/
		final FedExNotificationsData fedExNotificationsData = new FedExNotificationsData();
		final List<FedExEmailData> emailList = new ArrayList<>();
		final FedExEmailData fedExEmailData = new FedExEmailData();
		fedExEmailData.setRecipientType(BlintegrationConstants.RECIPIENT_TYPE);
		fedExEmailData.setNotifyOnShipment(false);
		fedExEmailData.setNotifyOnInTransit(false);
		fedExEmailData.setNotifyOnNextStop(true);
		fedExEmailData.setNotifyOnException(true);
		fedExEmailData.setNotifyOnDelivery(true);
		fedExEmailData.setEmailAddress(packagingInfo.getConsignment().getOrder().getDeliveryAddress().getEmail());
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
		if (fedExDeliveryDetailData.getLocation() != null && fedExDeliveryDetailData.getLocation().getAddress() != null)
		{
			fedExSMSlData.setPhoneNumber(fedExDeliveryDetailData.getLocation().getAddress().getPhone());
		}
		fedExSMSlData.setRecipientOptInTimestamp(consignment.getOrder().getCreationtime().getTime());
		smsList.add(fedExSMSlData);
		fedExNotificationsData.setSms(smsList);
		fedExShippingRequestData.setNotifications(fedExNotificationsData);

		return fedExShippingRequestData;

	}

	/**
	 * @param consignment
	 * @param fedExDeliveryDetailData
	 * @throws ParseException
	 */
	private void populateDeliveryDetails(final ConsignmentModel consignment, final FedExDeliveryDetailData fedExDeliveryDetailData)
	{
		final FedExLocationData deliveryLocationData = new FedExLocationData();
		final AddressModel deliveryAddress = consignment.getOrder().getDeliveryAddress();

		final AddressData deliveryData = addressConverter.convert(deliveryAddress);
		final AddressData deliveryAddressData = new AddressData();
		deliveryAddressData.setFirstName(deliveryData.getFirstName());
		deliveryAddress.setLastname(deliveryData.getLastName());
		deliveryAddressData.setLine1(deliveryData.getLine1());
		deliveryAddressData.setLine2(deliveryData.getLine2());
		deliveryAddressData.setTown(deliveryData.getTown());
		deliveryAddressData.setPhone(deliveryData.getPhone());
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
			deliveryLocationData.setHoursOfOperationStart(getStartTimeAndEndTime(consignment.getOrder().getDeliveryMode(), 0));
			deliveryLocationData.setHoursOfOperationEnd(getStartTimeAndEndTime(consignment.getOrder().getDeliveryMode(), 3));
		}

		fedExDeliveryDetailData.setInstructions(consignment.getOrder().getDeliveryNotes());
		fedExDeliveryDetailData.setLocation(deliveryLocationData);
	}

	/**
	 * @param consignment
	 * @param fedExPickupDetailData
	 * @throws ParseException
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
				populateSameDayDeliveryData(consignment, fedExPickupDetailData, fedExLocationData, addressData);
			}
			else if (BlDeliveryModeLoggingConstants.NEXT_DAY_RUSH_DELIVERY
					.equals(((ZoneDeliveryModeModel) deliveryMode).getShippingGroup().getCode()))
			{
				populateNextDayRushDelivery(fedExPickupDetailData, fedExLocationData, addressData);
			}

			fedExPickupDetailData.setReadyTime(getStartTimeAndEndTime(consignment.getOrder().getDeliveryMode(), 0));
			fedExLocationData.setHoursOfOperationStart(getStartTimeAndEndTime(consignment.getOrder().getDeliveryMode(), 0));
			fedExLocationData.setHoursOfOperationEnd(getStartTimeAndEndTime(consignment.getOrder().getDeliveryMode(), 3));
		}

		fedExLocationData.setResidential(false);
		fedExPickupDetailData.setLocation(fedExLocationData);
	}

	/**
	 * method will be used to populate next day rush delivery data
	 *
	 * @param fedExPickupDetailData
	 * @param fedExLocationData
	 * @param addressData
	 */
	private void populateNextDayRushDelivery(final FedExPickupDetailData fedExPickupDetailData,
			final FedExLocationData fedExLocationData, final AddressData addressData)
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

	/**
	 * method will be used to populate same day delivery data
	 *
	 * @param consignment
	 * @param fedExPickupDetailData
	 * @param fedExLocationData
	 * @param addressData
	 */
	private void populateSameDayDeliveryData(final ConsignmentModel consignment, final FedExPickupDetailData fedExPickupDetailData,
			final FedExLocationData fedExLocationData, final AddressData addressData)
	{
		AddressModel warehouseAddress = new AddressModel();
		if (Objects.nonNull(consignment.getWarehouse()) && Objects.nonNull(consignment.getWarehouse().getPointsOfService()))
		{
			warehouseAddress = consignment.getWarehouse().getPointsOfService().iterator().next().getAddress();
		}
		addressData.setLine1(warehouseAddress.getLine1());
		addressData.setTown(warehouseAddress.getTown());

		if (Objects.nonNull(warehouseAddress.getRegion()) && Objects.nonNull(warehouseAddress.getRegion().getIsocode()))
		{
			final RegionData regionData = new RegionData();
			regionData.setIsocodeShort(warehouseAddress.getRegion().getIsocodeShort());
			addressData.setRegion(regionData);
		}

		if (Objects.nonNull(warehouseAddress.getCountry()) && Objects.nonNull(warehouseAddress.getCountry().getIsocode()))
		{
			final CountryData countryData = new CountryData();
			countryData.setIsocode(warehouseAddress.getCountry().getIsocode());
			addressData.setCountry(countryData);
		}

		addressData.setPostalCode(sameDayPostalCode);
		fedExLocationData.setAddress(addressData);
		fedExPickupDetailData.setLocalTimeZone(sameDayTimeZone);
	}


	/**
	 * method will be used to get the operation start and end time
	 *
	 * @param deliveryMode
	 * @return
	 * @throws ParseException
	 */
	private long getStartTimeAndEndTime(final DeliveryModeModel deliveryMode, final int position)
	{
		Date operationCompletedTime = new Date();
		final String deliveryWindow = ((ZoneDeliveryModeModel) deliveryMode).getName();
		if (StringUtils.isNotBlank(deliveryWindow))
		{
			final DateFormat sdf = new SimpleDateFormat(BlintegrationConstants.SIMPLE_DATE_FORMAT);

			final String[] split = deliveryWindow.split(StringUtils.SPACE);
			final String hoursOfOperation = split[position];
			try
			{
				operationCompletedTime = sdf.parse(hoursOfOperation);
			}
			catch (final ParseException exception)
			{
				BlLogger.logMessage(LOG, Level.ERROR, "Error while executing getStartTimeAndEndTime method", exception.getMessage());
			}
		}
		return operationCompletedTime.getTime();
	}

}
