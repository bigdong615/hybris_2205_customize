/**
 *
 */
package com.bl.integration.populators;

import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;

import com.bl.facades.shipment.data.DimensionsTypeData;
import com.bl.facades.shipment.data.PackageTypeData;
import com.bl.facades.shipment.data.PackageWeightTypeData;
import com.bl.facades.shipment.data.ShipFromData;
import com.bl.facades.shipment.data.ShipToData;
import com.bl.facades.shipment.data.ShipmentData;
import com.bl.facades.shipment.data.ShipmentPhoneData;
import com.bl.facades.shipment.data.ShipperData;
import com.bl.facades.shipment.data.UpsPaymentInformation;
import com.bl.facades.shipment.data.UpsShipmentServiceData;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.integration.constants.BlintegrationConstants;


/**
 * @author Aditi Sharma
 *
 */
public class BLUpsShippingDataPopulator
{
	@Value("${blintegration.ups.shipment.shipper.number.ca}")
	private String shipperNumberCA;

	@Value("${blintegration.ups.shipment.shipper.number.ma}")
	private String shipperNumberMA;

	@Value("${blintegration.ups.shipment.shipper.phone.number}")
	private String shipperPhoneNumber;

	@Value("${blintegration.ups.shipment.shipper.name}")
	private String shipperName;

	@Value("${blintegration.ups.shipment.shipper.attension.name}")
	private String shipperAttensionName;

	@Value("${blintegration.ups.shipment.payment.information.type}")
	private String paymentInfoType;

	@Value("${blintegration.ups.shipment.packaging.weight.code}")
	private String packageWeightCode;

	@Value("${blintegration.ups.shipment.packaging.weight.description}")
	private String packageWeightDescription;

	@Value("${blintegration.ups.shipment.dimension.type.code}")
	private String dimensionTypeCode;

	@Value("${blintegration.ups.shipment.dimension.type.description}")
	private String dimensionTypeDescription;

	@Value("${blintegration.ups.shipment.return.service.code}")
	private String returnServiceCode;

	@Value("${blintegration.ups.shipment.return.service.description}")
	private String returnServiceDescription;

	@Resource(name = "addressConverter")
	private Converter<AddressModel, AddressData> addressConverter;

	public UpsShippingRequestData populateUPSShipmentRequest(final PackagingInfoModel packagingInfo)
	{
		final WarehouseModel warehouse = packagingInfo.getConsignment().getWarehouse();
		final UpsShippingRequestData upsRequestData = new UpsShippingRequestData();
		AddressModel shipperAddress = new AddressModel();

		final ShipmentData shipmentData = new ShipmentData();

		/** Creating Shipper Data **/

		if (warehouse != null && warehouse.getPointsOfService() != null)
		{
			shipperAddress = warehouse.getPointsOfService().iterator().next().getAddress();
		}
		final AddressData addressData = addressConverter.convert(shipperAddress);

		final ShipperData shipperData = new ShipperData();

		final ShipmentPhoneData shipperPhone = new ShipmentPhoneData();
		shipperPhone.setNumber(addressData.getPhone());

		final AddressData shipperAddressData = new AddressData();

		if (warehouse != null && warehouse.getAccountNumber() != null)
		{
			shipperData.setShipperNumber(warehouse.getAccountNumber());
		}
		shipperAddressData.setLine1(addressData.getLine1());
		shipperAddressData.setLine2(addressData.getLine2());
		shipperAddressData.setTown(addressData.getTown());
		shipperAddressData.setPhone(addressData.getPhone());

		if (addressData.getCountry() != null && addressData.getCountry().getIsocode() != null)
		{
			final CountryData countryData = new CountryData();
			countryData.setIsocode(addressData.getCountry().getIsocode());
			shipperAddressData.setCountry(countryData);
		}

		shipperAddressData.setPostalCode(addressData.getPostalCode());

		final RegionData regionData = new RegionData();
		regionData.setIsocodeShort(addressData.getRegion().getIsocodeShort());
		shipperAddressData.setRegion(regionData);
		shipperData.setName(shipperName);
		shipperData.setAttentionName(shipperAttensionName);
		shipperData.setPhone(shipperPhone);
		shipperData.setAddress(shipperAddressData);

		/** Creating ShipFrom Data **/
		final ShipFromData shipFromData = new ShipFromData();
		shipFromData.setName(shipperName);
		shipFromData.setAttentionName(shipperAttensionName);
		shipFromData.setPhone(shipperPhone);
		shipFromData.setAddress(shipperAddressData);

		/** Creating ShipTo Data **/
		final AddressData shipToAddress = addressConverter.convert(packagingInfo.getConsignment().getOrder().getDeliveryAddress());

		final AddressData shipToAddressData = new AddressData();
		if (StringUtils.isNotEmpty(shipToAddress.getFirstName()))
		{
			shipToAddressData.setFirstName(shipToAddress.getFirstName());
		}
		shipToAddressData.setLastName(shipToAddress.getLastName());
		shipToAddressData.setLine1(shipToAddress.getLine1());
		shipToAddressData.setLine2(shipToAddress.getLine2());
		shipToAddressData.setTown(shipToAddress.getTown());
		shipToAddressData.setPhone(shipToAddress.getPhone());

		if (shipToAddress.getRegion() != null && shipToAddress.getRegion().getIsocodeShort() != null)
		{
			final RegionData shipToRegionData = new RegionData();
			shipToRegionData.setIsocodeShort(shipToAddress.getRegion().getIsocodeShort());
			shipToAddressData.setRegion(shipToRegionData);
		}

		if (shipToAddress.getCountry() != null && shipToAddress.getCountry().getIsocode() != null)
		{
			final CountryData shipToCountryData = new CountryData();
			shipToCountryData.setIsocode(shipToAddress.getCountry().getIsocode());
			shipToAddressData.setCountry(shipToCountryData);
		}
		shipToAddressData.setPostalCode(shipToAddress.getPostalCode());

		final ShipmentPhoneData shipToPhone = new ShipmentPhoneData();
		shipToPhone.setNumber(shipToAddress.getPhone());

		final ShipToData shipToData = new ShipToData();
		if (shipToAddressData.getCompanyName() != null)
		{
			shipToData.setName(shipToAddressData.getCompanyName());
		}
		else
		{
			shipToData.setName(shipToAddressData.getFirstName().concat(" ").concat(shipToAddressData.getLastName()));
		}
		shipToData.setAttentionName(shipToAddressData.getFirstName().concat(" ").concat(shipToAddressData.getLastName()));
		shipToData.setPhone(shipToPhone);
		shipToData.setAddress(shipToAddressData);

		/** Creating UPS Shipment Service Data **/
		final UpsShipmentServiceData upsShipmentServiceData = new UpsShipmentServiceData();

		if (packagingInfo.getConsignment().getOptimizedShippingType() != null
				&& StringUtils.isNotBlank(packagingInfo.getConsignment().getOptimizedShippingType().getCode()))
		{
			upsShipmentServiceData.setCode(packagingInfo.getConsignment().getOptimizedShippingType().getServiceTypeCode());
			upsShipmentServiceData.setDescription(packagingInfo.getConsignment().getOptimizedShippingType().getServiceTypeDesc());
		}

		/** Creating UPS Payment Data **/
		final UpsPaymentInformation upsPaymentInformation = new UpsPaymentInformation();
		upsPaymentInformation.setType(paymentInfoType);

		if (warehouse.getAccountNumber() != null)
		{
			upsPaymentInformation.setAccountNumber(warehouse.getAccountNumber());
		}

		/** Creating UPS Package Data List **/

		final List<PackageTypeData> packageDataList = new ArrayList<>();
		populatePackage(packageDataList, packagingInfo);

		shipmentData.setShipper(shipperData);
		shipmentData.setShipFrom(shipFromData);
		shipmentData.setShipTo(shipToData);
		shipmentData.setService(upsShipmentServiceData);
		shipmentData.setPaymentInformation(upsPaymentInformation);
		shipmentData.setShipmentPackage(packageDataList);

		upsRequestData.setShipment(shipmentData);

		return upsRequestData;

	}

	/**
	 * @param packageDataList
	 * @param consignmentEntries
	 */
	private void populatePackage(final List<PackageTypeData> packageDataList, final PackagingInfoModel packagingInfo)
	{
		final UpsShipmentServiceData serviceDataForPackagingType = new UpsShipmentServiceData();

		serviceDataForPackagingType.setCode(BlintegrationConstants.PACAKAGING_TYPE_CODE);
		serviceDataForPackagingType.setDescription(BlintegrationConstants.PACAKAGING_TYPE_DESCIPTION);

		final UpsShipmentServiceData serviceDataForUnit = new UpsShipmentServiceData();
		serviceDataForUnit.setCode(packageWeightCode);
		serviceDataForUnit.setDescription(packageWeightDescription);

		final PackageTypeData packageData = new PackageTypeData();
		final DimensionsTypeData dimensionsTypeData = new DimensionsTypeData();
		final PackageWeightTypeData packageWeight = new PackageWeightTypeData();
		packageWeight.setUnitOfMeasurement(serviceDataForUnit);
		packageWeight.setWeight(packagingInfo.getGrossWeight());
		packageData.setPackagingType(serviceDataForPackagingType);
		packageData.setPackageWeight(packageWeight);

		final UpsShipmentServiceData unitOfMeas = new UpsShipmentServiceData();
		unitOfMeas.setCode(dimensionTypeCode);
		unitOfMeas.setDescription(dimensionTypeDescription);

		dimensionsTypeData.setHeight(packagingInfo.getHeight());
		dimensionsTypeData.setLength(packagingInfo.getLength());
		dimensionsTypeData.setWidth(packagingInfo.getWidth());
		dimensionsTypeData.setUnitOfMeasurement(unitOfMeas);
		packageData.setDimensions(dimensionsTypeData);
		packageDataList.add(packageData);
	}


}
