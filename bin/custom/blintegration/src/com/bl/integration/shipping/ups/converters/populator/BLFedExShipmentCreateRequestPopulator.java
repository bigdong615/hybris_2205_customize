/**
 *
 */
package com.bl.integration.shipping.ups.converters.populator;

import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Objects;

import org.apache.axis.types.NonNegativeInteger;
import org.apache.axis.types.PositiveInteger;
import org.springframework.beans.factory.annotation.Value;

import com.bl.core.model.OptimizedShippingMethodModel;
import com.bl.integration.constants.BlintegrationConstants;
import com.fedex.ship.stub.Address;
import com.fedex.ship.stub.ClientDetail;
import com.fedex.ship.stub.Contact;
import com.fedex.ship.stub.CustomerReference;
import com.fedex.ship.stub.CustomerReferenceType;
import com.fedex.ship.stub.Dimensions;
import com.fedex.ship.stub.DropoffType;
import com.fedex.ship.stub.LabelFormatType;
import com.fedex.ship.stub.LabelPrintingOrientationType;
import com.fedex.ship.stub.LabelSpecification;
import com.fedex.ship.stub.LinearUnits;
import com.fedex.ship.stub.Party;
import com.fedex.ship.stub.Payment;
import com.fedex.ship.stub.PaymentType;
import com.fedex.ship.stub.Payor;
import com.fedex.ship.stub.ProcessShipmentRequest;
import com.fedex.ship.stub.RequestedPackageLineItem;
import com.fedex.ship.stub.RequestedShipment;
import com.fedex.ship.stub.ShippingDocumentImageType;
import com.fedex.ship.stub.TransactionDetail;
import com.fedex.ship.stub.VersionId;
import com.fedex.ship.stub.WebAuthenticationCredential;
import com.fedex.ship.stub.WebAuthenticationDetail;
import com.fedex.ship.stub.Weight;
import com.fedex.ship.stub.WeightUnits;


/**
 * this class is responsible to populate request data for fedEx
 *
 * @author Aditi Sharma
 *
 */
public class BLFedExShipmentCreateRequestPopulator
{
	@Value("${blintegration.fedex.shipper.account.number}")
	private static String fedExAccountNumber;

	@Value("${blintegration.fedex.shipper.meter.number}")
	private static String fedExMeterNumber;

	@Value("${blintegration.fedex.api.key}")
	private static String fedExApiKey;

	@Value("${blintegration.fedex.shipment.password}")
	private static String fedExapiPassword;

	/**
	 * This method is used to create fedEx shipment
	 * @param packagingInfo
	 * @param packageCount
	 * @param sequenceNumber
	 * @return
	 */
	public ProcessShipmentRequest createFedExShipmentRequest(final PackagingInfoModel packagingInfo, final int packageCount,
			final String sequenceNumber, final OptimizedShippingMethodModel optimizedShippingMethod)
	{
		final ConsignmentModel consignment = packagingInfo.getConsignment();
		final ProcessShipmentRequest processShipmentRequest = new ProcessShipmentRequest(); // Build a request object

		createClientDetails(processShipmentRequest);

		// Create TransactionDetail for FedEx Shipment
		final TransactionDetail transactionDetail = new TransactionDetail();
		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(consignment.getOrder().getCode()).append(BlintegrationConstants.HYPHEN).append(BlintegrationConstants.OUT_BOUND_LABEL).append(BlintegrationConstants.HYPHEN).append(System.currentTimeMillis());
		transactionDetail.setCustomerTransactionId(stringBuilder.toString()); //
		processShipmentRequest.setTransactionDetail(transactionDetail);

		//Create RequestedShipment for FedEx Shipment
		final RequestedShipment requestedShipment = new RequestedShipment();

		createRequestedShipmentData(packagingInfo, sequenceNumber, consignment, requestedShipment, null, optimizedShippingMethod);
		//
		processShipmentRequest.setRequestedShipment(requestedShipment);
		//
		return processShipmentRequest;
	}

	/**
	 * This method is used to create return shipment for FedEx
	 * @param packagingInfo
	 * @param packageCount
	 * @param sequenceNumber
	 * @param warehouseModel
	 * @return
	 */
	public ProcessShipmentRequest createFedExReturnShipmentRequest(final PackagingInfoModel packagingInfo,
			final int packageCount, final String sequenceNumber, final WarehouseModel warehouseModel,
			final OptimizedShippingMethodModel optimizedShippingMethod)
	{
		final ConsignmentModel consignment = packagingInfo.getConsignment();
		final ProcessShipmentRequest processShipmentRequest = new ProcessShipmentRequest(); // Build a request object

		createClientDetails(processShipmentRequest);

		// Create TransactionDetail for FedEx Shipment
		final TransactionDetail transactionDetail = new TransactionDetail();
		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(consignment.getOrder().getCode()).append(BlintegrationConstants.HYPHEN).append(BlintegrationConstants.IN_BOUND_LABEL).append(BlintegrationConstants.HYPHEN).append(System.currentTimeMillis());
		transactionDetail.setCustomerTransactionId(stringBuilder.toString());
		processShipmentRequest.setTransactionDetail(transactionDetail);

		//Create RequestedShipment for FedEx Shipment
		final RequestedShipment requestedShipment = new RequestedShipment();

		createRequestedShipmentData(packagingInfo, sequenceNumber, consignment, requestedShipment, warehouseModel,
				optimizedShippingMethod);
		//
		processShipmentRequest.setRequestedShipment(requestedShipment);
		//
		return processShipmentRequest;
	}

	/**
	 * This method is used to create client details for FedEx Shipment
	 * @param processShipmentRequest
	 */
	private void createClientDetails(final ProcessShipmentRequest processShipmentRequest)
	{
		// Create Client Detail for FedEx Shipment
		processShipmentRequest.setClientDetail(createClientDetail());

		// Create WebAuthentication Detail for FedEx Shipment
		processShipmentRequest.setWebAuthenticationDetail(createWebAuthenticationDetail());

		// Create VersionId for FedEx Shipment
		final VersionId versionId = new VersionId(BlintegrationConstants.FEDEX_SERVICE_ID, 28, 0, 0);
		processShipmentRequest.setVersion(versionId);
	}

	/**
	 * This method is used to create requested shipment data for FedEx
	 * @param packagingInfo
	 * @param sequenceNumber
	 * @param consignment
	 * @param requestedShipment
	 */
	private void createRequestedShipmentData(final PackagingInfoModel packagingInfo, final String sequenceNumber,
			final ConsignmentModel consignment, final RequestedShipment requestedShipment, final WarehouseModel stateWarehouse,
			final OptimizedShippingMethodModel optimizedShippingMethod)
	{
		//consignment.getOptimizedShippingType().getServiceTypeDesc();
		requestedShipment.setShipTimestamp(getShipTimeStamp());
		setDropoffTypeOnRequestedShipment(requestedShipment);


		requestedShipment.setServiceType(optimizedShippingMethod.getServiceTypeCode());
		//requestedShipment.setServiceType(BlintegrationConstants.FEDEX_SERVICE_TYPE);
		//setServiceTypeOnRequestedShipment(consignment, requestedShipment);

		requestedShipment.setPackagingType(BlintegrationConstants.FEDEX_PACKAGING_TYPE);
		if (stateWarehouse == null)
		{
			requestedShipment.setShipper(addShipper(consignment, null));
			requestedShipment.setRecipient(addRecipient(consignment));
		}
		else
		{
			requestedShipment.setShipper(addRecipient(consignment));
			requestedShipment.setRecipient(addShipper(consignment, stateWarehouse));
		}
		requestedShipment.setShippingChargesPayment(addShippingChargesPayment());

		if (sequenceNumber.equals(BlintegrationConstants.ONE))
		{
			requestedShipment.setRequestedPackageLineItems(new RequestedPackageLineItem[]
			{ addRequestedPackageLineItem(packagingInfo) });
		}
		else
		{
			final RequestedPackageLineItem childPackageLineItem = addRequestedPackageLineItem(packagingInfo);
			childPackageLineItem.setSequenceNumber(new PositiveInteger(BlintegrationConstants.TWO));
			childPackageLineItem.setWeight(addPackageWeight(packagingInfo));
			childPackageLineItem.setDimensions(addPackageDimensions(packagingInfo));
			requestedShipment.setRequestedPackageLineItems(new RequestedPackageLineItem[]
			{ childPackageLineItem });
		}
		requestedShipment.setLabelSpecification(addLabelSpecification());
	}

	/**
	 * This method is used to set ServiceType on FedEx Shipment Request
	 *
	 * @param consignment
	 * @param requestedShipment
	 */
	/*
	 * private void setServiceTypeOnRequestedShipment(final ConsignmentModel consignment, final RequestedShipment
	 * requestedShipment) { final String serviceMethod = consignment.getOptimizedShippingType().getCode();
	 *
	 * if (StringUtils.isNotEmpty(serviceMethod)) { if
	 * (serviceMethod.equals(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode())) {
	 * requestedShipment.setServiceType(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode()); } else if
	 * (serviceMethod.equals(OptimizedShippingMethodEnum.TWO_DAY_AIR.getCode())) {
	 * requestedShipment.setServiceType(OptimizedShippingMethodEnum.TWO_DAY_AIR.getCode()); } else if
	 * (serviceMethod.equals(OptimizedShippingMethodEnum.TWO_DAY_AIR_AM.getCode())) {
	 * requestedShipment.setServiceType(OptimizedShippingMethodEnum.TWO_DAY_AIR_AM.getCode()); } else if
	 * (serviceMethod.equals(OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode())) {
	 * requestedShipment.setServiceType(OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode()); } else if
	 * (serviceMethod.equals(OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode())) {
	 * requestedShipment.setServiceType(OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode()); } else if
	 * (serviceMethod.equals(OptimizedShippingMethodEnum.NEXT_DAY_AIR.getCode())) {
	 * requestedShipment.setServiceType(OptimizedShippingMethodEnum.NEXT_DAY_AIR.getCode()); } else if
	 * (serviceMethod.equals(OptimizedShippingMethodEnum.NEXT_DAY_AIR_AM.getCode())) {
	 * requestedShipment.setServiceType(OptimizedShippingMethodEnum.NEXT_DAY_AIR_AM.getCode()); } else if
	 * (serviceMethod.equals(OptimizedShippingMethodEnum.NEXT_DAY_AIR_SAT.getCode())) {
	 * requestedShipment.setServiceType(OptimizedShippingMethodEnum.NEXT_DAY_AIR_SAT.getCode()); } } }
	 */



	/**
	 * This method is used to set DropOff Type on FedEx Shipment Request
	 *
	 * @param requestedShipment
	 */
	private void setDropoffTypeOnRequestedShipment(final RequestedShipment requestedShipment)
	{
		final Calendar now = Calendar.getInstance();
		if (now.get(Calendar.HOUR_OF_DAY) > BlintegrationConstants.SIXTEEN
				&& (now.get(Calendar.HOUR_OF_DAY) < BlintegrationConstants.EIGHTEEN
						|| (now.get(Calendar.HOUR_OF_DAY) == BlintegrationConstants.EIGHTEEN
								&& now.get(Calendar.MINUTE) < BlintegrationConstants.THIRTY_ONE)))
		{
			// Between 4:00 and 6:30 pm, business service center
			requestedShipment.setDropoffType(DropoffType.BUSINESS_SERVICE_CENTER);
		}
		else if (now.get(Calendar.HOUR_OF_DAY) > BlintegrationConstants.SIXTEEN
				&& (now.get(Calendar.HOUR_OF_DAY) < BlintegrationConstants.TWENTY
						|| (now.get(Calendar.HOUR_OF_DAY) == BlintegrationConstants.TWENTY
								&& now.get(Calendar.MINUTE) < BlintegrationConstants.THIRTY_ONE)))
		{
			// Between 6:30 and 8:30 pm, station
			requestedShipment.setDropoffType(DropoffType.STATION);
		}
		else
		{
			// Otherwise, pick up
			requestedShipment.setDropoffType(DropoffType.REGULAR_PICKUP);
		}
	}

	/**
	 * This method is used to get value for ship timestamp
	 *
	 * @return Calendar
	 */
	private Calendar getShipTimeStamp()
	{
		final Calendar sevenhoursearlier = Calendar.getInstance();
		sevenhoursearlier.add(Calendar.HOUR_OF_DAY, BlintegrationConstants.MINUS_SEVEN);
		return sevenhoursearlier;
	}

	/**
	 * This method is used to set package weight on FedEx Shipment Request
	 *
	 * @param packagingInfo
	 * @return
	 */
	private static Weight addPackageWeight(final PackagingInfoModel packagingInfo)
	{
		final Weight weight = new Weight();
		weight.setUnits(WeightUnits.LB);
		weight.setValue(new BigDecimal(packagingInfo.getGrossWeight()));
		return weight;
	}

	/**
	 * This method is used to set package dimensions on FedEx Shipment Request
	 *
	 * @param packagingInfo
	 * @return
	 */
	private static Dimensions addPackageDimensions(final PackagingInfoModel packagingInfo)
	{
		final Dimensions dimensions = new Dimensions();
		dimensions.setLength(new NonNegativeInteger(packagingInfo.getLength()));
		dimensions.setHeight(new NonNegativeInteger(packagingInfo.getHeight()));
		dimensions.setWidth(new NonNegativeInteger(packagingInfo.getWidth()));
		dimensions.setUnits(LinearUnits.IN);
		return dimensions;
	}

	/**
	 * This method is used to add shipper for FedEx Shipment Request
	 *
	 * @param consignment
	 * @return Party
	 */
	private static Party addShipper(final ConsignmentModel consignment, final WarehouseModel stateWarehouse)
	{
		AddressModel warehouseAddress = new AddressModel();
		if (Objects.isNull(stateWarehouse) && Objects.nonNull(consignment.getWarehouse())
				&& Objects.nonNull(consignment.getWarehouse().getPointsOfService()))
		{
			warehouseAddress = getWarehouseAddress(consignment.getWarehouse());
		}
		else if (Objects.nonNull(stateWarehouse) && Objects.nonNull(stateWarehouse.getPointsOfService()))
		{
			warehouseAddress = getWarehouseAddress(stateWarehouse);
		}

		final Party shipperParty = new Party(); // Sender information
		final Contact shipperContact = new Contact();
		shipperContact.setPersonName(warehouseAddress.getFirstname());
		shipperContact.setCompanyName(warehouseAddress.getCompany());
		shipperContact.setPhoneNumber(warehouseAddress.getPhone1());

		final Address shipperAddress = new Address();
		shipperAddress.setStreetLines(new String[]
		{ warehouseAddress.getLine1() });
		shipperAddress.setCity(warehouseAddress.getTown());
		shipperAddress.setStateOrProvinceCode(warehouseAddress.getRegion().getIsocodeShort());
		shipperAddress.setPostalCode(warehouseAddress.getPostalcode());
		shipperAddress.setCountryCode(warehouseAddress.getCountry().getIsocode());
		shipperParty.setContact(shipperContact);
		shipperParty.setAddress(shipperAddress);
		return shipperParty;
	}

	/**
	 * This method is used to add recipient for FedEx Shipment Request
	 *
	 * @param consignment
	 * @return
	 */
	private static Party addRecipient(final ConsignmentModel consignment)
	{
		final AddressModel recipientAddressModel = consignment.getOrder().getDeliveryAddress();
		final Party recipientParty = new Party(); // Recipient information
		final Contact recipientContact = new Contact();
		recipientContact.setPersonName(recipientAddressModel.getFirstname());
		recipientContact.setCompanyName(recipientAddressModel.getCompany());
		recipientContact.setPhoneNumber(recipientAddressModel.getPhone1());
		final Address recipientAddress = new Address();
		recipientAddress.setStreetLines(new String[]
		{ recipientAddressModel.getLine1() });
		recipientAddress.setCity(recipientAddressModel.getTown());
		recipientAddress.setStateOrProvinceCode(recipientAddressModel.getRegion().getIsocodeShort());
		recipientAddress.setPostalCode(recipientAddressModel.getPostalcode());
		recipientAddress.setCountryCode(recipientAddressModel.getCountry().getIsocode());
		//recipientAddress.setResidential(Boolean.valueOf(false));
		recipientParty.setContact(recipientContact);
		recipientParty.setAddress(recipientAddress);
		return recipientParty;
	}

	/**
	 * this method is used to get warehouse address
	 *
	 * @param warehouse
	 * @return AddressModel
	 */
	private static AddressModel getWarehouseAddress(final WarehouseModel warehouse)
	{
		return warehouse.getPointsOfService().iterator().next().getAddress();
	}

	/**
	 * This method is used to set shipping charges payment on FedEx Shipment Request
	 *
	 * @return
	 */
	private static Payment addShippingChargesPayment()
	{
		final Payment payment = new Payment(); // Payment information
		payment.setPaymentType(PaymentType.SENDER);
		final Payor payor = new Payor();
		final Party responsibleParty = new Party();
		responsibleParty.setAccountNumber(Config.getParameter(BlintegrationConstants.FEDEX_SHIPPER_ACCOUNT_NUMBER));
		final Address responsiblePartyAddress = new Address();
		responsiblePartyAddress.setCountryCode(BlintegrationConstants.FEDEX_COUNTRY_CODE);
		responsibleParty.setAddress(responsiblePartyAddress);
		responsibleParty.setContact(new Contact());
		payor.setResponsibleParty(responsibleParty);
		payment.setPayor(payor);
		return payment;
	}

	/**
	 * This method is used to set package details on FedEx Shipment Request
	 *
	 * @param packagingInfo
	 * @return
	 */
	private static RequestedPackageLineItem addRequestedPackageLineItem(final PackagingInfoModel packagingInfo)
	{
		final RequestedPackageLineItem requestedPackageLineItem = new RequestedPackageLineItem();
		requestedPackageLineItem.setSequenceNumber(new PositiveInteger(BlintegrationConstants.ONE));
		requestedPackageLineItem.setGroupPackageCount(new NonNegativeInteger(BlintegrationConstants.ONE));
		requestedPackageLineItem.setWeight(addPackageWeight(packagingInfo));
		requestedPackageLineItem.setDimensions(addPackageDimensions(packagingInfo));
		requestedPackageLineItem.setCustomerReferences(new CustomerReference[]
		{ addCustomerReference(CustomerReferenceType.INVOICE_NUMBER.getValue(),
				packagingInfo.getConsignment().getOrder().getCode()) });
		return requestedPackageLineItem;
	}

	/**
	 * This method is used to set customer reference on FedEx Shipment Request
	 *
	 * @param customerReferenceType
	 * @param customerReferenceValue
	 * @return
	 */
	private static CustomerReference addCustomerReference(final String customerReferenceType, final String customerReferenceValue)
	{
		final CustomerReference customerReference = new CustomerReference();
		customerReference.setCustomerReferenceType(CustomerReferenceType.fromString(customerReferenceType));
		customerReference.setValue(customerReferenceValue);
		return customerReference;
	}

	/**
	 * This method is used to set label on FedEx Shipment Request
	 *
	 * @return
	 */
	private static LabelSpecification addLabelSpecification()
	{

		final LabelSpecification labelSpecification = new LabelSpecification(); // Label specification
		labelSpecification.setImageType(ShippingDocumentImageType.ZPLII);// Image types PDF, PNG, DPL, ...
		labelSpecification.setLabelFormatType(LabelFormatType.COMMON2D); //LABEL_DATA_ONLY, COMMON2D
		labelSpecification.setLabelPrintingOrientation(LabelPrintingOrientationType.BOTTOM_EDGE_OF_TEXT_FIRST);
		//labelSpecification.setLabelStockType(LabelStockType.value1);
		return labelSpecification;
	}

	/**
	 * This method is used to create client details for FedEx shipment
	 *
	 * @return ClientDetail
	 */
	private static ClientDetail createClientDetail()
	{
		final ClientDetail clientDetail = new ClientDetail();
		clientDetail.setAccountNumber(Config.getParameter(BlintegrationConstants.FEDEX_SHIPPER_ACCOUNT_NUMBER));
		clientDetail.setMeterNumber(Config.getParameter(BlintegrationConstants.FEDEX_SHIPPER_METER_NUMBER));
		return clientDetail;
	}

	/**
	 * This method is used to create Web Authentication Detail for FedEx Shipment
	 *
	 * @return WebAuthenticationDetail
	 */
	private WebAuthenticationDetail createWebAuthenticationDetail()
	{
		final WebAuthenticationCredential wac = new WebAuthenticationCredential();

		wac.setKey(Config.getParameter(BlintegrationConstants.FEDEX_SHIPPER_API_KEY));
		wac.setPassword(Config.getParameter(BlintegrationConstants.FEDEX_SHIPPER_PASSWORD));
		return new WebAuthenticationDetail(wac, wac);
	}
}
