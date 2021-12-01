/**
 *
 */
package com.bl.integration.shipping.ups.converters.populator;

import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.io.File;
import java.io.FileOutputStream;
import java.math.BigDecimal;
import java.util.Calendar;

import org.apache.axis.types.NonNegativeInteger;
import org.apache.axis.types.PositiveInteger;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;

import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.integration.constants.BlintegrationConstants;
import com.fedex.ship.stub.Address;
import com.fedex.ship.stub.AssociatedShipmentDetail;
import com.fedex.ship.stub.ClientDetail;
import com.fedex.ship.stub.CodCollectionType;
import com.fedex.ship.stub.CodDetail;
import com.fedex.ship.stub.CodReturnPackageDetail;
import com.fedex.ship.stub.CompletedPackageDetail;
import com.fedex.ship.stub.CompletedShipmentDetail;
import com.fedex.ship.stub.Contact;
import com.fedex.ship.stub.CustomerReference;
import com.fedex.ship.stub.CustomerReferenceType;
import com.fedex.ship.stub.Dimensions;
import com.fedex.ship.stub.DropoffType;
import com.fedex.ship.stub.FreightBaseCharge;
import com.fedex.ship.stub.FreightRateDetail;
import com.fedex.ship.stub.FreightRateNotation;
import com.fedex.ship.stub.LabelFormatType;
import com.fedex.ship.stub.LabelSpecification;
import com.fedex.ship.stub.LinearUnits;
import com.fedex.ship.stub.Money;
import com.fedex.ship.stub.Notification;
import com.fedex.ship.stub.NotificationSeverityType;
import com.fedex.ship.stub.PackageOperationalDetail;
import com.fedex.ship.stub.PackageRateDetail;
import com.fedex.ship.stub.PackageRating;
import com.fedex.ship.stub.Party;
import com.fedex.ship.stub.Payment;
import com.fedex.ship.stub.PaymentType;
import com.fedex.ship.stub.Payor;
import com.fedex.ship.stub.ProcessShipmentReply;
import com.fedex.ship.stub.ProcessShipmentRequest;
import com.fedex.ship.stub.RequestedPackageLineItem;
import com.fedex.ship.stub.RequestedShipment;
import com.fedex.ship.stub.ShipServiceLocator;
import com.fedex.ship.stub.ShipmentOperationalDetail;
import com.fedex.ship.stub.ShipmentRateDetail;
import com.fedex.ship.stub.ShipmentRating;
import com.fedex.ship.stub.ShipmentSpecialServicesRequested;
import com.fedex.ship.stub.ShippingDocument;
import com.fedex.ship.stub.ShippingDocumentImageType;
import com.fedex.ship.stub.ShippingDocumentPart;
import com.fedex.ship.stub.Surcharge;
import com.fedex.ship.stub.TrackingId;
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

	public ProcessShipmentRequest createFedExShipmentRequest(final PackagingInfoModel packagingInfo, final int pkgCount,
			final String sequenceNumber)
	{
		final ConsignmentModel consignment = packagingInfo.getConsignment();
		final ProcessShipmentRequest processShipmentRequest = new ProcessShipmentRequest(); // Build a request object

		// Create Client Detail for FedEx Shipment
		processShipmentRequest.setClientDetail(createClientDetail());

		// Create WebAuthentication Detail for FedEx Shipment
		processShipmentRequest.setWebAuthenticationDetail(createWebAuthenticationDetail());

		// Create TransactionDetail for FedEx Shipment
		final TransactionDetail transactionDetail = new TransactionDetail();
		final String masterOrChild = (sequenceNumber.equals(BlintegrationConstants.ONE) ? BlintegrationConstants.FEDEX_MASTER
				: BlintegrationConstants.FEDEX_CHILD);
		transactionDetail.setCustomerTransactionId(consignment.getOrder().getCode() + BlintegrationConstants.HYPHEN
				+ BlintegrationConstants.IN_BOUND_OR_OUT_BOUND + BlintegrationConstants.HYPHEN + System.currentTimeMillis()); //
		processShipmentRequest.setTransactionDetail(transactionDetail);

		// Create VersionId for FedEx Shipment
		final VersionId versionId = new VersionId(BlintegrationConstants.FEDEX_SERVICE_ID, 28, 0, 0);
		processShipmentRequest.setVersion(versionId);

		//Create RequestedShipment for FedEx Shipment
		final RequestedShipment requestedShipment = new RequestedShipment();

		requestedShipment.setShipTimestamp(getShipTimeStamp());
		setDropoffTypeOnRequestedShipment(requestedShipment);
		requestedShipment.setServiceType(BlintegrationConstants.FEDEX_SERVICE_TYPE);
		//setServiceTypeOnRequestedShipment(consignment, requestedShipment);

		requestedShipment.setPackagingType(BlintegrationConstants.FEDEX_PACKAGING_TYPE);
		requestedShipment.setShipper(addShipper(consignment));
		requestedShipment.setRecipient(addRecipient(consignment));
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
		//
		processShipmentRequest.setRequestedShipment(requestedShipment);
		//
		return processShipmentRequest;
	}

	/**
	 * This method is used to set ServiceType on FedEx Shipment Request
	 *
	 * @param consignment
	 * @param requestedShipment
	 */
	private void setServiceTypeOnRequestedShipment(final ConsignmentModel consignment, final RequestedShipment requestedShipment)
	{
		final String serviceMethod = consignment.getOptimizedShippingType().getCode();

		if (StringUtils.isNotEmpty(serviceMethod))
		{
			if (serviceMethod.equals(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode()))
			{
				requestedShipment.setServiceType(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode());
			}
			else if (serviceMethod.equals(OptimizedShippingMethodEnum.TWO_DAY_AIR.getCode()))
			{
				requestedShipment.setServiceType(OptimizedShippingMethodEnum.TWO_DAY_AIR.getCode());
			}
			else if (serviceMethod.equals(OptimizedShippingMethodEnum.TWO_DAY_AIR_AM.getCode()))
			{
				requestedShipment.setServiceType(OptimizedShippingMethodEnum.TWO_DAY_AIR_AM.getCode());
			}
			else if (serviceMethod.equals(OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode()))
			{
				requestedShipment.setServiceType(OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode());
			}
			else if (serviceMethod.equals(OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode()))
			{
				requestedShipment.setServiceType(OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode());
			}
			else if (serviceMethod.equals(OptimizedShippingMethodEnum.NEXT_DAY_AIR.getCode()))
			{
				requestedShipment.setServiceType(OptimizedShippingMethodEnum.NEXT_DAY_AIR.getCode());
			}
			else if (serviceMethod.equals(OptimizedShippingMethodEnum.NEXT_DAY_AIR_AM.getCode()))
			{
				requestedShipment.setServiceType(OptimizedShippingMethodEnum.NEXT_DAY_AIR_AM.getCode());
			}
			else if (serviceMethod.equals(OptimizedShippingMethodEnum.NEXT_DAY_AIR_SAT.getCode()))
			{
				requestedShipment.setServiceType(OptimizedShippingMethodEnum.NEXT_DAY_AIR_SAT.getCode());
			}
		}
	}

	/**
	 * This method is used to set DropOff Type on FedEx Shipment Request
	 *
	 * @param requestedShipment
	 */
	private void setDropoffTypeOnRequestedShipment(final RequestedShipment requestedShipment)
	{
		final Calendar now = Calendar.getInstance();
		if (now.get(Calendar.HOUR_OF_DAY) > 16
				&& (now.get(Calendar.HOUR_OF_DAY) < 18 || (now.get(Calendar.HOUR_OF_DAY) == 18 && now.get(Calendar.MINUTE) < 31)))
		{
			// Between 4:00 and 6:30 pm, business service center
			requestedShipment.setDropoffType(DropoffType.BUSINESS_SERVICE_CENTER);
		}
		else if (now.get(Calendar.HOUR_OF_DAY) > 16
				&& (now.get(Calendar.HOUR_OF_DAY) < 20 || (now.get(Calendar.HOUR_OF_DAY) == 20 && now.get(Calendar.MINUTE) < 31)))
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
	 * @return Calendar
	 */
	private Calendar getShipTimeStamp()
	{
		final Calendar sevenhoursearlier = Calendar.getInstance();
		sevenhoursearlier.add(Calendar.HOUR_OF_DAY, -7);
		return sevenhoursearlier;
	}

	/**
	 * This method is used to set package weight on FedEx Shipment Request
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
	 * @param consignment
	 * @return Party
	 */
	private static Party addShipper(final ConsignmentModel consignment)
	{
		final AddressModel warehouseAddress = getWarehouseAddress(consignment.getWarehouse());
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
	 * @return
	 */
	private static Payment addShippingChargesPayment()
	{
		final Payment payment = new Payment(); // Payment information
		payment.setPaymentType(PaymentType.SENDER);
		final Payor payor = new Payor();
		final Party responsibleParty = new Party();
		responsibleParty.setAccountNumber(fedExAccountNumber);
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
	 * @return
	 */
	private static LabelSpecification addLabelSpecification()
	{

		final LabelSpecification labelSpecification = new LabelSpecification(); // Label specification
		labelSpecification.setImageType(ShippingDocumentImageType.PNG);// Image types PDF, PNG, DPL, ...
		labelSpecification.setLabelFormatType(LabelFormatType.COMMON2D); //LABEL_DATA_ONLY, COMMON2D


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
		clientDetail.setAccountNumber(fedExAccountNumber);
		clientDetail.setMeterNumber(fedExMeterNumber);
		return clientDetail;
	}

	/**
	 * This method is used to create Web Authentication Detail for FedEx Shipment
	 *
	 * @return WebAuthenticationDetail
	 */
	private WebAuthenticationDetail createWebAuthenticationDetail() {
		WebAuthenticationCredential wac = new WebAuthenticationCredential();

		wac.setKey(fedExApiKey);
		wac.setPassword(fedExapiPassword);
		return new WebAuthenticationDetail(wac, wac);
	}

	private static void updateEndPoint(final ShipServiceLocator serviceLocator)
	{
		final String endPoint = System.getProperty("endPoint");
		if (endPoint != null)
		{
			serviceLocator.setShipServicePortEndpointAddress(endPoint);
		}
	}
}
