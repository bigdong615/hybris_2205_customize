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
		//requestedShipment.setServiceType("PRIORITY_OVERNIGHT");
		setServiceTypeOnRequestedShipment(consignment, requestedShipment);

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
	 *
	 * @return Calendar
	 */
	private Calendar getShipTimeStamp()
	{
		final Calendar sevenhoursearlier = Calendar.getInstance();
		sevenhoursearlier.add(Calendar.HOUR_OF_DAY, -7);
		return sevenhoursearlier;
	}

	private static void writeServiceOutput(final ProcessShipmentReply reply) throws Exception
	{
		try
		{
			System.out.println(reply.getTransactionDetail().getCustomerTransactionId());
			final CompletedShipmentDetail csd = reply.getCompletedShipmentDetail();
			final String masterTrackingNumber = printMasterTrackingNumber(csd);
			printShipmentOperationalDetails(csd.getOperationalDetail());
			printShipmentRating(csd.getShipmentRating());
			final CompletedPackageDetail cpd[] = csd.getCompletedPackageDetails();
			printPackageDetails(cpd);
			saveShipmentDocumentsToFile(csd.getShipmentDocuments(), masterTrackingNumber);
			//  If Express COD shipment is requested, the COD return label is returned as an Associated Shipment.
			getAssociatedShipmentLabels(csd.getAssociatedShipments());
		}
		catch (final Exception e)
		{
			e.printStackTrace();
		}
		finally
		{
			//
		}
	}

	private static boolean isResponseOk(final NotificationSeverityType notificationSeverityType)
	{
		if (notificationSeverityType == null)
		{
			return false;
		}
		if (notificationSeverityType.equals(NotificationSeverityType.WARNING)
				|| notificationSeverityType.equals(NotificationSeverityType.NOTE)
				|| notificationSeverityType.equals(NotificationSeverityType.SUCCESS))
		{
			return true;
		}
		return false;
	}

	private static void printNotifications(final Notification[] notifications)
	{
		System.out.println("Notifications:");
		if (notifications == null || notifications.length == 0)
		{
			System.out.println("  No notifications returned");
		}
		for (int i = 0; i < notifications.length; i++)
		{
			final Notification n = notifications[i];
			System.out.print("  Notification no. " + i + ": ");
			if (n == null)
			{
				System.out.println("null");
				continue;
			}
			else
			{
				System.out.println("");
			}
			final NotificationSeverityType nst = n.getSeverity();

			System.out.println("    Severity: " + (nst == null ? "null" : nst.getValue()));
			System.out.println("    Code: " + n.getCode());
			System.out.println("    Message: " + n.getMessage());
			System.out.println("    Source: " + n.getSource());
		}
	}

	private static void printMoney(final Money money, final String description, final String space)
	{
		if (money != null)
		{
			System.out.println(space + description + ": " + money.getAmount() + " " + money.getCurrency());
		}
	}

	private static void printWeight(final Weight weight, final String description, final String space)
	{
		if (weight != null)
		{
			System.out.println(space + description + ": " + weight.getValue() + " " + weight.getUnits());
		}
	}

	private static void printString(final String value, final String description, final String space)
	{
		if (value != null)
		{
			System.out.println(space + description + ": " + value);
		}
	}


	private static Weight addPackageWeight(final PackagingInfoModel packagingInfo)
	{
		final Weight weight = new Weight();
		weight.setUnits(WeightUnits.LB);
		weight.setValue(new BigDecimal(packagingInfo.getGrossWeight()));
		return weight;
	}

	private static Dimensions addPackageDimensions(final PackagingInfoModel packagingInfo)
	{
		final Dimensions dimensions = new Dimensions();
		dimensions.setLength(new NonNegativeInteger(packagingInfo.getLength()));
		dimensions.setHeight(new NonNegativeInteger(packagingInfo.getHeight()));
		dimensions.setWidth(new NonNegativeInteger(packagingInfo.getWidth()));
		dimensions.setUnits(LinearUnits.IN);
		return dimensions;
	}

	//Shipment level reply information
	private static void printShipmentOperationalDetails(final ShipmentOperationalDetail shipmentOperationalDetail)
	{
		if (shipmentOperationalDetail != null)
		{
			System.out.println("Routing Details");
			printString(shipmentOperationalDetail.getUrsaPrefixCode(), "URSA Prefix", "  ");
			if (shipmentOperationalDetail.getCommitDay() != null)
			{
				printString(shipmentOperationalDetail.getCommitDay().getValue(), "Service Commitment", "  ");
			}
			printString(shipmentOperationalDetail.getAirportId(), "Airport Id", "  ");
			if (shipmentOperationalDetail.getDeliveryDay() != null)
			{
				printString(shipmentOperationalDetail.getDeliveryDay().getValue(), "Delivery Day", "  ");
			}
			System.out.println();
		}
	}

	private static void printShipmentRating(final ShipmentRating shipmentRating)
	{
		if (shipmentRating != null)
		{
			System.out.println("Shipment Rate Details");
			final ShipmentRateDetail[] srd = shipmentRating.getShipmentRateDetails();
			for (int j = 0; j < srd.length; j++)
			{
				System.out.println("  Rate Type: " + srd[j].getRateType().getValue());
				printWeight(srd[j].getTotalBillingWeight(), "Shipment Billing Weight", "    ");
				printMoney(srd[j].getTotalBaseCharge(), "Shipment Base Charge", "    ");
				printMoney(srd[j].getTotalNetCharge(), "Shipment Net Charge", "    ");
				printMoney(srd[j].getTotalSurcharges(), "Shipment Total Surcharge", "    ");
				if (null != srd[j].getSurcharges())
				{
					System.out.println("    Surcharge Details");
					final Surcharge[] s = srd[j].getSurcharges();
					for (int k = 0; k < s.length; k++)
					{
						printMoney(s[k].getAmount(), s[k].getSurchargeType().getValue(), "      ");
					}
				}
				printFreightDetail(srd[j].getFreightRateDetail());
				System.out.println();
			}
		}
	}

	//Package level reply information
	private static void printPackageOperationalDetails(final PackageOperationalDetail packageOperationalDetail)
	{
		if (packageOperationalDetail != null)
		{
			System.out.println("  Routing Details");
			printString(packageOperationalDetail.getAstraHandlingText(), "Astra", "    ");
			printString(packageOperationalDetail.getGroundServiceCode(), "Ground Service Code", "    ");
			System.out.println();
		}
	}

	private static void printPackageDetails(final CompletedPackageDetail[] cpd) throws Exception
	{
		if (cpd != null)
		{
			System.out.println("Package Details");
			for (int i = 0; i < cpd.length; i++)
			{ // Package details / Rating information for each package
				final String trackingNumber = cpd[i].getTrackingIds()[0].getTrackingNumber();
				printTrackingNumbers(cpd[i]);
				System.out.println();
				//
				printPackageRating(cpd[i].getPackageRating());
				//	Write label buffer to file
				final ShippingDocument sd = cpd[i].getLabel();
				saveLabelToFile(sd, trackingNumber);
				printPackageOperationalDetails(cpd[i].getOperationalDetail());
				// If Ground COD shipment is requested, the COD return label is returned as in CodReturnPackageDetail.
				printGroundCodLabel(cpd[i], trackingNumber);
				System.out.println();
			}
		}
	}

	private static void printPackageRating(final PackageRating packageRating)
	{
		if (packageRating != null)
		{
			System.out.println("Package Rate Details");
			final PackageRateDetail[] prd = packageRating.getPackageRateDetails();
			for (int j = 0; j < prd.length; j++)
			{
				System.out.println("  Rate Type: " + prd[j].getRateType().getValue());
				printWeight(prd[j].getBillingWeight(), "Billing Weight", "    ");
				printMoney(prd[j].getBaseCharge(), "Base Charge", "    ");
				printMoney(prd[j].getNetCharge(), "Net Charge", "    ");
				printMoney(prd[j].getTotalSurcharges(), "Total Surcharge", "    ");
				if (null != prd[j].getSurcharges())
				{
					System.out.println("    Surcharge Details");
					final Surcharge[] s = prd[j].getSurcharges();
					for (int k = 0; k < s.length; k++)
					{
						printMoney(s[k].getAmount(), s[k].getSurchargeType().getValue(), "      ");
					}
				}
				System.out.println();
			}
		}
	}

	private static void printTrackingNumbers(final CompletedPackageDetail completedPackageDetail)
	{
		if (completedPackageDetail.getTrackingIds() != null)
		{
			final TrackingId[] trackingId = completedPackageDetail.getTrackingIds();
			for (int i = 0; i < trackingId.length; i++)
			{
				final String trackNumber = trackingId[i].getTrackingNumber();
				final String trackType = trackingId[i].getTrackingIdType().getValue();
				final String formId = trackingId[i].getFormId();
				printString(trackNumber, trackType + " tracking number", "  ");
				printString(formId, "Form Id", "  ");
			}
		}
	}

	private static String getPayorAccountNumber()
	{
		// See if payor account number is set as system property,
		// if not default it to "XXX"
		String payorAccountNumber = System.getProperty("Payor.AccountNumber");
		if (payorAccountNumber == null)
		{
			payorAccountNumber = "510087780"; // Replace "XXX" with the payor account number
		}
		return payorAccountNumber;
	}

	/**
	 * This method is used to add shipper for FedEx Shipment Request
	 *
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

	private static Payment addShippingChargesPayment()
	{
		final Payment payment = new Payment(); // Payment information
		payment.setPaymentType(PaymentType.SENDER);
		final Payor payor = new Payor();
		final Party responsibleParty = new Party();
		responsibleParty.setAccountNumber(getPayorAccountNumber());
		final Address responsiblePartyAddress = new Address();
		responsiblePartyAddress.setCountryCode(BlintegrationConstants.FEDEX_COUNTRY_CODE);
		responsibleParty.setAddress(responsiblePartyAddress);
		responsibleParty.setContact(new Contact());
		payor.setResponsibleParty(responsibleParty);
		payment.setPayor(payor);
		return payment;
	}

	private static ShipmentSpecialServicesRequested addShipmentSpecialServicesRequested()
	{
		final ShipmentSpecialServicesRequested shipmentSpecialServicesRequested = new ShipmentSpecialServicesRequested();
		shipmentSpecialServicesRequested.setSpecialServiceTypes(new String[]
		{ "COD" });
		//shipmentSpecialServicesRequested.setSpecialServiceTypes(shipmentSpecialServiceType);
		final CodDetail codDetail = new CodDetail();
		codDetail.setCollectionType(CodCollectionType.ANY);
		final Money codMoney = new Money();
		codMoney.setCurrency("USD");
		codMoney.setAmount(new BigDecimal(150.0));
		codDetail.setCodCollectionAmount(codMoney);
		shipmentSpecialServicesRequested.setCodDetail(codDetail);
		return shipmentSpecialServicesRequested;
	}

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

	private static CustomerReference addCustomerReference(final String customerReferenceType, final String customerReferenceValue)
	{
		final CustomerReference customerReference = new CustomerReference();
		customerReference.setCustomerReferenceType(CustomerReferenceType.fromString(customerReferenceType));
		customerReference.setValue(customerReferenceValue);
		return customerReference;
	}

	// Need to check with Mani
	private static LabelSpecification addLabelSpecification()
	{

		final LabelSpecification labelSpecification = new LabelSpecification(); // Label specification
		labelSpecification.setImageType(ShippingDocumentImageType.PDF);// Image types PDF, PNG, DPL, ...
		labelSpecification.setLabelFormatType(LabelFormatType.COMMON2D); //LABEL_DATA_ONLY, COMMON2D


		return labelSpecification;
	}

	private static void printFreightDetail(final FreightRateDetail freightRateDetail)
	{
		if (freightRateDetail != null)
		{
			System.out.println("  Freight Details");
			printFreightNotations(freightRateDetail);
			printFreightBaseCharges(freightRateDetail);

		}
	}

	private static void printFreightNotations(final FreightRateDetail frd)
	{
		if (null != frd.getNotations())
		{
			System.out.println("    Notations");
			final FreightRateNotation notations[] = frd.getNotations();
			for (int n = 0; n < notations.length; n++)
			{
				printString(notations[n].getCode(), "Code", "      ");
				printString(notations[n].getDescription(), "Notification", "      ");
			}
		}
	}

	private static void printFreightBaseCharges(final FreightRateDetail frd)
	{
		if (null != frd.getBaseCharges())
		{
			final FreightBaseCharge baseCharges[] = frd.getBaseCharges();
			for (int i = 0; i < baseCharges.length; i++)
			{
				System.out.println("    Freight Rate Details");
				printString(baseCharges[i].getDescription(), "Description", "      ");
				printString(baseCharges[i].getFreightClass().getValue(), "Freight Class", "      ");
				printString(baseCharges[i].getRatedAsClass().getValue(), "Rated Class", "      ");
				printWeight(baseCharges[i].getWeight(), "Weight", "      ");
				printString(baseCharges[i].getChargeBasis().getValue(), "Charge Basis", "      ");
				printMoney(baseCharges[i].getChargeRate(), "Charge Rate", "      ");
				printMoney(baseCharges[i].getExtendedAmount(), "Extended Amount", "      ");
				printString(baseCharges[i].getNmfcCode(), "NMFC Code", "      ");
			}
		}
	}

	private static String printMasterTrackingNumber(final CompletedShipmentDetail csd)
	{
		String trackingNumber = "";
		if (null != csd.getMasterTrackingId())
		{
			trackingNumber = csd.getMasterTrackingId().getTrackingNumber();
			System.out.println("Master Tracking Number");
			System.out.println("  Type: " + csd.getMasterTrackingId().getTrackingIdType());
			System.out.println("  Tracking Number: " + trackingNumber);
		}
		return trackingNumber;
	}

	//Saving and displaying shipping documents (labels)
	private static void saveLabelToFile(final ShippingDocument shippingDocument, final String trackingNumber) throws Exception
	{
		final ShippingDocumentPart[] sdparts = shippingDocument.getParts();
		for (int a = 0; a < sdparts.length; a++)
		{
			final ShippingDocumentPart sdpart = sdparts[a];
			String labelLocation = System.getProperty("file.label.location");
			if (labelLocation == null)
			{
				labelLocation = "D:\\Advance\\ShipService_v28_java\\ShipService_v28_java\\Ship_ProcessShipment_Express_DomesticMPS\\";
			}
			final String shippingDocumentType = shippingDocument.getType().getValue();
			final String labelFileName = new String(labelLocation + shippingDocumentType + "." + trackingNumber + "_" + a + ".pdf");
			final File labelFile = new File(labelFileName);
			final FileOutputStream fos = new FileOutputStream(labelFile);
			fos.write(sdpart.getImage());
			fos.close();
			System.out.println("\nlabel file name " + labelFile.getAbsolutePath());
			Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + labelFile.getAbsolutePath());
		}
	}

	private static void printGroundCodLabel(final CompletedPackageDetail completedPackageDetail, final String trackingNumber)
			throws Exception
	{
		final CodReturnPackageDetail codReturnPackageDetail = completedPackageDetail.getCodReturnDetail();
		if (codReturnPackageDetail != null && codReturnPackageDetail.getLabel() != null)
		{
			codReturnPackageDetail.getLabel();
			final String labelLocation = System.getProperty("file.label.location");
			final String labelName = codReturnPackageDetail.getLabel().getType().getValue();
			final ShippingDocumentPart[] parts = codReturnPackageDetail.getLabel().getParts();
			for (int i = 0; i < parts.length; i++)
			{
				final String codLabelFileName = new String(labelLocation + labelName + "." + trackingNumber + "_" + i + ".pdf");
				final File codLabelFile = new File(codLabelFileName);
				final FileOutputStream fos = new FileOutputStream(codLabelFile);
				fos.write(parts[i].getImage());
				fos.close();
				System.out.println("\nCod return label file name " + codLabelFile.getAbsolutePath());
				Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + codLabelFile.getAbsolutePath());
			}
		}
	}

	private static void saveShipmentDocumentsToFile(final ShippingDocument[] shippingDocument, final String trackingNumber)
			throws Exception
	{
		if (shippingDocument != null)
		{
			for (int i = 0; i < shippingDocument.length; i++)
			{
				final ShippingDocumentPart[] sdparts = shippingDocument[i].getParts();
				for (int a = 0; a < sdparts.length; a++)
				{
					final ShippingDocumentPart sdpart = sdparts[a];
					String labelLocation = System.getProperty("file.label.location");
					if (labelLocation == null)
					{
						labelLocation = "D:\\Advance\\ShipService_v28_java\\ShipService_v28_java\\Ship_ProcessShipment_Express_DomesticMPS\\";
					}
					final String labelName = shippingDocument[i].getType().getValue();
					final String shippingDocumentLabelFileName = new String(
							labelLocation + labelName + "." + trackingNumber + "_" + a + ".pdf");
					final File shippingDocumentLabelFile = new File(shippingDocumentLabelFileName);
					final FileOutputStream fos = new FileOutputStream(shippingDocumentLabelFile);
					fos.write(sdpart.getImage());
					fos.close();
					System.out.println("\nAssociated shipment label file name " + shippingDocumentLabelFile.getAbsolutePath());
					Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + shippingDocumentLabelFile.getAbsolutePath());
				}
			}
		}
	}

	private static void getAssociatedShipmentLabels(final AssociatedShipmentDetail[] associatedShipmentDetail) throws Exception
	{
		if (associatedShipmentDetail != null)
		{
			for (int j = 0; j < associatedShipmentDetail.length; j++)
			{
				if (associatedShipmentDetail[j].getLabel() != null && associatedShipmentDetail[j].getType() != null)
				{
					final String trackingNumber = associatedShipmentDetail[j].getTrackingId().getTrackingNumber();
					final String associatedShipmentType = associatedShipmentDetail[j].getType().getValue();
					final ShippingDocument associatedShipmentLabel = associatedShipmentDetail[j].getLabel();
					saveAssociatedShipmentLabelToFile(associatedShipmentLabel, trackingNumber, associatedShipmentType);
				}
			}
		}
	}

	private static void saveAssociatedShipmentLabelToFile(final ShippingDocument shippingDocument, final String trackingNumber,
			final String labelName) throws Exception
	{
		final ShippingDocumentPart[] sdparts = shippingDocument.getParts();
		for (int a = 0; a < sdparts.length; a++)
		{
			final ShippingDocumentPart sdpart = sdparts[a];
			String labelLocation = System.getProperty("file.label.location");
			if (labelLocation == null)
			{
				labelLocation = "D:\\Advance\\ShipService_v28_java\\ShipService_v28_java\\Ship_ProcessShipment_Express_DomesticMPS\\";
			}
			final String associatedShipmentLabelFileName = new String(
					labelLocation + labelName + "." + trackingNumber + "_" + a + ".pdf");
			final File associatedShipmentLabelFile = new File(associatedShipmentLabelFileName);
			final FileOutputStream fos = new FileOutputStream(associatedShipmentLabelFile);
			fos.write(sdpart.getImage());
			fos.close();
			System.out.println("\nAssociated shipment label file name " + associatedShipmentLabelFile.getAbsolutePath());
			Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + associatedShipmentLabelFile.getAbsolutePath());
		}
	}

	/**
	 * This method is used to create client details for FedEx shipment
	 *
	 * @return ClientDetail
	 */
	private static ClientDetail createClientDetail()
	{
		final ClientDetail clientDetail = new ClientDetail();
		clientDetail.setAccountNumber("510087780");
		clientDetail.setMeterNumber("119182802");
		return clientDetail;
	}

	/**
	 * This method is used to create Web Authentication Detail for FedEx Shipment
	 *
	 * @return WebAuthenticationDetail
	 */
	private static WebAuthenticationDetail createWebAuthenticationDetail()
	{
		final WebAuthenticationCredential userCredential = new WebAuthenticationCredential();
		final String key = "N4jviY6D0v5uY6iU";
		final String password = "kv7rLvEgVZrovN0I5OI87r07f";

		userCredential.setKey(key);
		userCredential.setPassword(password);

		WebAuthenticationCredential parentCredential = null;
		final Boolean useParentCredential = true;
		if (useParentCredential)
		{
			final String parentKey = "N4jviY6D0v5uY6iU";
			final String parentPassword = "kv7rLvEgVZrovN0I5OI87r07f";

			parentCredential = new WebAuthenticationCredential();
			parentCredential.setKey(parentKey);
			parentCredential.setPassword(parentPassword);
		}
		return new WebAuthenticationDetail(parentCredential, userCredential);

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
