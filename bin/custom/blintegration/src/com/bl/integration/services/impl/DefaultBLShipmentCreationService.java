package com.bl.integration.services.impl;

import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.ws.BindingProvider;
import javax.xml.ws.handler.Handler;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

import com.bl.core.model.BlProductModel;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.integration.Soap.logging.handler.SOAPLoggingHandler;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.fedex.shipment.pojo.FedExShipmentRequest;
import com.bl.integration.populators.BLUPSSecurityPopulator;
import com.bl.integration.services.BLShipmentCreationService;
import com.bl.integration.shipping.ups.converters.populator.BLFedExShipmentCreateRequestPopulator;
import com.bl.integration.shipping.ups.converters.populator.BLUPSShipmentCreateRequestPopulator;
import com.bl.integration.shipping.ups.converters.populator.BLUPSShipmentCreateResponsePopulator;
import com.bl.logging.BlLogger;
import com.bl.shipment.data.UPSShipmentCreateResponse;
import com.fedex.ship.stub.CompletedPackageDetail;
import com.fedex.ship.stub.CompletedShipmentDetail;
import com.fedex.ship.stub.Notification;
import com.fedex.ship.stub.NotificationSeverityType;
import com.fedex.ship.stub.PackageRateDetail;
import com.fedex.ship.stub.ProcessShipmentReply;
import com.fedex.ship.stub.ProcessShipmentRequest;
import com.fedex.ship.stub.ShipServiceLocator;
import com.fedex.ship.stub.ShipmentOperationalDetail;
import com.fedex.ship.stub.Surcharge;
import com.google.gson.Gson;
import com.sun.xml.ws.client.ClientTransportException;//NOSONAR
import com.ups.wsdl.xoltws.ship.v1.ShipPortType;
import com.ups.wsdl.xoltws.ship.v1.ShipService;
import com.ups.wsdl.xoltws.ship.v1.ShipmentErrorMessage;
import com.ups.xmlschema.xoltws.error.v1.ErrorDetailType;
import com.ups.xmlschema.xoltws.error.v1.Errors;
import com.ups.xmlschema.xoltws.ship.v1.ShipmentRequest;
import com.ups.xmlschema.xoltws.ship.v1.ShipmentResponse;
import com.ups.xmlschema.xoltws.upss.v1.UPSSecurity;


/**
 * This class is responsible for shipment creation
 *
 * @author Aditi Sharma
 *
 */
public class DefaultBLShipmentCreationService implements BLShipmentCreationService
{
	private static final Logger LOG = Logger.getLogger(DefaultBLShipmentCreationService.class);

	private BLUPSShipmentCreateRequestPopulator blUPSShipmentCreateRequestPopulator;

	private BLUPSShipmentCreateResponsePopulator blUPSShipmentCreateResponsePopulator;

	private BLFedExShipmentCreateRequestPopulator blFedExShipmentCreateRequestPopulator;

	private BLUPSSecurityPopulator blUPSSecurityPopulator;

	@Value("${blintegration.fedex.api.key}")
	private String fedExapiKey;

	@Value("${blintegration.fedex.shipment.url}")
	private String fedExapiURL;

	@Value("${blintegration.fedex.shipment.password}")
	private String fedExapiPassword;

	@Value("${blintegration.ups.shipment.endpoint.url}")
	private String endpointURL;

	@Value("${blintegration.ups.shipment.create.qname}")
	private String qName;

	@Value("${blintegration.ship.wsdl.location}")
	private String wsdlLocation;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public UPSShipmentCreateResponse createUPSShipment(final UpsShippingRequestData upsShipmentRequest)
	{
		final UPSShipmentCreateResponse upsShipmentResponseData = new UPSShipmentCreateResponse();
		try
		{
			final ShipmentRequest shipmentRequest = getBlUPSShipmentCreateRequestPopulator()
					.convertToUPSShipmentRequest(upsShipmentRequest);
			final UPSSecurity upsSecurity = getBlUPSSecurityPopulator().populateUPSSecurity();

			final ShipmentResponse upsShipmentResponse = createUPSShipmentResponse(shipmentRequest, upsSecurity);

			final UPSShipmentCreateResponse upsResponse = getBlUPSShipmentCreateResponsePopulator()
					.convertUPSResponse(upsShipmentResponse);

			upsShipmentResponseData.setStatusCode(upsResponse.getStatusCode());
			upsShipmentResponseData.setStatusMessage(upsResponse.getStatusMessage());
			return upsResponse;

		}

		catch (final ClientTransportException ct)
		{
			upsShipmentResponseData.setStatusCode(BlintegrationConstants.CLIENT_SIDE_ERROR);
			upsShipmentResponseData.setStatusMessage(BlintegrationConstants.CLIENT_SIDE_ERROR_DESCRIPTION);
			upsShipmentResponseData.setErrorDescription(ct.getMessage());
		}
		catch (final Exception ex)
		{
			if (ex instanceof ShipmentErrorMessage) //NOSONAR
			{
				final ShipmentErrorMessage err = (ShipmentErrorMessage) ex;
				final Errors faultMessage = err.getFaultInfo();
				final ErrorDetailType errorDetails = faultMessage.getErrorDetail().get(0);
				if (errorDetails != null && errorDetails.getPrimaryErrorCode() != null)
				{
					upsShipmentResponseData.setStatusCode(errorDetails.getPrimaryErrorCode().getCode());
					upsShipmentResponseData.setErrorDescription(errorDetails.getPrimaryErrorCode().getDescription());
					upsShipmentResponseData.setStatusMessage(ex.getMessage());
				}
				else
				{
					upsShipmentResponseData.setErrorDescription(ex.getMessage());
				}
			}
			else
			{
				BlLogger.logMessage(LOG, Level.INFO, ex.getMessage());
				populateResponseExceptionData(upsShipmentResponseData, ex);
			}
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 */

	public ProcessShipmentReply createFedExShipment(final PackagingInfoModel packagingInfo, final int packageCount,
			final Map<String, Integer> sequenceMap, final WarehouseModel warehouseModel)
	{
		ProcessShipmentRequest masterRequest = new ProcessShipmentRequest();
		if (warehouseModel == null)
		{
			masterRequest = getBlFedExShipmentCreateRequestPopulator().createFedExShipmentRequest(packagingInfo, packageCount,
					sequenceMap.get(packagingInfo.getPackageId()).toString());
		}
		else
		{
			masterRequest = getBlFedExShipmentCreateRequestPopulator().createFedExReturnShipmentRequest(packagingInfo, packageCount,
					sequenceMap.get(packagingInfo.getPackageId()).toString(), warehouseModel);
		}
		try
		{
			// Initialize the service
			ShipServiceLocator service;
			com.fedex.ship.stub.ShipPortType port;
			//
			service = new ShipServiceLocator();
			updateEndPoint(service);
			port = service.getShipServicePort();
			
			BlLogger.logMessage(LOG, Level.DEBUG, "Sending Request to FedEx for Shipment Generation");			
			return port.processShipment(masterRequest); // This is the call to the ship web service passing in a request object and returning a reply object

		}
		catch (final Exception e)
		{
			e.printStackTrace();
		}
		return null;

	}

	private Map processResponse(final ProcessShipmentReply reply, final AbstractOrderModel order) throws Exception
	{
		final Map results = new HashMap();
		final CompletedShipmentDetail csd = reply.getCompletedShipmentDetail();
		final CompletedPackageDetail cpd[] = csd.getCompletedPackageDetails();
		final StringBuffer logMessage = new StringBuffer("");
		logMessage.append("Package details ");
		for (int i = 0; i < cpd.length; i++)
		{ // Package details / Rating
		  // information for each
		  // package
			final String trackingNumber = cpd[i].getTrackingIds(0).getTrackingNumber();
			results.put("TrackingNumber", trackingNumber);

			if (cpd[i].getPackageRating() != null)
			{
				final PackageRateDetail[] prd = cpd[i].getPackageRating().getPackageRateDetails();
				for (int j = 0; j < prd.length; j++)
				{
					if (null != prd[j].getBillingWeight())
					{
						logMessage.append(
								"Billing weight: " + prd[j].getBillingWeight().getValue() + " " + prd[j].getBillingWeight().getUnits());
					}
					results.put("BillingWeight", prd[j].getBillingWeight().getValue());
					if (null != prd[j].getBaseCharge())
					{
						logMessage.append("Base charge: " + prd[j].getBaseCharge().getAmount() + " ");
					}
					if (null != prd[j].getNetCharge())
					{
						logMessage.append("Net charge: " + prd[j].getNetCharge().getAmount() + " ");
					}
					results.put("NetCharge", prd[j].getNetCharge().getAmount());
					if (null != prd[j].getSurcharges())
					{
						final Surcharge[] s = prd[j].getSurcharges();
						for (int k = 0; k < s.length; k++)
						{
							if (null != s[k].getSurchargeType())
							{
								logMessage.append(s[k].getSurchargeType() + " surcharge " + s[k].getAmount().getAmount() + " ");
							}
						}
					}
					if (null != prd[j].getTotalSurcharges())
					{
						logMessage.append("Total surcharge: " + prd[j].getTotalSurcharges().getAmount() + " ");
					}
					logMessage.append(" Routing details ");

				}
			}
		}

		return results;
	}


	private static void updateEndPoint(final ShipServiceLocator serviceLocator)
	{
		final String endPoint = System.getProperty("endPoint");
		if (endPoint != null)
		{
			serviceLocator.setShipServicePortEndpointAddress(endPoint);
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

	private static void writeServiceOutput(final ProcessShipmentReply reply) throws Exception
	{
		try
		{
			System.out.println(reply.getTransactionDetail().getCustomerTransactionId());
			final CompletedShipmentDetail csd = reply.getCompletedShipmentDetail();
			final String masterTrackingNumber = printMasterTrackingNumber(csd);
			printShipmentOperationalDetails(csd.getOperationalDetail());
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

	private static void printString(final String value, final String description, final String space)
	{
		if (value != null)
		{
			System.out.println(space + description + ": " + value);
		}
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

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean checkOrderStatus(final ConsignmentModel consignment)
	{
		if (consignment.getOrder() != null)
		{
			final OrderStatus status = consignment.getOrder().getStatus();
			if (status.equals(OrderStatus.CANCELLED) || status.equals(OrderStatus.CHECKED_INVALID)
					|| status.equals(OrderStatus.PAYMENT_NOT_AUTHORIZED) || status.equals(OrderStatus.RECEIVED_PAYMENT_DECLINED))
			{
				return false;
			}
		}
		return true;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, Integer> getSequenceNumber(final Map<String, Integer> sequenceMap, final List<PackagingInfoModel> packages,
			final int packageCount)
	{
		for (int i = 0; i < packageCount; i++)
		{
			sequenceMap.put(packages.get(i).getPackageId(), i + 1);
		}
		return sequenceMap;
	}
	
	/**
	 * method will be used to create fedEx shipment response
	 *
	 * @param fedExShipemtnReq
	 * @return
	 */
	private HttpResponse createFedExShipmentResponse(final FedExShipmentRequest fedExShipemtnReq)
	{
		final HttpClient httpclient = HttpClients.createDefault(); //NOSONAR

		URIBuilder builder;
		try
		{
			builder = new URIBuilder(fedExapiURL);
			final URI uri = builder.build();
			final HttpPost request = new HttpPost(uri);
			request.setHeader(BlintegrationConstants.CONTENT_TYPE, "application/json");
			request.setHeader(BlintegrationConstants.X_API_KEY, fedExapiKey);
			request.setHeader(BlintegrationConstants.AUTHORIZATION, fedExapiPassword);

			final Gson gson = new Gson();
			final String json = gson.toJson(fedExShipemtnReq);

			final StringEntity reqEntity = new StringEntity(json, BlintegrationConstants.UTF_8_CODE);
			request.setEntity(reqEntity);

			return httpclient.execute(request);
		}
		catch (final URISyntaxException uriException)
		{
			BlLogger.logMessage(LOG, Level.DEBUG, uriException.getMessage());
		}

		catch (final IOException io)
		{
			BlLogger.logMessage(LOG, Level.DEBUG, io.getMessage());
		}

		return null;

	}


	/**
	 * method will be used to populate response data for exception
	 *
	 * @param upsShipmentResponseData
	 * @param ex
	 */
	private void populateResponseExceptionData(final UPSShipmentCreateResponse upsShipmentResponseData, final Exception ex)
	{
		upsShipmentResponseData.setStatusCode(BlintegrationConstants.INTERNAL_SERVER_ERROR_CODE);
		upsShipmentResponseData.setStatusMessage(BlintegrationConstants.FAILURE_STRING);
		upsShipmentResponseData.setErrorDescription(ex.getMessage());
	}

	/**
	 * method will be used to create shipment response for UPS
	 *
	 * @param shipmentRequest
	 * @param upsSecurity
	 * @throws ShipmentErrorMessage
	 */
	private ShipmentResponse createUPSShipmentResponse(final ShipmentRequest shipmentRequest, final UPSSecurity upsSecurity)
			throws ShipmentErrorMessage
	{
		ShipService shipService = null;
		final QName qname = new QName(qName, BlintegrationConstants.Q_NAME_CODE);
		shipService = new ShipService(getServiceURL(), qname);

		final ShipPortType shipPort = shipService.getShipPort();
		final BindingProvider bp = (BindingProvider) shipPort;
		bp.getRequestContext().put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, endpointURL);

		final List<Handler> handlerChain = bp.getBinding().getHandlerChain();
		handlerChain.add(new SOAPLoggingHandler());
		bp.getBinding().setHandlerChain(handlerChain);

		return shipPort.processShipment(shipmentRequest, upsSecurity);

	}

	/**
	 * method will be used to get teh service URL
	 *
	 * @return
	 */
	private URL getServiceURL()
	{
		return this.getClass().getClassLoader().getResource(Config.getString(wsdlLocation, "META-INF/wsdl/Ship.wsdl"));
	}

	/**
	 * {@inheritDoc}
	 */
	public PackagingInfoModel getPackageForSerial(final ConsignmentModel consignment, final String serialCode)
	{
		for (final PackagingInfoModel blPackage : consignment.getPackaginginfos())
		{
			if (isSerialPresentInPackage(blPackage, serialCode))
			{
				return blPackage;
			}
		}
		return null;
	}

	/**
	 * Checks if is serial present in package.
	 *
	 * @param blPackage
	 *           the bl package
	 * @param serialCode
	 *           the serial code
	 * @return true, if is serial present in package
	 */
	private boolean isSerialPresentInPackage(final PackagingInfoModel blPackage, final String serialCode)
	{
		for (final BlProductModel product : blPackage.getSerialProducts())
		{
			if (product.getCode().equals(serialCode))
			{
				return true;
			}
		}
		return false;
	}

	/**
	 * @return the blUPSShipmentCreateRequestPopulator
	 */
	public BLUPSShipmentCreateRequestPopulator getBlUPSShipmentCreateRequestPopulator()
	{
		return blUPSShipmentCreateRequestPopulator;
	}

	/**
	 * @param blUPSShipmentCreateRequestPopulator
	 *           the blUPSShipmentCreateRequestPopulator to set
	 */
	public void setBlUPSShipmentCreateRequestPopulator(
			final BLUPSShipmentCreateRequestPopulator blUPSShipmentCreateRequestPopulator)
	{
		this.blUPSShipmentCreateRequestPopulator = blUPSShipmentCreateRequestPopulator;
	}

	/**
	 * @return the blUPSShipmentCreateResponsePopulator
	 */
	public BLUPSShipmentCreateResponsePopulator getBlUPSShipmentCreateResponsePopulator()
	{
		return blUPSShipmentCreateResponsePopulator;
	}

	/**
	 * @param blUPSShipmentCreateResponsePopulator
	 *           the blUPSShipmentCreateResponsePopulator to set
	 */
	public void setBlUPSShipmentCreateResponsePopulator(
			final BLUPSShipmentCreateResponsePopulator blUPSShipmentCreateResponsePopulator)
	{
		this.blUPSShipmentCreateResponsePopulator = blUPSShipmentCreateResponsePopulator;
	}

	/**
	 * @return the blUPSSecurityPopulator
	 */
	public BLUPSSecurityPopulator getBlUPSSecurityPopulator()
	{
		return blUPSSecurityPopulator;
	}

	/**
	 * @param blUPSSecurityPopulator
	 *           the blUPSSecurityPopulator to set
	 */
	public void setBlUPSSecurityPopulator(final BLUPSSecurityPopulator blUPSSecurityPopulator)
	{
		this.blUPSSecurityPopulator = blUPSSecurityPopulator;
	}

	/**
	 * @return the blFedExShipmentCreateRequestPopulator
	 */
	public BLFedExShipmentCreateRequestPopulator getBlFedExShipmentCreateRequestPopulator()
	{
		return blFedExShipmentCreateRequestPopulator;
	}

	/**
	 * @param blFedExShipmentCreateRequestPopulator
	 *           the blFedExShipmentCreateRequestPopulator to set
	 */
	public void setBlFedExShipmentCreateRequestPopulator(
			final BLFedExShipmentCreateRequestPopulator blFedExShipmentCreateRequestPopulator)
	{
		this.blFedExShipmentCreateRequestPopulator = blFedExShipmentCreateRequestPopulator;
	}

}
