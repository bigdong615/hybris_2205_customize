/**
 *
 */
package com.bl.integration.services.impl;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

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

import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.integration.Soap.logging.handler.SOAPLoggingHandler;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.fedex.shipment.pojo.FedExShipmentRequest;
import com.bl.integration.populators.BLUPSSecurityPopulator;
import com.bl.integration.services.BLShipmentCreationService;
import com.bl.integration.shipping.ups.converters.populator.BLFedExShipmentCreateRequestPopulator;
import com.bl.integration.shipping.ups.converters.populator.BLUPSShipmentCreateRequestPopulator;
import com.bl.integration.shipping.ups.converters.populator.BLUPSShipmentCreateResponsePopulator;
import com.bl.integration.ups.error.v1.pojo.ErrorDetailType;
import com.bl.integration.ups.error.v1.pojo.Errors;
import com.bl.integration.ups.ship.v1.pojo.ShipmentRequest;
import com.bl.integration.ups.ship.v1.pojo.ShipmentResponse;
import com.bl.integration.ups.upss.v1.pojo.UPSSecurity;
import com.bl.integration.ups.wsdl.shipment.v1.pojo.ShipPortType;
import com.bl.integration.ups.wsdl.shipment.v1.pojo.ShipService;
import com.bl.integration.ups.wsdl.shipment.v1.pojo.ShipmentErrorMessage;
import com.bl.logging.BlLogger;
import com.bl.shipment.data.UPSShipmentCreateResponse;
import com.google.gson.Gson;
import com.sun.xml.ws.client.ClientTransportException;


/**
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

	private DefaultUPSIntegrationService upsIntegrationService;

	@Value("${blintegration.ups.shipment.endpoint.url}")
	private String endpointURL;

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

			BlLogger.logMessage(LOG, Level.INFO, "UPS Response " + "Shipment Number " + upsResponse.getShipmentIdentificationNumber()
					+ "Label URL" + upsResponse.getLabelURL());

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
			if (ex instanceof ShipmentErrorMessage)
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
			}
			else
			{
				BlLogger.logMessage(LOG, Level.INFO, ex.getMessage());
				populateResponseExceptionData(upsShipmentResponseData, ex);
			}
		}
		return null;
	}

	@Override
	public String createFedExShipment(final FedExShippingRequestData fedExShipmentReqData)
	{
		final FedExShipmentRequest fedExShipemtnReq = getBlFedExShipmentCreateRequestPopulator()
				.convertToFedExShipmentRequest(fedExShipmentReqData);
		final HttpResponse createFedExShipmentResponse = createFedExShipmentResponse(fedExShipemtnReq);
		if (createFedExShipmentResponse != null)
		{
			BlLogger.logMessage(LOG, Level.INFO, createFedExShipmentResponse.toString());
		}
		return null;
	}

	private HttpResponse createFedExShipmentResponse(final FedExShipmentRequest fedExShipemtnReq)
	{
		final HttpClient httpclient = HttpClients.createDefault();

		URIBuilder builder;
		try
		{
			builder = new URIBuilder("https://api.test.samedaycity.fedex.com/shipment/v1/shipment");
			final URI uri = builder.build();
			final HttpPost request = new HttpPost(uri);
			request.setHeader("Content-Type", "application/json");
			request.setHeader("X-Api-Key", "71fcf31cdf13458992fbcc65856f3023");

			final Gson gson = new Gson();
			final String json = gson.toJson(fedExShipemtnReq);

			final StringEntity reqEntity = new StringEntity(json, "utf-8");
			request.setEntity(reqEntity);

			return httpclient.execute(request);
		}
		catch (final URISyntaxException uriException)
		{
			BlLogger.logMessage(LOG, Level.INFO, uriException.getMessage());
		}

		catch (final IOException io)
		{
			BlLogger.logMessage(LOG, Level.INFO, io.getMessage());
		}
		return null;

	}


	private void populateResponseExceptionData(final UPSShipmentCreateResponse upsShipmentResponseData, final Exception ex)
	{
		upsShipmentResponseData.setStatusCode(BlintegrationConstants.INTERNAL_SERVER_ERROR_CODE);
		upsShipmentResponseData.setStatusMessage(BlintegrationConstants.FAILURE_STRING);
		upsShipmentResponseData.setErrorDescription(ex.getMessage());
	}

	/**
	 * @param shipmentRequest
	 * @param upsSecurity
	 * @throws ShipmentErrorMessage
	 */
	private ShipmentResponse createUPSShipmentResponse(final ShipmentRequest shipmentRequest, final UPSSecurity upsSecurity)
			throws ShipmentErrorMessage
	{
		final ShipmentResponse shipResponse = new ShipmentResponse();
		final ShipService shipService = new ShipService();
		final ShipPortType shipPort = shipService.getShipPort();
		final BindingProvider bp = (BindingProvider) shipPort;
		bp.getRequestContext().put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, endpointURL);

		final List<Handler> handlerChain = bp.getBinding().getHandlerChain();
		handlerChain.add(new SOAPLoggingHandler());
		bp.getBinding().setHandlerChain(handlerChain);

		return shipPort.processShipment(shipmentRequest, upsSecurity);

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
