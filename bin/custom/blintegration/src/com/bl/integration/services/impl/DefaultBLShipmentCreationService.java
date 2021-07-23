package com.bl.integration.services.impl;

import de.hybris.platform.util.Config;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;

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
import com.bl.logging.BlLogger;
import com.bl.shipment.data.UPSShipmentCreateResponse;
import com.google.gson.Gson;
import com.sun.xml.ws.client.ClientTransportException;
import com.ups.wsdl.xoltws.ship.v1.ShipPortType;
import com.ups.wsdl.xoltws.ship.v1.ShipService;
import com.ups.wsdl.xoltws.ship.v1.ShipmentErrorMessage;
import com.ups.xmlschema.xoltws.error.v1.ErrorDetailType;
import com.ups.xmlschema.xoltws.error.v1.Errors;
import com.ups.xmlschema.xoltws.ship.v1.ShipmentRequest;
import com.ups.xmlschema.xoltws.ship.v1.ShipmentResponse;
import com.ups.xmlschema.xoltws.upss.v1.UPSSecurity;


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

	@Value("${blintegration.fedex.api.key}")
	private String fedExapiKey;

	@Value("${blintegration.fedex.shipment.url}")
	private String fedExapiURL;

	@Value("${blintegration.ups.shipment.endpoint.url}")
	private String endpointURL;

	@Value("${blintegration.ups.shipment.create.qname}")
	private String qName;

	@Value("${blintegration.ship.wsdl.location}")
	private String wsdlLocation;

	private ShipService shipService;

	/**
	 * @param upsShipmentRequest
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
	 * @param fedExShipmentReqData
	 * @return
	 */
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

	/**
	 * @param fedExShipemtnReq
	 * @return
	 */
	private HttpResponse createFedExShipmentResponse(final FedExShipmentRequest fedExShipemtnReq)
	{
		final HttpClient httpclient = HttpClients.createDefault();

		URIBuilder builder;
		try
		{
			builder = new URIBuilder(fedExapiURL);
			final URI uri = builder.build();
			final HttpPost request = new HttpPost(uri);
			request.setHeader("Content-Type", "application/json");
			request.setHeader(BlintegrationConstants.X_API_KEY, fedExapiKey);

			final Gson gson = new Gson();
			final String json = gson.toJson(fedExShipemtnReq);

			final StringEntity reqEntity = new StringEntity(json, BlintegrationConstants.UTF_8_CODE);
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


	/**
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
	 * @param shipmentRequest
	 * @param upsSecurity
	 * @throws ShipmentErrorMessage
	 */
	private ShipmentResponse createUPSShipmentResponse(final ShipmentRequest shipmentRequest, final UPSSecurity upsSecurity)
			throws ShipmentErrorMessage
	{
		final QName qname = new QName(qName, "ShipService");
		shipService = new ShipService(getServiceURL(), qname);

		final ShipPortType shipPort = shipService.getShipPort();
		final BindingProvider bp = (BindingProvider) shipPort;
		bp.getRequestContext().put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, endpointURL);

		final List<Handler> handlerChain = bp.getBinding().getHandlerChain();
		handlerChain.add(new SOAPLoggingHandler());
		bp.getBinding().setHandlerChain(handlerChain);

		return shipPort.processShipment(shipmentRequest, upsSecurity);

	}

	private URL getServiceURL()
	{
		return this.getClass().getClassLoader().getResource(Config.getString(wsdlLocation, "META-INF/wsdl/Ship.wsdl"));
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
