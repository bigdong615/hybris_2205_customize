/**
 *
 */
package com.bl.integration.services.impl;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.apache.commons.lang.BooleanUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

import com.bl.facades.fexEx.data.SameDayCityReqData;
import com.bl.facades.fexEx.data.SameDayCityResData;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.services.BlFedExSameDayService;
import com.bl.logging.BlLogger;

import atg.taglib.json.util.JSONException;
import atg.taglib.json.util.JSONObject;


/**
 * @author Aditi Sharma
 *
 */
public class DefaultFedExSameDayServiceImpl implements BlFedExSameDayService
{
	private static final Logger LOG = Logger.getLogger(DefaultFedExSameDayServiceImpl.class);

	@Value("${blintegration.fedex.request.uri}")
	private String requestURI;

	@Value("${blintegration.fedex.same.day.api.key}")
	private String apiKey;

	@Value("${blintegration.fedex.samedaycity.mock.enable}")
	private Boolean mockEnable;


	@Override
	public SameDayCityResData getAvailability(final SameDayCityReqData sameDayCityReqData)
	{
		HttpGet request = new HttpGet();
		final SameDayCityResData sameDayCityResData = new SameDayCityResData();

		final HttpClient httpClient = HttpClients.createDefault();

		try
		{
			final URIBuilder builder = new URIBuilder(requestURI);
			builder.setParameter(BlintegrationConstants.PICKUP_ZIP_CODE, sameDayCityReqData.getWarehouseZipCode());
			builder.setParameter(BlintegrationConstants.DROPOFF_ZIP_CODE, sameDayCityReqData.getDeliveryAddressZipCode());

			final URI uri = builder.build();
			request = new HttpGet(uri);
			request.setHeader(BlintegrationConstants.X_API_KEY, apiKey);
			BlLogger.logMessage(LOG, Level.INFO,
					"**** FEDEX Same day Request " + "WarehouseZipCode" + sameDayCityReqData.getWarehouseZipCode()
							+ "DeliveryAddress ZipCode" + sameDayCityReqData.getDeliveryAddressZipCode());
			BlLogger.logMessage(LOG, Level.INFO, "FedEx Request: " + request.toString());

			final HttpResponse response = httpClient.execute(request);

			BlLogger.logMessage(LOG, Level.INFO, "**** FEDEX Same day Response ");
			BlLogger.logMessage(LOG, Level.INFO, "FedEx Reponse: " + response.toString());

			final String result = EntityUtils.toString(response.getEntity());

			// This condtion will get remove once proxy server issue get resolved
			if (BooleanUtils.isTrue(mockEnable))
			{
				if (sameDayCityReqData.getDeliveryAddressZipCode().equals("95054")
						|| sameDayCityReqData.getDeliveryAddressZipCode().equals("10109"))
				{
					sameDayCityResData.setServiceApplicable(true);
					return sameDayCityResData;
				}
			}
			else
			{
				if (response.getStatusLine().getStatusCode() == 200 || response.getStatusLine().getStatusCode() == 201)
				{
					final JSONObject jsonObj = new JSONObject(result);
					sameDayCityResData.setServiceApplicable(jsonObj.getBoolean("available"));
					BlLogger.logMessage(LOG, Level.INFO, "**** Same day city call SUCCESS ****" + jsonObj.getBoolean("available"));
					return sameDayCityResData;
				}

				else
				{
					sameDayCityResData.setErrorCode(response.getStatusLine().getStatusCode());
					sameDayCityResData.setErrorResponse(response.getStatusLine().getReasonPhrase());
					sameDayCityResData.setServiceApplicable(null);
					BlLogger.logMessage(LOG, Level.ERROR, "*************** Error Occured *************** "
							+ response.getStatusLine().getStatusCode() + " : " + response.getStatusLine().getReasonPhrase());
					return sameDayCityResData;
				}
			}

		}
		catch (final IOException | URISyntaxException | JSONException ex)
		{
			sameDayCityResData.setErrorCode(400);
			sameDayCityResData.setErrorResponse(ex.getMessage());
			sameDayCityResData.setServiceApplicable(null);
			BlLogger.logMessage(LOG, Level.ERROR, "************* IO Exception occure at : " + ex);
		}
		finally
		{
			request.releaseConnection();
		}
		return sameDayCityResData;
	}

}
