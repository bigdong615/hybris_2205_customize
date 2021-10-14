package com.bl.integration.services.impl;

import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.cronjob.BlAutomatedUPSScrapeJob;
import com.bl.integration.services.BlUPSTrackService;
import com.bl.integration.ups.common.v1.RequestType;
import com.bl.integration.ups.track.TrackPortType;
import com.bl.integration.ups.track.TrackService;
import com.bl.integration.ups.track.v2.TrackRequest;
import com.bl.integration.ups.track.v2.TrackResponse;
import com.bl.integration.ups.v1.UPSSecurity;
import com.bl.integration.ups.v1.UPSSecurity.ServiceAccessToken;
import com.bl.integration.ups.v1.UPSSecurity.UsernameToken;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.ws.BindingProvider;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created to call the UPS Service
 * @author Manikandan
 */
public class DefaultBlUPSTrackServiceImpl implements BlUPSTrackService {

  private static final Logger LOG = Logger.getLogger(DefaultBlUPSTrackServiceImpl.class);

  /**
   * This method created for track ups service
   * @param abstractOrderModel abstractOrderModel
   * @param packagingInfoModel packagingInfoModel
   * @return Map<String, Object>
   */
  @Override
  public Map<String, Object> trackUPSService(final AbstractOrderModel abstractOrderModel , final
  PackagingInfoModel packagingInfoModel) {
    final Map<String, Object> stringObjectMap = new HashMap<>();
      try {
        final TrackService trackService = new TrackService();
        TrackPortType trackPortType = trackService.getTrackPort();

        final BindingProvider bindingProvider = (BindingProvider)trackPortType;
        getEndPointURLForUPS(bindingProvider);
        TrackResponse trackResponse = trackPortType.processTrack(getTrackRequestForUPS(), getSecurityDetailsForUPS());
        BlLogger.logMessage(LOG , Level.ERROR , trackResponse.getResponse().toString());
      } catch(Exception e) {
        BlLogger.logMessage(LOG , Level.ERROR , "Error while executing trackUPSService" , e.getMessage());
      }
    return stringObjectMap;
  }

  /**
   * This method created to get the URL for UPS service
   * @param bindingProvider bindingProvider
   */
  private void getEndPointURLForUPS(final BindingProvider bindingProvider){
    bindingProvider.getRequestContext().put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, getValuesFromProperty(
        BlintegrationConstants.UPS_API_URL));
  }

  /**
   * This method created to set the tracking details for UPS
   * @return TrackRequest
   */
  private TrackRequest getTrackRequestForUPS(){
    final TrackRequest trackRequest = new TrackRequest();
    final RequestType request = new RequestType();
    final List<String> requestOption = request.getRequestOption();
    requestOption.add("1");
    request.setRequestOption(requestOption);
    trackRequest.setRequest(request);
    trackRequest.setInquiryNumber("1Z19E5968748538530");
    trackRequest.setTrackingOption("02");
    return trackRequest;
  }

  /**
   * This method created for set the token for UPS service
   * @return UPSSecurity
   */
  private UPSSecurity getSecurityDetailsForUPS(){
    final UPSSecurity upsSecurity = new UPSSecurity();
    final ServiceAccessToken serviceAccessToken = new ServiceAccessToken();
    serviceAccessToken.setAccessLicenseNumber(getValuesFromProperty(BlintegrationConstants.UPS_API_LICENSE));
    upsSecurity.setServiceAccessToken(serviceAccessToken);
    final UsernameToken usernameToken = new UPSSecurity.UsernameToken();
    usernameToken.setUsername(getValuesFromProperty(BlintegrationConstants.UPS_API_USER_NAME));
    usernameToken.setPassword(getValuesFromProperty(BlintegrationConstants.UPS_API_PASSWORD));
    upsSecurity.setUsernameToken(usernameToken);
    return upsSecurity;
  }

  /**
   * This method created to get the values from propery
   * @param key key
   * @return String
   */
  private String getValuesFromProperty(final String key) {
    return StringUtils.isNotBlank(Config.getParameter(key)) ? Config.getParameter(key)
        : StringUtils.EMPTY;
  }


}
