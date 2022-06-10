package com.bl.integration.services.impl;

import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.services.BlUPSTrackService;
import com.bl.integration.ups.common.v1.RequestType;
import com.bl.integration.ups.track.TrackPortType;
import com.bl.integration.ups.track.TrackService;
import com.bl.integration.ups.track.v2.ActivityType;
import com.bl.integration.ups.track.v2.DeliveryDetailType;
import com.bl.integration.ups.track.v2.PackageType;
import com.bl.integration.ups.track.v2.ServiceType;
import com.bl.integration.ups.track.v2.ShipmentType;
import com.bl.integration.ups.track.v2.TrackRequest;
import com.bl.integration.ups.track.v2.TrackResponse;
import com.bl.integration.ups.v1.UPSSecurity;
import com.bl.integration.ups.v1.UPSSecurity.ServiceAccessToken;
import com.bl.integration.ups.v1.UPSSecurity.UsernameToken;
import com.bl.logging.BlLogger;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.ups.wsdl.xoltws.ship.v1.ShipService;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.xml.namespace.QName;
import javax.xml.ws.BindingProvider;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

/**
 * This class created to call the UPS Service
 * @author Manikandan
 */
public class DefaultBlUPSTrackServiceImpl implements BlUPSTrackService {

  private static final Logger LOG = Logger.getLogger(DefaultBlUPSTrackServiceImpl.class);
  
	@Value("${blintegration.ups.track.create.qname}")
	private String qName;

	@Value("${blintegration.ups.scrape.wsdl.location}")
	private String trackLocation;
  /**
   * {@inheritDoc}
   */
  @Override
  public Map<String, Object> trackUPSService(final AbstractOrderModel abstractOrderModel , final
  PackagingInfoModel packagingInfoModel) {
    BlLogger.logMessage(LOG  , Level.INFO , "Started Performing UPS Scrape for UPS service");
    final Map<String, Object> stringObjectMap = new HashMap<>();
      try {
      	TrackService trackService = null;
   		final QName qname = new QName(qName, BlintegrationConstants.Q_NAME_TRACK_CODE);
   		trackService = new TrackService(getServiceURL(), qname);
   		final TrackPortType trackPortType = trackService.getTrackPort();
        final BindingProvider bindingProvider = (BindingProvider)trackPortType;
        getEndPointURLForUPS(bindingProvider);
        final TrackResponse response = trackPortType.processTrack(getTrackRequestForUPS(packagingInfoModel), getSecurityDetailsForUPS());
        final Gson gson = new GsonBuilder().create();
        final String json = gson.toJson(response);
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "UPS Scrape Response {}" , json);
        convertResponse(response , stringObjectMap);
      } catch(final Exception e) {
        BlLogger.logMessage(LOG , Level.ERROR , "Error while executing trackUPSService " , e);
        return Collections.emptyMap();
      }
    stringObjectMap.put(BlintegrationConstants.SCRAPE_TYPE , BlintegrationConstants.UPS_TYPE);
    BlLogger.logMessage(LOG  , Level.INFO , "Finished UPS Scrape for UPS service");
    return stringObjectMap;
  }

  /**
	 * method will be used to get the service URL
	 *
	 * @return
	 */
	private URL getServiceURL()
	{
		return this.getClass().getClassLoader().getResource(Config.getString(trackLocation, "TrackClient/META-INF/trackwsdl/Track.wsdl"));
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
   * @param packagingInfoModel  packagingInfoModel to prepare request
   * @return TrackRequest
   */
  private TrackRequest getTrackRequestForUPS(final PackagingInfoModel packagingInfoModel){
    final TrackRequest trackRequest = new TrackRequest();
    final RequestType request = new RequestType();
    final List<String> requestOption = request.getRequestOption();
    requestOption.add(BlintegrationConstants.REQUEST_OPTION_NUMBER);
    request.setRequestOption(requestOption);
    trackRequest.setRequest(request);
    trackRequest.setInquiryNumber(StringUtils.isBlank(packagingInfoModel.getInBoundTrackingNumber())
        ? StringUtils.EMPTY : packagingInfoModel.getInBoundTrackingNumber());
    trackRequest.setTrackingOption(BlintegrationConstants.TRACKING_OPTION);
      final Gson gson = new GsonBuilder().create();
      final String json = gson.toJson(trackRequest);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "UPS Scrape Request {}" , json);
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
      final Gson gson = new GsonBuilder().create();
      final String json = gson.toJson(upsSecurity);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "UPS Securtiy Credential {}" , json);
      
    return upsSecurity;
  }

  /**
   * This method created to get the values from propery
   * @param key to get values from Property file
   * @return String values get from property
   */
  private String getValuesFromProperty(final String key) {
    return StringUtils.isNotBlank(Config.getParameter(key)) ? Config.getParameter(key)
        : StringUtils.EMPTY;
  }

  /**
   * This method created to convert response
   * @param response response from UPS scrape service
   * @param stringObjectMap response to be updated
   */
  private void convertResponse(final TrackResponse response, final Map<String, Object> stringObjectMap) {
    if(Objects.nonNull(response)){
      final List<ShipmentType> shipment = response.getShipment();
      if(CollectionUtils.isNotEmpty(shipment)) {
              for(ShipmentType shipmentType : shipment) {
                ServiceType service = shipmentType.getService();
                if (Objects.nonNull(service)) {
                  stringObjectMap.put(BlintegrationConstants.SERVICE_TYPE, service.getCode());
                  stringObjectMap.put(BlintegrationConstants.SERVICE_DESCRIPTION, service.getDescription());
                }
                convertShipmentResponse(stringObjectMap , shipmentType);
                int packageCount = 0;
                int activityCount = 0;
                convertResponseFromResults(shipmentType , packageCount , activityCount ,stringObjectMap);
                stringObjectMap.put(BlintegrationConstants.PACKAGE_COUNT, packageCount);
                stringObjectMap.put(BlintegrationConstants.ACTIVITY_COUNT, activityCount);
              }
            }
    }
  }

  /**
   * This method created to convert the response from results
   * @param shipmentType shipmentType of package
   * @param activityCount activity Count
   * @param packageCount package Count
   * @param stringObjectMap stringObjectMap results to be updated
   */
  private void convertResponseFromResults(final ShipmentType shipmentType, final int activityCount, int packageCount,
      final Map<String, Object> stringObjectMap) {
    if (shipmentType.getPackage() != null && shipmentType.getPackage().size() > 0) {
      for (final PackageType pkg : shipmentType.getPackage()) {
        packageCount++;
        if (pkg.getDeliveryDetail() != null && pkg.getDeliveryDetail().size() > 0) {
          for (final DeliveryDetailType deliveryDetail : pkg.getDeliveryDetail()) {
            convertDeliveryDetailsFromResponse(deliveryDetail , stringObjectMap);
          }
        }
       performConvertResponse(stringObjectMap , pkg , activityCount);
      }
    }
  }

  /**
   * This method created to convert the response for package
   * @param stringObjectMap stringObjectMap results to be updated
   * @param pkg package from response
   * @param activityCount activityCount
   */
  private void performConvertResponse(final Map<String, Object> stringObjectMap, final PackageType pkg, int activityCount) {
    if (pkg.getActivity() != null) {
      activityCount += pkg.getActivity().size();
    }
    // Find the newest activity
     ActivityType lastActivity = null;
    if(CollectionUtils.isNotEmpty(pkg.getActivity())) {
     lastActivity = findLastRelevantActivity(pkg.getActivity());
    }
    if (lastActivity != null) {
      convertStatusResponse(stringObjectMap , lastActivity);
    }
    covertPackageResponse(stringObjectMap, pkg);
    BlLogger.logMessage(LOG , Level.INFO , "activity count " , String.valueOf(activityCount));
  }

  /**
   * This method created to convert the response for package
   * @param stringObjectMap results to be updated
   * @param pkg package from response
   */
  private void covertPackageResponse(final Map<String, Object> stringObjectMap, final PackageType pkg) {
    final ActivityType delivered = findActivityType(pkg.getActivity(), BlintegrationConstants.PACAKAGE_ACTIVITY_D);
    if (delivered != null) {
      stringObjectMap.put(BlintegrationConstants.A_PACKAGE_WAS_DELIVERED, BlintegrationConstants.REQUEST_OPTION_NUMBER);
      stringObjectMap.put(BlintegrationConstants.A_PACKAGE_WAS_DELIVERED_ON, convertIntoDate(convertUPSDateTime(delivered.getDate(), delivered
          .getTime())));
    }
    if (Objects.nonNull(stringObjectMap.get(BlintegrationConstants.SHIP_TIME_STAMP))) {
      final ActivityType pickedUp = findActivityType(pkg.getActivity(), BlintegrationConstants.PACAKAGE_ACTIVITY_P);
      if (pickedUp != null) {
        stringObjectMap.put(BlintegrationConstants.SHIP_TIME_STAMP, convertIntoDate(convertUPSDateTime(pickedUp.getDate(), pickedUp.getTime())));
      }
    }
    if (Objects.nonNull((stringObjectMap.get(BlintegrationConstants.SHIP_TIME_STAMP)))) {
       ActivityType inTransit = null;
      if(CollectionUtils.isNotEmpty(pkg.getActivity())) {
        inTransit = findFirstActivity(pkg.getActivity());
      }
        if (inTransit != null) {
          stringObjectMap.put(BlintegrationConstants.SHIP_TIME_STAMP,
                  convertIntoDate(convertUPSDateTime(inTransit.getDate(), inTransit.getTime())));

      }
    }
  }

  /**
   * This method created to convert delivery details from response
   * @param deliveryDetail package delivery date
   * @param stringObjectMap results to be updated
   */
  private void convertDeliveryDetailsFromResponse(final DeliveryDetailType deliveryDetail,
     final  Map<String, Object> stringObjectMap) {
    final String type = deliveryDetail.getType().getCode();
    if (BlintegrationConstants.DELIVERY_DETAIL_ONE.equals(type)) {
      stringObjectMap.put(BlintegrationConstants.DELIVERY_TIME_STAMP ,  convertIntoDate(convertUPSDateTime(deliveryDetail.getDate(),
          deliveryDetail.getTime())));
    } else if (BlintegrationConstants.DELIVERY_DETAIL_TWO.equals(type)) {
      stringObjectMap.put(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP, convertIntoDate(convertUPSDateTime(deliveryDetail.getDate(),
          deliveryDetail.getTime())));
    } else if (BlintegrationConstants.DELIVERY_DETAIL_THREE.equals(type)) {
      stringObjectMap.put(BlintegrationConstants.SCHEDULED_DELIVERY_TIME_STAMP, convertIntoDate(convertUPSDateTime(deliveryDetail.getDate(),
          deliveryDetail.getTime())));
    } else if (BlintegrationConstants.DELIVERY_DETAIL_FOUR.equals(type)) {
      stringObjectMap.put(BlintegrationConstants.RESCHEDULED_DELIVERY_TIME_STAMP ,
              convertIntoDate(convertUPSDateTime(deliveryDetail.getDate(), deliveryDetail.getTime())));
    }
  }

  /**
   * This method created to convert shipment response
   * @param stringObjectMap results to be updated
   * @param shipmentType shipment type of package
   */
  private void convertShipmentResponse(final Map<String, Object> stringObjectMap, final ShipmentType shipmentType){
    stringObjectMap.put(BlintegrationConstants.TRACKING_NUMBER , shipmentType.getInquiryNumber().getValue());
    if (Objects.nonNull(shipmentType.getShipmentWeight())) {
      stringObjectMap.put(BlintegrationConstants.SHIPMENT_WEIGHT, shipmentType.getShipmentWeight().getWeight());
    }
    stringObjectMap.put(BlintegrationConstants.SHIP_TIME_STAMP, convertIntoDate(convertUPSDateTime(shipmentType.getPickupDate(), null)));
    stringObjectMap.put(BlintegrationConstants.A_PACKAGE_WAS_DELIVERED, BlintegrationConstants.ZERO);
    stringObjectMap.put(BlintegrationConstants.A_PACKAGE_WAS_DELIVERED_ON, StringUtils.EMPTY);
  }

  /**
   * This method created to convert status from response
   * @param stringObjectMap results to be updated
   * @param lastActivity last activity from ActivityType
   */
  private void convertStatusResponse(final Map<String, Object> stringObjectMap,
     final ActivityType lastActivity){
    stringObjectMap.put(BlintegrationConstants.STATUS_TYPE, lastActivity.getStatus().getType());
    stringObjectMap.put(BlintegrationConstants.STATUS_DESCRIPTION, lastActivity.getStatus().getDescription());
    stringObjectMap.put(BlintegrationConstants.STATUS_CODE, lastActivity.getStatus().getCode());
    stringObjectMap.put(BlintegrationConstants.ACTIVITY_TIME_STAMP ,  convertIntoDate(convertUPSDateTime(lastActivity.getDate(), lastActivity
        .getTime())));
  }

  /**
   * This method created to covert UPS Date
   * @param upsDate expected date of delivery
   * @param upsTime expected time
   * @return String string
   */

  private String convertUPSDateTime(final String upsDate, String upsTime) {
    String newDateTime = StringUtils.EMPTY;
    if (StringUtils.isBlank(upsDate)) {
      return newDateTime;
    }
    if (StringUtils.isBlank(upsTime)) {
      upsTime = BlintegrationConstants.DEFAULT;
    }
    try {
      newDateTime = upsDate + StringUtils.SPACE + upsTime;
    } catch (Exception ex) {
      newDateTime = StringUtils.EMPTY;
    }
    return newDateTime;
  }

  /**
   * This method created to find the first activity from response
   * @param activities list of activities from response
   * @return ActivityType to be updated
   */
  private ActivityType findFirstActivity(final List<ActivityType> activities) {
    ActivityType found = null;
      for (final ActivityType activity : activities) {
        final String activityDateTime = convertUPSDateTime(activity.getDate(), activity.getTime());
        if (activity.getStatus() != null) {
          if (found == null) {
            found = activity;
          } else {
            if (convertUPSDateTime(found.getDate(), found.getTime()).length() != 19 || activityDateTime.compareTo(convertUPSDateTime(found.getDate(), found.getTime())) < 0) {
              found = activity;
            }
          }
        }
      }
    return found;
  }

  /**
   * This method created to find the relevant activity
   * @param activities ist of activiies from response
   * @return ActivityType activity types
   */
  private ActivityType findLastRelevantActivity(final List<ActivityType> activities) {
    ActivityType found = null;
      for (final ActivityType activity : activities) {
        final String activityDateTime = convertUPSDateTime(activity.getDate(), activity.getTime());
        if (null != activity.getStatus()) {
          if (found == null) {
            found = activity;
          } else {
            final String foundActivityDateTime = convertUPSDateTime(found.getDate(), found.getTime());
            if (foundActivityDateTime.length() != 19 || activityDateTime.compareTo(foundActivityDateTime) > 0) {
              found = activity;
            }
          }
        }
      }
    return found;
  }

  /**
   * This method created to find the activity type
   * @param activities activities
   * @param type type
   * @return ActivityType
   */
  private ActivityType findActivityType(final List<ActivityType> activities, final String type) {
    ActivityType found = null;
    if (activities != null && StringUtils.isNotBlank(type)) {
      for (ActivityType activity : activities) {
        if (activity.getStatus() != null && type.equals(activity.getStatus().getType())) {
          return activity;
        }
      }
    }

    return found;
  }

  private Date convertIntoDate(final String value){
    if(Objects.nonNull(value))
    { try {
      SimpleDateFormat formatter= new SimpleDateFormat(BlintegrationConstants.UPS_DATE_FORMAT);
      final String activityDate = String.valueOf(value);
      return formatter.parse(activityDate.split(" ")[0]);
    } catch (ParseException e) {
      BlLogger.logMessage(LOG , Level.ERROR , "Error while converting date" , e);
    }
    }
  return null;
  }
}
