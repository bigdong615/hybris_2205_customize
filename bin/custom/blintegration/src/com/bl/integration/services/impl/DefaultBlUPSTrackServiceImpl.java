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
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import javax.xml.ws.BindingProvider;
import org.apache.commons.collections.CollectionUtils;
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
    BlLogger.logMessage(LOG  , Level.INFO , "Started Performing UPS Scrape for UPS service");
    final Map<String, Object> stringObjectMap = new HashMap<>();
      try {
        final TrackService trackService = new TrackService();
        TrackPortType trackPortType = trackService.getTrackPort();

        final BindingProvider bindingProvider = (BindingProvider)trackPortType;
        getEndPointURLForUPS(bindingProvider);
        TrackResponse response = trackPortType.processTrack(getTrackRequestForUPS(packagingInfoModel), getSecurityDetailsForUPS());
        convertResponse(response , stringObjectMap);
      } catch(Exception e) {
        BlLogger.logMessage(LOG , Level.ERROR , "Error while executing trackUPSService" , e.getMessage());
      }
    stringObjectMap.put(BlintegrationConstants.SCRAPE_TYPE , BlintegrationConstants.UPS_TYPE);
    BlLogger.logMessage(LOG  , Level.INFO , "Finished UPS Scrape for UPS service");
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
   * @param packagingInfoModel  packagingInfoModel
   * @return TrackRequest
   */
  private TrackRequest getTrackRequestForUPS(final PackagingInfoModel packagingInfoModel){
    final TrackRequest trackRequest = new TrackRequest();
    final RequestType request = new RequestType();
    final List<String> requestOption = request.getRequestOption();
    requestOption.add(BlintegrationConstants.REQUEST_OPTION_NUMBER);
    request.setRequestOption(requestOption);
    trackRequest.setRequest(request);
    trackRequest.setInquiryNumber(StringUtils.isBlank(packagingInfoModel.getTrackingNumber()) ? StringUtils.EMPTY : packagingInfoModel.getTrackingNumber());
    trackRequest.setTrackingOption(BlintegrationConstants.TRACKING_OPTION);
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

  /**
   * This method created to convert response
   * @param response response
   * @param stringObjectMap map
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

  private void convertResponseFromResults(ShipmentType shipmentType, int activityCount, int packageCount, Map<String, Object> stringObjectMap) {
    if (shipmentType.getPackage() != null && shipmentType.getPackage().size() > 0) {
      for (PackageType pkg : shipmentType.getPackage()) {
        packageCount++;
        if (pkg.getDeliveryDetail() != null && pkg.getDeliveryDetail().size() > 0) {
          for (DeliveryDetailType deliveryDetail : pkg.getDeliveryDetail()) {
            convertDeliveryDetailsFromResponse(deliveryDetail , stringObjectMap);
          }
        }
       performConvertResponse(stringObjectMap , pkg , activityCount);
      }
    }
  }

  private void performConvertResponse(
      final Map<String, Object> stringObjectMap, final PackageType pkg, int activityCount) {
    if (pkg.getActivity() != null) {
      activityCount += pkg.getActivity().size();
    }
    // Find the newest activity
    ActivityType lastActivity = findLastRelevantActivity(pkg.getActivity());
    if (lastActivity != null) {
      convertStatusResponse(stringObjectMap , lastActivity);
    }
    covertPacakageResponse(stringObjectMap, pkg);
    BlLogger.logMessage(LOG , Level.INFO , "activity count" , String.valueOf(activityCount));
  }

  private void covertPacakageResponse(
      Map<String, Object> stringObjectMap, PackageType pkg) {
    ActivityType delivered = findActivityType(pkg.getActivity(), BlintegrationConstants.PACAKAGE_ACTIVITY_D);
    if (delivered != null) {
      stringObjectMap.put(BlintegrationConstants.A_PACKAGE_WAS_DELIVERED, BlintegrationConstants.REQUEST_OPTION_NUMBER);
      stringObjectMap.put(BlintegrationConstants.A_PACKAGE_WAS_DELIVERED_ON, convertUPSDateTime(delivered.getDate(), delivered
          .getTime()));
    }

    if (StringUtils.isBlank((String) stringObjectMap.get(BlintegrationConstants.SHIP_TIME_STAMP))) {
      ActivityType pickedUp = findActivityType(pkg.getActivity(), BlintegrationConstants.PACAKAGE_ACTIVITY_P);
      if (pickedUp != null) {
        stringObjectMap.put(BlintegrationConstants.SHIP_TIME_STAMP, convertUPSDateTime(pickedUp.getDate(), pickedUp.getTime()));
      }
    }
    if (StringUtils.isBlank((String) stringObjectMap.get(BlintegrationConstants.SHIP_TIME_STAMP))) {
      ActivityType inTransit = findFirstActivity(pkg.getActivity(), BlintegrationConstants.PACAKAGE_ACTIVITY_I);
      if (inTransit != null) {
        stringObjectMap.put(BlintegrationConstants.SHIP_TIME_STAMP ,  convertUPSDateTime(inTransit.getDate(), inTransit.getTime()));
      }
    }
  }

  private void convertDeliveryDetailsFromResponse(final DeliveryDetailType deliveryDetail,
     final  Map<String, Object> stringObjectMap) {
    final String type = deliveryDetail.getType().getCode();
    if (BlintegrationConstants.DELIVERY_DETAIL_ONE.equals(type)) {
      stringObjectMap.put(BlintegrationConstants.DELIVERY_TIME_STAMP ,  convertUPSDateTime(deliveryDetail.getDate(),
          deliveryDetail.getTime()));
    } else if (BlintegrationConstants.DELIVERY_DETAIL_TWO.equals(type)) {
      stringObjectMap.put(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP, convertUPSDateTime(deliveryDetail.getDate(),
          deliveryDetail.getTime()));
    } else if (BlintegrationConstants.DELIVERY_DETAIL_THREE.equals(type)) {
      stringObjectMap.put(BlintegrationConstants.SCHEDULED_DELIVERY_TIME_STAMP, convertUPSDateTime(deliveryDetail.getDate(),
          deliveryDetail.getTime()));
    } else if (BlintegrationConstants.DELIVERY_DETAIL_FOUR.equals(type)) {
      stringObjectMap.put(BlintegrationConstants.RESCHEDULED_DELIVERY_TIME_STAMP ,
          convertUPSDateTime(deliveryDetail.getDate(), deliveryDetail.getTime()));
    }
  }

  private void convertShipmentResponse(final Map<String, Object> stringObjectMap,
      final ShipmentType shipmentType){
    stringObjectMap.put(BlintegrationConstants.TRACKING_NUMBER , shipmentType.getInquiryNumber().getValue());
    if (Objects.nonNull(shipmentType.getShipmentWeight())) {
      stringObjectMap.put(BlintegrationConstants.SHIPMENT_WEIGHT, shipmentType.getShipmentWeight().getWeight());
    }
    stringObjectMap.put(BlintegrationConstants.SHIP_TIME_STAMP, convertUPSDateTime(shipmentType.getPickupDate(), null));
    stringObjectMap.put(BlintegrationConstants.A_PACKAGE_WAS_DELIVERED, BlintegrationConstants.ZERO);
    stringObjectMap.put(BlintegrationConstants.A_PACKAGE_WAS_DELIVERED_ON, StringUtils.EMPTY);
  }

  private void convertStatusResponse(final Map<String, Object> stringObjectMap,
     final  ActivityType lastActivity){
    stringObjectMap.put(BlintegrationConstants.STATUS_TYPE, lastActivity.getStatus().getType());
    stringObjectMap.put(BlintegrationConstants.STATUS_DESCRIPTION, lastActivity.getStatus().getDescription());
    stringObjectMap.put(BlintegrationConstants.STATUS_CODE, lastActivity.getStatus().getCode());
    stringObjectMap.put(BlintegrationConstants.ACTIVITY_TIME_STAMP ,  convertUPSDateTime(lastActivity.getDate(), lastActivity
        .getTime()));
  }

  /**
   * This method created to covert UPS Date
   * @param upsDate  upsDate
   * @param upsTime upsTime
   * @return String
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
   * @param activities
   * @param type
   * @return
   */
  private ActivityType findFirstActivity(final List<ActivityType> activities, final String type) {
    ActivityType found = null;
    if (activities != null) {
      for (ActivityType activity : activities) {
        String statusType = null;
        String activityDateTime = convertUPSDateTime(activity.getDate(), activity.getTime());
        if (activity.getStatus() != null) {
          statusType = activity.getStatus().getType();
          if (StringUtils.isBlank(statusType) || !type.equals(statusType)) {
            continue;
          }
          if (found == null) {
            found = activity;
          } else {
            String foundActivityDateTime = convertUPSDateTime(found.getDate(), found.getTime());
            if (activityDateTime.length() != 19) {
              continue;
            }
            if (foundActivityDateTime.length() != 19 || activityDateTime.compareTo(foundActivityDateTime) < 0) {
              found = activity;
            }
          }
        }
      }
    }

    return found;
  }




  /**
   * This method created to find the relevant activity
   * @param activities activities
   * @return ActivityType
   */
  private ActivityType findLastRelevantActivity(final List<ActivityType> activities) {
    ActivityType found = null;
    if (activities != null) {
      for (ActivityType activity : activities) {
        String activityDateTime = convertUPSDateTime(activity.getDate(), activity.getTime());
        if (activity.getStatus() != null) {
          if (found == null) {
            found = activity;
          } else {
            String foundActivityDateTime = convertUPSDateTime(found.getDate(), found.getTime());
            if (activityDateTime.length() != 19) {
              continue;
            }
            if (foundActivityDateTime.length() != 19 || activityDateTime.compareTo(foundActivityDateTime) > 0) {
              found = activity;
            }
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



}
