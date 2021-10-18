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
    stringObjectMap.put("upsScrapeServiceType" , "UPS");
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
   * @param packagingInfoModel
   */
  private TrackRequest getTrackRequestForUPS(final PackagingInfoModel packagingInfoModel){
    final TrackRequest trackRequest = new TrackRequest();
    final RequestType request = new RequestType();
    final List<String> requestOption = request.getRequestOption();
    requestOption.add("1");
    request.setRequestOption(requestOption);
    trackRequest.setRequest(request);
    trackRequest.setInquiryNumber(StringUtils.isBlank(packagingInfoModel.getTrackingNumber()) ? StringUtils.EMPTY : packagingInfoModel.getTrackingNumber());
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
                  stringObjectMap.put("ServiceType", service.getCode());
                  stringObjectMap.put("ServiceDescription", service.getDescription());
                }

                stringObjectMap.put("InquiryNumber" , shipmentType.getInquiryNumber().getValue());

                if (Objects.nonNull(shipmentType.getShipmentWeight())) {
                  stringObjectMap.put("ShipmentWeight", shipmentType.getShipmentWeight().getWeight());
                }
                stringObjectMap.put("ShipTimestamp", convertUPSDateTime(shipmentType.getPickupDate(), null));
                stringObjectMap.put("APackageWasDelivered", "0");
                stringObjectMap.put("APackageWasDeliveredOn", "");

                int packageCount = 0;
                int activityCount = 0;
                if (shipmentType.getPackage() != null && shipmentType.getPackage().size() > 0) {
                  for (PackageType pkg : shipmentType.getPackage()) {
                    packageCount++;
                    if (pkg.getDeliveryDetail() != null && pkg.getDeliveryDetail().size() > 0) {
                      for (DeliveryDetailType deliveryDetail : pkg.getDeliveryDetail()) {
                        String type = deliveryDetail.getType().getCode();
                        if ("01".equals(type)) {
                          stringObjectMap.put("DeliveryTimestamp", convertUPSDateTime(deliveryDetail.getDate(),
                              deliveryDetail.getTime()));
                        } else if ("02".equals(type)) {
                          stringObjectMap.put("EstimatedDeliveryTimestamp", convertUPSDateTime(deliveryDetail.getDate(),
                              deliveryDetail.getTime()));
                        } else if ("03".equals(type)) {
                          stringObjectMap.put("ScheduledDeliveryTimestamp", convertUPSDateTime(deliveryDetail.getDate(),
                              deliveryDetail.getTime()));
                        } else if ("04".equals(type)) {
                          stringObjectMap.put("RescheduledDeliveryTimestamp",
                              convertUPSDateTime(deliveryDetail.getDate(), deliveryDetail.getTime()));
                        }
                      }
                    }
                    if (pkg.getActivity() != null) {
                      activityCount += pkg.getActivity().size();
                    }
                    // Find the newest activity
                    ActivityType lastActivity = findLastRelevantActivity(pkg.getActivity());
                    if (lastActivity != null) {

                      stringObjectMap.put("StatusType", lastActivity.getStatus().getType());

                      stringObjectMap.put("StatusDescription", lastActivity.getStatus().getDescription());

                      stringObjectMap.put("StatusCode", lastActivity.getStatus().getCode());
                      stringObjectMap.put("ActivityTimeStamp", convertUPSDateTime(lastActivity.getDate(), lastActivity
                          .getTime()));
                    }

                    ActivityType delivered = findActivityType(pkg.getActivity(), "D");
                    if (delivered != null) {
                      stringObjectMap.put("APackageWasDelivered", "1");
                      stringObjectMap.put("APackageWasDeliveredOn", convertUPSDateTime(delivered.getDate(), delivered
                          .getTime()));
                    }

                    if (StringUtils.isBlank((String) stringObjectMap.get("ShipTimestamp"))) {
                      ActivityType pickedUp = findActivityType(pkg.getActivity(), "P");
                      if (pickedUp != null) {
                        stringObjectMap.put("ShipTimestamp", convertUPSDateTime(pickedUp.getDate(), pickedUp.getTime()));
                      }
                    }
                    if (StringUtils.isBlank((String) stringObjectMap.get("ShipTimestamp"))) {
                      ActivityType inTransit = findFirstActivity(pkg.getActivity(), "I");
                      if (inTransit != null) {
                        stringObjectMap.put("ShipTimestamp", convertUPSDateTime(inTransit.getDate(), inTransit.getTime()));
                      } else {

                      }
                    }

                  }
                }
                stringObjectMap.put("PackageCount", packageCount);
                stringObjectMap.put("ActivityCount", activityCount);

              }


            }
    }

  }


  private String convertUPSDateTime(String upsDate, String upsTime) {
    String newDateTime = "";
    if (StringUtils.isBlank(upsDate)) {
      return newDateTime;
    }
    if (StringUtils.isBlank(upsTime)) {
      upsTime = "000000";
    }
    try {
      newDateTime = upsDate + " " + upsTime;
    } catch (Exception ex) {
      newDateTime = "";
    }
    return newDateTime;
  }

  private ActivityType findFirstActivity(List<ActivityType> activities, String type) {
    ActivityType found = null;
    if (activities != null) {
      for (ActivityType activity : activities) {
        String statusType = null;
        String statusCode = null;
        String statusDescription = null;
        String activityDateTime = convertUPSDateTime(activity.getDate(), activity.getTime());
        if (activity.getStatus() != null) {
          statusType = activity.getStatus().getType();
          statusCode = activity.getStatus().getCode();
          statusDescription = activity.getStatus().getDescription();
          if (StringUtils.isBlank(statusType) || !type.equals(statusType)) {
            continue;
          }

          // Fill it if we have nothing
          if (found == null) {
            found = activity;
          } else {
            String foundActivityDateTime = convertUPSDateTime(found.getDate(), found.getTime());
            if (activityDateTime.length() != 19) {

              continue;
            }
            if (foundActivityDateTime.length() != 19) {

              found = activity;
            } else if (activityDateTime.compareTo(foundActivityDateTime) < 0) {

              found = activity;
            } else {

            }
          }
        }
      }
    }

    return found;
  }


  private ActivityType findLastRelevantActivity(List<ActivityType> activities) {
    ActivityType found = null;
    if (activities != null) {
      for (ActivityType activity : activities) {
        String statusType = null;
        String statusCode = null;
        String statusDescription = null;
        String activityDateTime = convertUPSDateTime(activity.getDate(), activity.getTime());
        if (activity.getStatus() != null) {
          statusType = activity.getStatus().getType();
          statusCode = activity.getStatus().getCode();
          statusDescription = activity.getStatus().getDescription();

          if (found == null) {
            found = activity;
          } else {
            String foundActivityDateTime = convertUPSDateTime(found.getDate(), found.getTime());
            if (activityDateTime.length() != 19) {

              continue;
            }
            if (foundActivityDateTime.length() != 19) {

              found = activity;
            } else if (activityDateTime.compareTo(foundActivityDateTime) > 0) {

              found = activity;
            } else {

            }
          }
        }
      }
    }

    return found;
  }


  private ActivityType findActivityType(List<ActivityType> activities, String type) {
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
