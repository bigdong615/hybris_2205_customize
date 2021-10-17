package com.bl.integration.services.impl;

import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.fedex.tracking.pojo.Address;
import com.bl.integration.fedex.tracking.pojo.ClientDetail;
import com.bl.integration.fedex.tracking.pojo.CompletedTrackDetail;
import com.bl.integration.fedex.tracking.pojo.TrackDetail;
import com.bl.integration.fedex.tracking.pojo.TrackEvent;
import com.bl.integration.fedex.tracking.pojo.TrackIdentifierType;
import com.bl.integration.fedex.tracking.pojo.TrackPackageIdentifier;
import com.bl.integration.fedex.tracking.pojo.TrackPortType;
import com.bl.integration.fedex.tracking.pojo.TrackReply;
import com.bl.integration.fedex.tracking.pojo.TrackRequest;
import com.bl.integration.fedex.tracking.pojo.TrackRequestProcessingOptionType;
import com.bl.integration.fedex.tracking.pojo.TrackSelectionDetail;
import com.bl.integration.fedex.tracking.pojo.TrackServiceLocator;
import com.bl.integration.fedex.tracking.pojo.TrackingDateOrTimestamp;
import com.bl.integration.fedex.tracking.pojo.TransactionDetail;
import com.bl.integration.fedex.tracking.pojo.VersionId;
import com.bl.integration.fedex.tracking.pojo.WebAuthenticationCredential;
import com.bl.integration.fedex.tracking.pojo.WebAuthenticationDetail;
import com.bl.integration.services.BlTrackWebService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class DefaultBlTrackWebServiceImpl implements BlTrackWebService {

  private static final Logger LOG = Logger.getLogger(DefaultBlTrackWebServiceImpl.class);
  public static final String DATE_FORMAT = "yyyy-MM-dd";


  @Override
  public Map<String, Object> trackService(final AbstractOrderModel abstractOrderModel , final
      PackagingInfoModel packagingInfoModel) {

    final Map<String, Object> results = new HashMap<>();
    if (Objects.nonNull(abstractOrderModel)) {
      try {
        final TrackRequest trackRequest = new TrackRequest();
        trackRequest.setClientDetail(getCliendDetailsForFedex());
        trackRequest.setWebAuthenticationDetail(getWebAuthenticationDetailsForFedex());
        trackRequest.setTransactionDetail(getTransactionDetailForFedex(abstractOrderModel));
        trackRequest.setVersion(getVersionIdForFedex());
        getTrackageIndentifierNumber(trackRequest);
        final TrackServiceLocator service = new TrackServiceLocator();
        final TrackPortType port;
        updateEndPoint(service);
        port = service.getTrackServicePort();
        final TrackReply response  = port.track(trackRequest);
        convertResponse(response , results);
        BlLogger.logMessage(LOG, Level.INFO, response.getCompletedTrackDetails()[0].getTrackDetails()[0].getDatesOrTimes()[0].getType().getValue());
      } catch (final Exception e) {
        BlLogger.logMessage(LOG, Level.ERROR, "Error While Calling Track service", e);
        return new LinkedHashMap<>();
      }
    }

    results.put("upsScrapeServiceType" , "fedex");
    return results;
  }



  private ClientDetail getCliendDetailsForFedex() {
    final ClientDetail clientDetail = new ClientDetail();
    clientDetail.setAccountNumber(getValuesFromProperty(BlintegrationConstants.FEDEX_ACCOUNT_NUMBER));
    clientDetail.setMeterNumber(getValuesFromProperty(BlintegrationConstants.FEDEX_METER_NUMBER));
    return clientDetail;
  }

  private WebAuthenticationDetail getWebAuthenticationDetailsForFedex() {
    final WebAuthenticationCredential userCredential = new WebAuthenticationCredential();
    final WebAuthenticationCredential parentCredential = new WebAuthenticationCredential();
    userCredential.setKey(getValuesFromProperty(BlintegrationConstants.FEDEX_USER_API_KEY));
    userCredential
        .setPassword(getValuesFromProperty(BlintegrationConstants.FEDEX_USER_API_PASSWORD));
    parentCredential.setKey(getValuesFromProperty(BlintegrationConstants.FEDEX_PARENT_API_KEY));
    parentCredential
        .setPassword(getValuesFromProperty(BlintegrationConstants.FEDEX_PARENT_API_PASSWORD));
    return new WebAuthenticationDetail(parentCredential, userCredential);
  }


  private TransactionDetail getTransactionDetailForFedex(
      final AbstractOrderModel abstractOrderModel) {
    final TransactionDetail transactionDetail = new TransactionDetail();
    final String transactionId = "00005003"
        + BlintegrationConstants.HYPHEN
        + BlintegrationConstants.IN_BOUND_OR_OUT_BOUND
        + BlintegrationConstants.HYPHEN
        + System.currentTimeMillis();
    transactionDetail.setCustomerTransactionId(transactionId);
    return transactionDetail;
  }

  private VersionId getVersionIdForFedex() {
    return new VersionId(BlintegrationConstants.TRCK, 19, 0, 0);
  }

  private void getTrackageIndentifierNumber(final TrackRequest trackRequest) {
    final TrackPackageIdentifier packageIdentifier = new TrackPackageIdentifier();
    final TrackSelectionDetail selectionDetail = new TrackSelectionDetail();
    packageIdentifier.setValue("744802117499787"); // Used for testing
    packageIdentifier.setType(TrackIdentifierType.TRACKING_NUMBER_OR_DOORTAG);
    selectionDetail.setPackageIdentifier(packageIdentifier);
    trackRequest.setSelectionDetails(new TrackSelectionDetail[]{selectionDetail});
    TrackRequestProcessingOptionType processingOption = TrackRequestProcessingOptionType.INCLUDE_DETAILED_SCANS;
    trackRequest.setProcessingOptions(new TrackRequestProcessingOptionType[]{processingOption});
  }


  private void updateEndPoint(final TrackServiceLocator serviceLocator) {
    serviceLocator.setTrackServicePortEndpointAddress(
        getValuesFromProperty(BlintegrationConstants.FEDEX_API_URL));
  }

  private String getValuesFromProperty(final String key) {
    return StringUtils.isNotBlank(Config.getParameter(key)) ? Config.getParameter(key)
        : StringUtils.EMPTY;
  }

  private void convertResponse(final TrackReply response , final Map<String, Object> responseResults) throws ParseException {
    if(Objects.nonNull(response)) {
      final CompletedTrackDetail[] completedTrackDetails = response.getCompletedTrackDetails();
      if(ArrayUtils.isNotEmpty(completedTrackDetails)) {
        getTrackDetailFromResponse(responseResults, completedTrackDetails);
      }
    }
  }

  private void getTrackDetailFromResponse(final Map<String, Object> responseResults,
      final CompletedTrackDetail[] completedTrackDetails) throws ParseException {
    for (final CompletedTrackDetail completedTrackDetail : completedTrackDetails) {
      if (ArrayUtils.isNotEmpty(completedTrackDetail.getTrackDetails())) {
        for (final TrackDetail trackDetails : completedTrackDetail.getTrackDetails()) {
          getPackageDetails(responseResults, trackDetails);
          responseResults
              .put("TrackingNumber", getValuesFromResponse(trackDetails.getTrackingNumber()));
          responseResults.put("TrackingNumberUniqueIdentifier",
              trackDetails.getTrackingNumberUniqueIdentifier());
          getStatusFromResponse(responseResults, trackDetails);
          responseResults.put("ServiceType", Objects.nonNull(trackDetails.getService()) ?
              trackDetails.getService().getType() : StringUtils.EMPTY);
          responseResults.put("ShipmentWeight", Objects.nonNull(trackDetails.getShipmentWeight()) ?
              trackDetails.getShipmentWeight() : StringUtils.EMPTY);
          getTimeStampFromResponse(responseResults, trackDetails);
          getDestinationAddressFromResponse(responseResults, trackDetails);
          getTrackDetailsFromResponse(responseResults, trackDetails);
        }
      }
    }
  }

  private void getStatusFromResponse(final Map<String, Object> responseResults, final TrackDetail trackDetails) {
    responseResults.put("StatusCode", Objects.nonNull( trackDetails.getService()) ?
        getValuesFromResponse(trackDetails.getService().getType()) : StringUtils.EMPTY);
    responseResults.put("StatusDescription",  Objects.nonNull( trackDetails.getStatusDetail()) ?
        getValuesFromResponse(trackDetails.getStatusDetail().getDescription())
        :StringUtils.EMPTY);
  }

  private void getPackageDetails(final Map<String, Object> responseResults, final TrackDetail trackDetails) {
    responseResults.put("PackageSequenceNumber",
        getValuesFromResponse(String.valueOf(trackDetails.getPackageSequenceNumber())));
    responseResults.put("PackageCount",
        getValuesFromResponse(String.valueOf(trackDetails.getPackageCount())));
    responseResults.put("Packaging", Objects.nonNull(trackDetails.getPackaging())
        ? trackDetails.getPackaging() : StringUtils.EMPTY);
    responseResults.put("PackageWeight", Objects.nonNull(trackDetails.getPackageWeight()) ?
        trackDetails.getPackageWeight() : StringUtils.EMPTY);
  }

  private void getTimeStampFromResponse(final Map<String, Object> responseResults, final TrackDetail trackDetails)
      throws ParseException {
    if(ArrayUtils.isNotEmpty(trackDetails.getDatesOrTimes())) {
      for (final TrackingDateOrTimestamp trackingDateOrTimestamp : trackDetails
          .getDatesOrTimes()) {
        if (StringUtils
            .equalsIgnoreCase("SHIP", Objects.nonNull(trackingDateOrTimestamp.getType()) ? trackingDateOrTimestamp.getType().getValue() : StringUtils.EMPTY)) {
          responseResults.put("ShipTimestamp",
              convertTime(trackingDateOrTimestamp.getDateOrTimestamp()));
        }
        if (StringUtils.equalsIgnoreCase("ESTIMATED_DELIVERY",
            Objects.nonNull(trackingDateOrTimestamp.getType())? trackingDateOrTimestamp.getType().getValue() : StringUtils.EMPTY)) {
          responseResults.put("EstimatedDeliveryTimestamp",
              convertTime(trackingDateOrTimestamp.getDateOrTimestamp()));
        }
      }
    }
  }

  private void getDestinationAddressFromResponse(final Map<String, Object> responseResults, final TrackDetail trackDetails) {
    final Address destinationAddress = trackDetails.getDestinationAddress();
    if (destinationAddress != null) {
      final Map<String, String> address = new LinkedHashMap<>();
      address.put("City", destinationAddress.getCity());
      address.put("PostalCode", destinationAddress.getPostalCode());
      address.put("CountryCode", destinationAddress.getCountryCode());
      responseResults.put("DestinationAddress", address);
    }
  }

  private void getTrackDetailsFromResponse(final Map<String, Object> responseResults, final TrackDetail trackDetails) {
    if (ArrayUtils.isNotEmpty(trackDetails.getEvents())) {
      final List<Map<String, Object>> list = new ArrayList<>();
      for (int i = 0; i < trackDetails.getEvents().length; i++) {
        final Map<String, Object> map = new LinkedHashMap<>();
        final TrackEvent trackEvent = trackDetails.getEvents()[i];
        if (Objects.nonNull(trackEvent)) {
          map.put("Timestamp", trackEvent.getTimestamp().getTime());
          map.put("Description", trackEvent.getEventDescription());
          Address address = trackEvent.getAddress();
          if (Objects.nonNull(address)) {
            final Map<String, String> trackAddress = new LinkedHashMap<>();
            trackAddress.put("City", address.getCity());
            trackAddress.put("State", address.getStateOrProvinceCode());
            map.put("Address", trackAddress);
          }
          list.add(map);
        }
      }
      responseResults.put("TrackEvents", list);
    }
  }

  private Date convertTime(final String dateOrTimestamp) throws ParseException {
    final SimpleDateFormat formatter = new SimpleDateFormat(DATE_FORMAT);
    Calendar calendar = new GregorianCalendar(TimeZone.getTimeZone("UTC"));
    calendar.setTime(formatter.parse(dateOrTimestamp));
    return calendar.getTime();
  }


  private String getValuesFromResponse(final String value){
    return StringUtils.isBlank(value) ? StringUtils.EMPTY : value;
  }


}
