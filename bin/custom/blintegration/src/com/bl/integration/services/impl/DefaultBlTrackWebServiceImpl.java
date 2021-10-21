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

/**
 * This class created to track the UPS service
 * @author Manikandan
 */
public class DefaultBlTrackWebServiceImpl implements BlTrackWebService {

  private static final Logger LOG = Logger.getLogger(DefaultBlTrackWebServiceImpl.class);

  /**
   * {@inheritDoc}
   */
  @Override
  public Map<String, Object> trackService(final AbstractOrderModel abstractOrderModel , final
      PackagingInfoModel packagingInfoModel) {

    final Map<String, Object> results = new HashMap<>();
    if (Objects.nonNull(abstractOrderModel)) {
      BlLogger.logMessage(LOG , Level.INFO , "Started performing UPS Scrape for Fedex Service");
      try {
        final TrackRequest trackRequest = new TrackRequest();
        trackRequest.setClientDetail(getCliendDetailsForFedex());
        trackRequest.setWebAuthenticationDetail(getWebAuthenticationDetailsForFedex());
        trackRequest.setTransactionDetail(getTransactionDetailForFedex(abstractOrderModel));
        trackRequest.setVersion(getVersionIdForFedex());
        getTrackageIndentifierNumber(trackRequest , packagingInfoModel);
        final TrackServiceLocator service = new TrackServiceLocator();
        final TrackPortType port;
        updateEndPoint(service);
        port = service.getTrackServicePort();
        final TrackReply response  = port.track(trackRequest);
        convertResponse(response , results);
      } catch (final Exception e) {
        BlLogger.logMessage(LOG, Level.ERROR, "Error While Calling Track service", e);
        return new LinkedHashMap<>();
      }
    }

    results.put(BlintegrationConstants.SCRAPE_TYPE , BlintegrationConstants.FEDEX_TYPE);
    BlLogger.logMessage(LOG , Level.INFO , " Finished performing UPS Scrape for Fedex Service");

    return results;
  }

  /**
   * This method craeted to set the Fedex client details
   * @return ClientDetail
   */
  private ClientDetail getCliendDetailsForFedex() {
    final ClientDetail clientDetail = new ClientDetail();
    clientDetail.setAccountNumber(getValuesFromProperty(BlintegrationConstants.FEDEX_ACCOUNT_NUMBER));
    clientDetail.setMeterNumber(getValuesFromProperty(BlintegrationConstants.FEDEX_METER_NUMBER));
    return clientDetail;
  }

  /**
   * This method craeted to set the Fedex web  details
   * @return WebAuthenticationDetail
   */
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


  /**
   * This method created to Set the Transaction details for fedex
   * @param abstractOrderModel abstractOrderModel
   * @return TransactionDetail
   */
  private TransactionDetail getTransactionDetailForFedex(
      final AbstractOrderModel abstractOrderModel) {
    final TransactionDetail transactionDetail = new TransactionDetail();
    final String transactionId = abstractOrderModel.getCode()
        + BlintegrationConstants.HYPHEN
        + BlintegrationConstants.IN_BOUND_OR_OUT_BOUND
        + BlintegrationConstants.HYPHEN
        + System.currentTimeMillis();
    transactionDetail.setCustomerTransactionId(transactionId);
    return transactionDetail;
  }

  /**
   * This method created to set the version Details
   * @return version ID
   */
  private VersionId getVersionIdForFedex() {
    return new VersionId(BlintegrationConstants.TRCK, 19, 0, 0);
  }

  /**
   * This method created to set the tracking details
   * @param trackRequest trackRequest
   * @param packagingInfoModel
   */
  private void getTrackageIndentifierNumber(final TrackRequest trackRequest,
      PackagingInfoModel packagingInfoModel) {
    final TrackPackageIdentifier packageIdentifier = new TrackPackageIdentifier();
    final TrackSelectionDetail selectionDetail = new TrackSelectionDetail();
    packageIdentifier.setValue(Objects.nonNull(packagingInfoModel.getTrackingNumber())
        ? packagingInfoModel.getTrackingNumber(): StringUtils.EMPTY);
    packageIdentifier.setType(TrackIdentifierType.TRACKING_NUMBER_OR_DOORTAG);
    selectionDetail.setPackageIdentifier(packageIdentifier);
    trackRequest.setSelectionDetails(new TrackSelectionDetail[]{selectionDetail});
    TrackRequestProcessingOptionType processingOption = TrackRequestProcessingOptionType.INCLUDE_DETAILED_SCANS;
    trackRequest.setProcessingOptions(new TrackRequestProcessingOptionType[]{processingOption});
  }


  /**
   * This method created to upadate the End point
   * @param serviceLocator serviceLocator
   */
  private void updateEndPoint(final TrackServiceLocator serviceLocator) {
    serviceLocator.setTrackServicePortEndpointAddress(
        getValuesFromProperty(BlintegrationConstants.FEDEX_API_URL));
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
   * This method created to convert the response
   * @param response response
   * @param responseResults responseResults
   */
  private void convertResponse(final TrackReply response , final Map<String, Object> responseResults) throws ParseException {
    if(Objects.nonNull(response)) {
      final CompletedTrackDetail[] completedTrackDetails = response.getCompletedTrackDetails();
      if(ArrayUtils.isNotEmpty(completedTrackDetails)) {
        getTrackDetailFromResponse(responseResults, completedTrackDetails);
      }
    }
  }

  /**
   * This method created to track the details from response
   * @param responseResults responseResults
   * @param completedTrackDetails completedTrackDetails
   * @throws ParseException ParseException
   */
  private void getTrackDetailFromResponse(final Map<String, Object> responseResults,
      final CompletedTrackDetail[] completedTrackDetails) throws ParseException {
    for (final CompletedTrackDetail completedTrackDetail : completedTrackDetails) {
      if (ArrayUtils.isNotEmpty(completedTrackDetail.getTrackDetails())) {
        for (final TrackDetail trackDetails : completedTrackDetail.getTrackDetails()) {
          getPackageDetails(responseResults, trackDetails);
          responseResults
              .put(BlintegrationConstants.TRACKING_NUMBER, getValuesFromResponse(trackDetails.getTrackingNumber()));
          responseResults.put(BlintegrationConstants.TRACKING_NUMBER_IDENTIFIER,
              trackDetails.getTrackingNumberUniqueIdentifier());
          getStatusFromResponse(responseResults, trackDetails);
          responseResults.put(BlintegrationConstants.SERVICE_TYPE, Objects.nonNull(trackDetails.getService()) ?
              trackDetails.getService().getType() : StringUtils.EMPTY);
          responseResults.put(BlintegrationConstants.SHIPMENT_WEIGHT, Objects.nonNull(trackDetails.getShipmentWeight()) ?
              trackDetails.getShipmentWeight() : StringUtils.EMPTY);
          getTimeStampFromResponse(responseResults, trackDetails);
          getDestinationAddressFromResponse(responseResults, trackDetails);
          getTrackDetailsFromResponse(responseResults, trackDetails);
        }
      }
    }
  }

  /**
   *This method created to get the status from response
   * @param responseResults responseResults
   * @param trackDetails trackDetails
   */
  private void getStatusFromResponse(final Map<String, Object> responseResults, final TrackDetail trackDetails) {
    responseResults.put(BlintegrationConstants.STATUS_CODE, Objects.nonNull( trackDetails.getService()) ?
        getValuesFromResponse(trackDetails.getService().getType()) : StringUtils.EMPTY);
    responseResults.put(BlintegrationConstants.STATUS_DESCRIPTION,  Objects.nonNull( trackDetails.getStatusDetail()) ?
        getValuesFromResponse(trackDetails.getStatusDetail().getDescription())
        :StringUtils.EMPTY);
  }

  /**
   * This method created to get the pacakage details
   * @param responseResults responseResults
   * @param trackDetails trackDetails
   */
  private void getPackageDetails(final Map<String, Object> responseResults, final TrackDetail trackDetails) {
    responseResults.put(BlintegrationConstants.PACAKAGE_SEQUENCE_NUMBER,
        getValuesFromResponse(String.valueOf(trackDetails.getPackageSequenceNumber())));
    responseResults.put(BlintegrationConstants.PACKAGE_COUNT,
        getValuesFromResponse(String.valueOf(trackDetails.getPackageCount())));
    responseResults.put(BlintegrationConstants.PACKAGING, Objects.nonNull(trackDetails.getPackaging())
        ? trackDetails.getPackaging() : StringUtils.EMPTY);
    responseResults.put(BlintegrationConstants.PACKAGE_WEIGHT, Objects.nonNull(trackDetails.getPackageWeight()) ?
        trackDetails.getPackageWeight() : StringUtils.EMPTY);
  }

  /**
   * This method created to get the time stamp from response
   * @param responseResults responseResults
   * @param trackDetails trackDetails
   * @throws ParseException ParseException
   */
  private void getTimeStampFromResponse(final Map<String, Object> responseResults, final TrackDetail trackDetails)
      throws ParseException {
    if(ArrayUtils.isNotEmpty(trackDetails.getDatesOrTimes())) {
      for (final TrackingDateOrTimestamp trackingDateOrTimestamp : trackDetails
          .getDatesOrTimes()) {
        if (StringUtils
            .equalsIgnoreCase(BlintegrationConstants.SHIP, Objects.nonNull(trackingDateOrTimestamp.getType()) ? trackingDateOrTimestamp.getType().getValue() : StringUtils.EMPTY)) {
          responseResults.put(BlintegrationConstants.SHIP_TIME_STAMP,
              convertTime(trackingDateOrTimestamp.getDateOrTimestamp()));
        }
        if (StringUtils.equalsIgnoreCase(BlintegrationConstants.ESTIMATED_DELIVERY,
            Objects.nonNull(trackingDateOrTimestamp.getType())? trackingDateOrTimestamp.getType().getValue() : StringUtils.EMPTY)) {
          responseResults.put(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP,
              convertTime(trackingDateOrTimestamp.getDateOrTimestamp()));
        }
      }
    }
  }

  /**
   * This method created to get the destination address from reposne
   * @param responseResults responseResults
   * @param trackDetails trackDetails
   */
  private void getDestinationAddressFromResponse(final Map<String, Object> responseResults, final TrackDetail trackDetails) {
    final Address destinationAddress = trackDetails.getDestinationAddress();
    if (Objects.nonNull(destinationAddress)) {
      final Map<String, String> address = new LinkedHashMap<>();
      address.put(BlintegrationConstants.CITY, destinationAddress.getCity());
      address.put(BlintegrationConstants.POSTAL_CODE, destinationAddress.getPostalCode());
      address.put(BlintegrationConstants.COUNTRY_CODE, destinationAddress.getCountryCode());
      responseResults.put(BlintegrationConstants.DESTINATION_ADDRESS, address);
    }
  }

  /**
   * This method created to get the track details from response
   * @param responseResults responseResults
   * @param trackDetails trackDetails
   */
  private void getTrackDetailsFromResponse(final Map<String, Object> responseResults, final TrackDetail trackDetails) {
    if (ArrayUtils.isNotEmpty(trackDetails.getEvents())) {
      final List<Map<String, Object>> list = new ArrayList<>();
      for (int i = 0; i < trackDetails.getEvents().length; i++) {
        final Map<String, Object> map = new LinkedHashMap<>();
        final TrackEvent trackEvent = trackDetails.getEvents()[i];
        if (Objects.nonNull(trackEvent)) {
          map.put(BlintegrationConstants.TIME_STAMP, trackEvent.getTimestamp().getTime());
          map.put(BlintegrationConstants.DESCRIPTION, trackEvent.getEventDescription());
          Address address = trackEvent.getAddress();
          if (Objects.nonNull(address)) {
            final Map<String, String> trackAddress = new LinkedHashMap<>();
            trackAddress.put(BlintegrationConstants.CITY, address.getCity());
            trackAddress.put(BlintegrationConstants.STATE, address.getStateOrProvinceCode());
            map.put(BlintegrationConstants.ADDRESS, trackAddress);
          }
          list.add(map);
        }
      }
      responseResults.put(BlintegrationConstants.TRACK_EVENTS, list);
    }
  }

  /**
   * This method created to convert the time from response
   * @param dateOrTimestamp dateOrTimestamp
   * @return Date
   * @throws ParseException ParseException
   */
  private Date convertTime(final String dateOrTimestamp) throws ParseException {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlintegrationConstants.DATE_FORMAT);
    Calendar calendar = new GregorianCalendar(TimeZone.getTimeZone(BlintegrationConstants.UTC));
    calendar.setTime(formatter.parse(dateOrTimestamp));
    return calendar.getTime();
  }


  /**
   * This common method create to check the null values
   * @param value values
   * @return String
   */
  private String getValuesFromResponse(final String value){
    return StringUtils.isBlank(value) ? StringUtils.EMPTY : value;
  }


}
