package com.bl.integration.services.impl;

import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.fedex.tracking.pojo.ClientDetail;
import com.bl.integration.fedex.tracking.pojo.TrackIdentifierType;
import com.bl.integration.fedex.tracking.pojo.TrackPackageIdentifier;
import com.bl.integration.fedex.tracking.pojo.TrackPortType;
import com.bl.integration.fedex.tracking.pojo.TrackReply;
import com.bl.integration.fedex.tracking.pojo.TrackRequest;
import com.bl.integration.fedex.tracking.pojo.TrackRequestProcessingOptionType;
import com.bl.integration.fedex.tracking.pojo.TrackSelectionDetail;
import com.bl.integration.fedex.tracking.pojo.TrackServiceLocator;
import com.bl.integration.fedex.tracking.pojo.TransactionDetail;
import com.bl.integration.fedex.tracking.pojo.VersionId;
import com.bl.integration.fedex.tracking.pojo.WebAuthenticationCredential;
import com.bl.integration.fedex.tracking.pojo.WebAuthenticationDetail;
import com.bl.integration.services.BlTrackWebService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import java.util.Objects;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class DefaultBlTrackWebServiceImpl implements BlTrackWebService {

  private static final Logger LOG = Logger.getLogger(DefaultBlTrackWebServiceImpl.class);

  @Override
  public boolean trackService(final AbstractOrderModel abstractOrderModel) {

    if (Objects.isNull(abstractOrderModel)) {
      try {
        final TrackRequest trackRequest = new TrackRequest();
        trackRequest.setClientDetail(getCliendDetailsForFedex());
        trackRequest.setWebAuthenticationDetail(getWebAuthenticationDetailsForFedex());
        trackRequest.setTransactionDetail(getTransactionDetailForFedex(abstractOrderModel));
        trackRequest.setVersion(getVersionIdForFedex());
        getTrackageIndentifierNumber(trackRequest);
        final TrackServiceLocator service = new TrackServiceLocator();
        final TrackPortType port;
        service.setTrackServicePortEndpointAddress(BlintegrationConstants.FEDEX_API_URL);
        port = service.getTrackServicePort();
        TrackReply reply = port.track(trackRequest);
        BlLogger.logMessage(LOG , Level.INFO , reply.toString());
      } catch (final Exception e) {
        BlLogger.logMessage(LOG, Level.ERROR, "Error While Calling Track service", e);
        return Boolean.FALSE;
      }
    }

    return Boolean.TRUE;
  }

  private ClientDetail getCliendDetailsForFedex() {
    final ClientDetail clientDetail = new ClientDetail();
    clientDetail.setAccountNumber(BlintegrationConstants.FEDEX_ACCOUNT_NUMBER);
    clientDetail.setMeterNumber(BlintegrationConstants.FEDEX_METER_NUMBER);
    return clientDetail;
  }

  private WebAuthenticationDetail getWebAuthenticationDetailsForFedex() {
    final WebAuthenticationCredential userCredential = new WebAuthenticationCredential();
    final WebAuthenticationCredential parentCredential = new WebAuthenticationCredential();
    userCredential.setKey(BlintegrationConstants.FEDEX_USER_API_KEY);
    userCredential.setPassword(BlintegrationConstants.FEDEX_USER_API_PASSWORD);
    parentCredential.setKey(BlintegrationConstants.FEDEX_PARENT_API_KEY);
    parentCredential.setPassword(BlintegrationConstants.FEDEX_PARENT_API_PASSWORD);
    return new WebAuthenticationDetail(parentCredential, userCredential);
  }


  private TransactionDetail getTransactionDetailForFedex(
      final AbstractOrderModel abstractOrderModel) {
    TransactionDetail transactionDetail = new TransactionDetail();
    String transId = "000001"
        + BlintegrationConstants.HYPHEN
        + BlintegrationConstants.IN_BOUND_OR_OUT_BOUND
        + BlintegrationConstants.HYPHEN
        + System.currentTimeMillis();
    transactionDetail.setCustomerTransactionId(transId);
    return transactionDetail;
  }

  private VersionId getVersionIdForFedex() {
    return new VersionId(BlintegrationConstants.TRCK, 19, 0, 0);
  }

  private void getTrackageIndentifierNumber(final TrackRequest trackRequest) {
    TrackPackageIdentifier packageIdentifier = new TrackPackageIdentifier();
    TrackSelectionDetail selectionDetail = new TrackSelectionDetail();
    packageIdentifier.setValue("744802117499787"); // Used for testinf
    packageIdentifier.setType(TrackIdentifierType.TRACKING_NUMBER_OR_DOORTAG);
    selectionDetail.setPackageIdentifier(packageIdentifier);
    trackRequest.setSelectionDetails(new TrackSelectionDetail[]{selectionDetail});
    TrackRequestProcessingOptionType processingOption = TrackRequestProcessingOptionType.INCLUDE_DETAILED_SCANS;
    trackRequest.setProcessingOptions(new TrackRequestProcessingOptionType[]{processingOption});
  }

  private static void updateEndPoint(TrackServiceLocator serviceLocator) {
    serviceLocator.setTrackServicePortEndpointAddress(BlintegrationConstants.FEDEX_API_URL);
  }


}
