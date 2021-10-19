package com.bl.integration.services.impl;

import com.bl.core.enums.CarrierEnum;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.dao.impl.DefaultBlOrderDao;
import com.bl.integration.services.UPSScrapeService;
import com.bl.integration.upsscrape.impl.BlUpdateSerialService;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created for UPS scrape Service
 * @author Manikandan
 */
public class DefaultUPSScrapeService implements UPSScrapeService {

  private static final Logger LOG = Logger.getLogger(DefaultUPSScrapeService.class);

  private BlUpdateSerialService blUpdateSerialService;
  private DefaultBlTrackWebServiceImpl defaultBlTrackWebService;
  private BaseStoreService baseStoreService;
  private UserService userService;
  private CustomerAccountService customerAccountService;
  private DefaultBlOrderDao defaultBlOrderDao;
  private ModelService service;
  private DefaultBlUPSTrackServiceImpl defaultBlUPSTrackServiceImpl;

  /**
   * This method created to perform UPS scrape service
   */
  @Override
  public void performUPSScrapeForOrders() {
    final AtomicReference<Map<String, Object>> stringObjectMap = new AtomicReference<>();
    final List<AbstractOrderModel> orderModelList = getDefaultBlOrderDao().getOrdersForUPSScrape();
    orderModelList.forEach(abstractOrderModel -> abstractOrderModel.getConsignments().forEach(consignmentModel ->
        consignmentModel.getPackaginginfos().forEach(packagingInfoModel -> {
          final String carrierCode = getCarrierType(packagingInfoModel);
          BlLogger.logMessage(LOG , Level.INFO , "Performing UPS Scrape job for carrier" ,carrierCode);
          try {
            if(Objects.isNull(packagingInfoModel.getNumberOfRepetitions()) || packagingInfoModel.getNumberOfRepetitions() < 3) {
              if (StringUtils.equalsIgnoreCase(CarrierEnum.FEDEX.getCode(), carrierCode)) {
                performUPSService(abstractOrderModel, packagingInfoModel, stringObjectMap);
              } else if(StringUtils.equalsIgnoreCase(CarrierEnum.UPS.getCode(), carrierCode)) {
                performFedexService(abstractOrderModel, packagingInfoModel, stringObjectMap);

              }
            }
            final Map<String, Object> stringObjectMap1 = stringObjectMap.get();
            postResponseAction(abstractOrderModel , packagingInfoModel , stringObjectMap1);
          }
          catch (Exception e){
            BlLogger.logFormattedMessage(LOG , Level.ERROR , "Error while fetching package{} from Order {}" , e.getMessage() , packagingInfoModel.getPk() , abstractOrderModel.getCode());
          }
        })));
  }

  /**
   * This method created to perform late order for USP scrape
   */
  @Override
  public void performUPSScrapeForLateOrder() {
    final AtomicReference<Map<String, Object>> stringObjectMap = new AtomicReference<>();
    final List<PackagingInfoModel> packagingInfoModels = getDefaultBlOrderDao().getRescheduledPackagesForUPSScrape();
    BlLogger.logMessage(LOG , Level.INFO , "Started Performing UPS scrape for Late orders");

    packagingInfoModels.forEach(packagingInfoModel -> {
      final AbstractOrderModel abstractOrderModel = packagingInfoModel.getConsignment().getOrder();
      try {
       final String carrierCode = getCarrierType(packagingInfoModel);
        if(Objects.isNull(packagingInfoModel.getNumberOfRepetitions()) || packagingInfoModel.getNumberOfRepetitions() < 3) {
          if (StringUtils
              .equalsIgnoreCase(CarrierEnum.FEDEX.getCode(), carrierCode)) {
            performUPSService(abstractOrderModel, packagingInfoModel, stringObjectMap);
          } else if(StringUtils
              .equalsIgnoreCase(CarrierEnum.UPS.getCode(), carrierCode)) {
            performFedexService(abstractOrderModel, packagingInfoModel, stringObjectMap);
          }
        }
        final Map<String, Object> stringObjectMap1 = stringObjectMap.get();
        postResponseAction(abstractOrderModel , packagingInfoModel , stringObjectMap1);
      }
      catch (Exception e){
        BlLogger.logFormattedMessage(LOG , Level.ERROR , "Error while fetching package{} from Order {}" , e.getMessage() , packagingInfoModel.getPk() , abstractOrderModel.getCode());
      }
    });
    BlLogger.logMessage(LOG , Level.INFO , "Finished Performing UPS scrape for Late orders");

  }

  /**
   * This method created to perform Fedex service for UPS scrape
   * @param abstractOrderModel abstractOrderModel
   * @param packagingInfoModel packagingInfoModel
   * @param stringObjectMap stringObjectMap
   */
  private void performFedexService(final AbstractOrderModel abstractOrderModel , final PackagingInfoModel packagingInfoModel ,
      final AtomicReference<Map<String, Object>> stringObjectMap) {
    stringObjectMap.set(getDefaultBlTrackWebService()
        .trackService(abstractOrderModel, packagingInfoModel));
  }


  /**
   * This method created to perform UPS service for UPS scrape
   * @param abstractOrderModel abstractOrderModel
   * @param packagingInfoModel packagingInfoModel
   * @param stringObjectMap stringObjectMap
   */
  private void performUPSService(final AbstractOrderModel abstractOrderModel , final PackagingInfoModel packagingInfoModel ,
      final AtomicReference<Map<String, Object>> stringObjectMap) {
    stringObjectMap.set(getDefaultBlUPSTrackServiceImpl()
        .trackUPSService(abstractOrderModel, packagingInfoModel));
  }

  /**
   * This method created to updated package details after UPS scrape service
   * @param orderModel orderModel
   * @param trackingNumber trackingNumber
   * @param packagingInfoModel packagingInfoModel
   */
  private void updatePackageDetails(final AbstractOrderModel orderModel, final String trackingNumber , final
  PackagingInfoModel packagingInfoModel) {
    BlLogger.logMessage(LOG , Level.INFO , trackingNumber);
      if(Objects.isNull(packagingInfoModel.getNumberOfRepetitions())) {
        packagingInfoModel.setNumberOfRepetitions(0);
      }
      packagingInfoModel.setPackageReturnedToWarehouse(Boolean.TRUE);
      packagingInfoModel.setIsScrapeScanCompleted(Boolean.TRUE);
      getService().save(packagingInfoModel);
      getService().refresh(packagingInfoModel);
      getService().save(orderModel);
      getService().refresh(orderModel);
  }

  /**
   * This method created to update response
   * @param abstractOrderModel abstractOrderModel
   * @param packagingInfoModel packagingInfoModel
   * @param stringObjectMap stringObjectMap1
   */
  private void postResponseAction(final AbstractOrderModel abstractOrderModel , final PackagingInfoModel packagingInfoModel ,
      final Map<String, Object>  stringObjectMap){
    if (MapUtils.isNotEmpty(stringObjectMap)) {
      if (Objects.nonNull(stringObjectMap.get(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP))) {
        final Date estimatedDeliveryTimestamp = (Date) stringObjectMap.get(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP);
        if (estimatedDeliveryTimestamp.before(new Date())) {
          updatePackageDetails(abstractOrderModel, String.valueOf(stringObjectMap.get(BlintegrationConstants.TRACKING_NUMBER)), packagingInfoModel);
        }
      } else if (StringUtils.equalsIgnoreCase(BlintegrationConstants.DELIVERED,
          String.valueOf(stringObjectMap.get(BlintegrationConstants.STATUS_DESCRIPTION)))) {
        updatePackageDetails(abstractOrderModel, String.valueOf(stringObjectMap.get(BlintegrationConstants.TRACKING_NUMBER)), packagingInfoModel);
      } else if (Objects.nonNull(stringObjectMap.get(BlintegrationConstants.TRACK_EVENTS))) {
        updatePackageDetailsForResponse(stringObjectMap , abstractOrderModel , packagingInfoModel);
      }
      else if(Objects.nonNull(stringObjectMap.get(BlintegrationConstants.STATUS_TYPE)) &&
          (((String) stringObjectMap.get(BlintegrationConstants.STATUS_TYPE)).equalsIgnoreCase(BlintegrationConstants.M)
      || ((String) stringObjectMap.get(BlintegrationConstants.STATUS_TYPE)).equalsIgnoreCase(BlintegrationConstants.MV))){
        BlLogger.logMessage(LOG , Level.INFO , "Package is not shipped yet");
      }
      else {
        getBlUpdateSerialService().updateSerialProducts(String.valueOf(stringObjectMap.get(BlintegrationConstants.TRACKING_NUMBER)),
                abstractOrderModel.getCode(), Objects.nonNull (stringObjectMap.get(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP))
                ? (Date) stringObjectMap.get(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP) : new Date(), packagingInfoModel.getNumberOfRepetitions(),
                packagingInfoModel);
      }
    }
  }

  /**
   * This method created to update package details for reponse
   * @param stringObjectMap stringObjectMap
   * @param abstractOrderModel abstractOrderModel
   * @param packagingInfoModel packagingInfoModel
   */
  private void updatePackageDetailsForResponse(final Map<String, Object> stringObjectMap,
      final AbstractOrderModel abstractOrderModel, final PackagingInfoModel packagingInfoModel) {
    final List<Map<String, Object>> list = (List<Map<String, Object>>) stringObjectMap.get(BlintegrationConstants.TRACK_EVENTS);
    list.forEach(objectMap -> {
      if (StringUtils.equalsIgnoreCase(BlintegrationConstants.DELIVERED,
          (CharSequence) objectMap.get(BlintegrationConstants.DESCRIPTION))) {
        updatePackageDetails(abstractOrderModel,
            String.valueOf(stringObjectMap.get(BlintegrationConstants.TRACKING_NUMBER)),
            packagingInfoModel);
      }
    });
  }

  /**
   * This method created to get the carrier type
   * @param packagingInfoModel packagingInfoModel
   * @return String
   */
  private String getCarrierType(final PackagingInfoModel packagingInfoModel){
    final AtomicReference<String> carrierCode = new AtomicReference<>(StringUtils.EMPTY);
    if(Objects.nonNull(packagingInfoModel.getConsignment())) {
      final ZoneDeliveryModeModel zoneDeliveryModeModel = (ZoneDeliveryModeModel) packagingInfoModel.getConsignment().getDeliveryMode();
      if (Objects.nonNull(zoneDeliveryModeModel) && Objects
          .nonNull(zoneDeliveryModeModel.getCarrier())) {
        carrierCode.set(zoneDeliveryModeModel.getCarrier().getCode());
      }
    }
    return carrierCode.get();
  }

  public BlUpdateSerialService getBlUpdateSerialService() {
    return blUpdateSerialService;
  }

  public void setBlUpdateSerialService(
      BlUpdateSerialService blUpdateSerialService) {
    this.blUpdateSerialService = blUpdateSerialService;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public CustomerAccountService getCustomerAccountService() {
    return customerAccountService;
  }

  public void setCustomerAccountService(
      CustomerAccountService customerAccountService) {
    this.customerAccountService = customerAccountService;
  }


  public DefaultBlTrackWebServiceImpl getDefaultBlTrackWebService() {
    return defaultBlTrackWebService;
  }

  public void setDefaultBlTrackWebService(
      DefaultBlTrackWebServiceImpl defaultBlTrackWebService) {
    this.defaultBlTrackWebService = defaultBlTrackWebService;
  }


  public DefaultBlOrderDao getDefaultBlOrderDao() {
    return defaultBlOrderDao;
  }

  public void setDefaultBlOrderDao(DefaultBlOrderDao defaultBlOrderDao) {
    this.defaultBlOrderDao = defaultBlOrderDao;
  }


  public ModelService getService() {
    return service;
  }

  public void setService(ModelService service) {
    this.service = service;
  }


  public DefaultBlUPSTrackServiceImpl getDefaultBlUPSTrackServiceImpl() {
    return defaultBlUPSTrackServiceImpl;
  }

  public void setDefaultBlUPSTrackServiceImpl(
      DefaultBlUPSTrackServiceImpl defaultBlUPSTrackServiceImpl) {
    this.defaultBlUPSTrackServiceImpl = defaultBlUPSTrackServiceImpl;
  }


}
