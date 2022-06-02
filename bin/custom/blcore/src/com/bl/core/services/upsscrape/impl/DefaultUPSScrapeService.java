package com.bl.core.services.upsscrape.impl;

import com.bl.core.enums.CarrierEnum;
import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.services.upsscrape.UPSScrapeService;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.services.impl.DefaultBlTrackWebServiceImpl;
import com.bl.integration.services.impl.DefaultBlUPSTrackServiceImpl;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.time.DateUtils;
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
  private BlOrderDao orderDao;
  private ModelService service;
  private DefaultBlUPSTrackServiceImpl defaultBlUPSTrackServiceImpl;

  /**
   * {@inheritDoc}
   */
  @Override
  public void performUPSScrapeForOrders() {
    final AtomicReference<Map<String, Object>> stringObjectMap = new AtomicReference<>();
    final List<AbstractOrderModel> orderModelList = getOrderDao().getOrdersForUPSScrape();
    orderModelList.forEach(abstractOrderModel -> abstractOrderModel.getConsignments().forEach(consignmentModel ->
        consignmentModel.getPackaginginfos().forEach(packagingInfoModel -> {
          packagingInfoModel.isIsScrapeScanCompleted();
          if(BooleanUtils.isFalse(packagingInfoModel.isIsScrapeScanCompleted())) {
            final String carrierCode = getCarrierType(packagingInfoModel);
            BlLogger.logMessage(LOG, Level.INFO, "Performing UPS Scrape job for carrier ", carrierCode);
            try {
              performUPSScrapeService(packagingInfoModel, carrierCode, stringObjectMap, abstractOrderModel);
            } catch (final Exception e) {
              BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error while fetching package{} from Order {} ", e.getMessage(), packagingInfoModel.getPk(), abstractOrderModel.getCode());
              BlLogger.logMessage(LOG, Level.ERROR, "Error while performing UPS Scrape job", e);
            }
          }
        })));
  }

  /**
   * This method created to perform the UPS scrape Service for UPS and Fedex service
   * @param packagingInfoModel package to be scan by service
   * @param carrierCode carrier code
   * @param stringObjectMap results to be updated
   * @param abstractOrderModel abstract order model to get the request
   */
  private void performUPSScrapeService(final PackagingInfoModel packagingInfoModel, final String carrierCode,
      final AtomicReference<Map<String, Object>> stringObjectMap, final AbstractOrderModel abstractOrderModel){
    if (isOrderAllowToScan(packagingInfoModel) && (Objects.isNull(packagingInfoModel.getNumberOfRepetitions())
        || packagingInfoModel.getNumberOfRepetitions() < 3)) {
      if (StringUtils.equalsIgnoreCase(CarrierEnum.UPS.getCode(), carrierCode)) {
        performUPSService(abstractOrderModel, packagingInfoModel, stringObjectMap);
      } else if (StringUtils.equalsIgnoreCase(CarrierEnum.FEDEX.getCode(), carrierCode)) {
        performFedexService(abstractOrderModel, packagingInfoModel, stringObjectMap);
      }
    }
    final Map<String, Object> stringObjectMap1 = stringObjectMap.get();
    postResponseAction(abstractOrderModel , packagingInfoModel , stringObjectMap1);
  }


  /**
   *
   * {@inheritDoc}
   */
  @Override
  public void performUPSScrapeForLateOrder() {
    final AtomicReference<Map<String, Object>> stringObjectMap = new AtomicReference<>();
    final List<PackagingInfoModel> packagingInfoModels = getOrderDao().getRescheduledPackagesForUPSScrape();
    BlLogger.logMessage(LOG , Level.INFO , "Started Performing UPS scrape for Late orders");
    packagingInfoModels.forEach(packagingInfoModel -> {
      final AbstractOrderModel abstractOrderModel = packagingInfoModel.getConsignment().getOrder();
      try {
       final String carrierCode = getCarrierType(packagingInfoModel);
        if(Objects.isNull(packagingInfoModel.getNumberOfRepetitions()) || packagingInfoModel.getNumberOfRepetitions() < 3) {
          if (StringUtils
              .equalsIgnoreCase(CarrierEnum.UPS.getCode(), carrierCode)) {
            performUPSService(abstractOrderModel, packagingInfoModel, stringObjectMap);
          } else if(StringUtils
              .equalsIgnoreCase(CarrierEnum.FEDEX.getCode(), carrierCode)) {
            performFedexService(abstractOrderModel, packagingInfoModel, stringObjectMap);
          }
        }
        final Map<String, Object> stringObjectMap1 = stringObjectMap.get();
        postResponseAction(abstractOrderModel , packagingInfoModel , stringObjectMap1);
      }
      catch (final Exception e){
        BlLogger.logFormattedMessage(LOG , Level.ERROR , "Error while fetching package{} from Order {} " , e.getMessage() , packagingInfoModel.getPk() , abstractOrderModel.getCode());
        BlLogger.logMessage(LOG , Level.ERROR , "Error while fetching package from Order" , e);
      }
    });
    BlLogger.logMessage(LOG , Level.INFO , "Finished Performing UPS scrape for Late orders");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void performUPSScrapeForDelayedOrUpdatedOrder() {
    final AtomicReference<Map<String, Object>> stringObjectMap = new AtomicReference<>();
    final List<PackagingInfoModel> packagingInfoModels = getOrderDao().getDelayedOrUpdatedPackagesForUPSScrape();
    BlLogger.logMessage(LOG , Level.INFO , "Started Performing UPS scrape for Delayed or updated orders");
    packagingInfoModels.forEach(packagingInfoModel -> {
      final AbstractOrderModel abstractOrderModel = packagingInfoModel.getConsignment().getOrder();
      try {
        final String carrierCode = getCarrierType(packagingInfoModel);
        if(Objects.isNull(packagingInfoModel.getNumberOfRepetitions()) || packagingInfoModel.getNumberOfRepetitions() < 3) {
          if (StringUtils
              .equalsIgnoreCase(CarrierEnum.UPS.getCode(), carrierCode)) {
            performUPSService(abstractOrderModel, packagingInfoModel, stringObjectMap);
          } else if(StringUtils
              .equalsIgnoreCase(CarrierEnum.FEDEX.getCode(), carrierCode)) {
            performFedexService(abstractOrderModel, packagingInfoModel, stringObjectMap);
          }
        }
        final Map<String, Object> stringObjectMap1 = stringObjectMap.get();
        postResponseAction(abstractOrderModel , packagingInfoModel , stringObjectMap1);
      }
      catch (final Exception e){
        BlLogger.logFormattedMessage(LOG , Level.ERROR , "while fetching package{} from Order {} " , e.getMessage() , packagingInfoModel.getPk() , abstractOrderModel.getCode());
        BlLogger.logMessage(LOG , Level.ERROR , "Error while fetching package from Order" , e);
      }
    });
    BlLogger.logMessage(LOG , Level.INFO , "Finished Performing UPS scrape Delayed or updated orders");

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


        updatePackagesDetailsForEstimatedDelivery(stringObjectMap , abstractOrderModel , packagingInfoModel);
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
       updateSerialStatusBasedIfDeliveryIsLateOrNotFound(stringObjectMap , abstractOrderModel , packagingInfoModel);
      }
    }
  }

  /**
   * This method created to update the serial if UPS scrape not found delivery details or Late
   * @param stringObjectMap response from UPS scrape
   * @param abstractOrderModel order model to be update
   * @param packagingInfoModel package to be update
   */
  private void updateSerialStatusBasedIfDeliveryIsLateOrNotFound(final Map<String, Object> stringObjectMap,
      final AbstractOrderModel abstractOrderModel, final PackagingInfoModel packagingInfoModel) {
    getBlUpdateSerialService().updateSerialProducts(Objects.isNull(stringObjectMap.get(BlintegrationConstants.TRACKING_NUMBER))
            ? StringUtils.EMPTY : String.valueOf(stringObjectMap.get(BlintegrationConstants.TRACKING_NUMBER)),
        abstractOrderModel.getCode(), Objects.isNull(stringObjectMap.get(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP))
            ? new Date(): (Date)stringObjectMap.get(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP),
        Objects.isNull(packagingInfoModel.getNumberOfRepetitions()) ? 0 : packagingInfoModel.getNumberOfRepetitions(),
        packagingInfoModel,(Date)stringObjectMap.get(BlintegrationConstants.ACTIVITY_TIME_STAMP));
  }

  /**
   * This method created to update the pacakage details based on estimated delivery
   * @param stringObjectMap response from UPS scrape
   * @param abstractOrderModel order model to be update after scrape
   * @param packagingInfoModel packaging model to be update
   */
  private void updatePackagesDetailsForEstimatedDelivery(final Map<String, Object> stringObjectMap, final AbstractOrderModel abstractOrderModel,
      final PackagingInfoModel packagingInfoModel) {
    final Date estimatedDeliveryTimestamp = (Date) stringObjectMap.get(BlintegrationConstants.ESTIMATED_DELIVERY_TIME_STAMP);
    packagingInfoModel.setReturningDate(estimatedDeliveryTimestamp);
    if (estimatedDeliveryTimestamp.before(new Date()) || DateUtils.isSameDay(new Date(), estimatedDeliveryTimestamp)) {
      updatePackageDetails(abstractOrderModel, String.valueOf(stringObjectMap.get(BlintegrationConstants.TRACKING_NUMBER)), packagingInfoModel);
    }
  }

  /**
   * This method created to update package details for response
   * @param stringObjectMap stringObjectMap response from UPS scrape
   * @param abstractOrderModel abstractOrderModel order to be update
   * @param packagingInfoModel packagingInfoModel to be update
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
   * @param packagingInfoModel packagingInfoModel to get the carrier type
   * @return String carrier code
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

  /**
   * This method created to check whether the order is extend or not for UPS scarpe
   * @param packagingInfoModel package to be get scan for UPS scrape
   * @return response based on condition
   */
  private boolean isOrderAllowToScan(final PackagingInfoModel packagingInfoModel) {
    final AtomicBoolean isAllowed = new AtomicBoolean(true);
    final AbstractOrderModel abstractOrderModel = packagingInfoModel.getConsignment().getOrder();
    if (null != packagingInfoModel.getConsignment() &&
        BooleanUtils.isFalse(abstractOrderModel.getIsExtendedOrder()) && CollectionUtils
        .isNotEmpty(abstractOrderModel.getExtendedOrderCopyList())) {
      final Date optimizedShippingEndDate = getDateFromExtendOrderCopyList(abstractOrderModel , packagingInfoModel.getConsignment());
      if(Objects.nonNull(optimizedShippingEndDate)) {
        if (DateUtils.isSameDay(optimizedShippingEndDate, new Date())) {
          isAllowed.set(Boolean.TRUE);
        }
        else {
          isAllowed.set(Boolean.FALSE);
        }
      }
    }
    if(CollectionUtils.isEmpty(abstractOrderModel.getExtendedOrderCopyList())){
      isAllowed.set(Boolean.TRUE);
    }
    return isAllowed.get();
  }

  /**
   * This method created to get the extend order list from original order
   * @param abstractOrderModel order model to get the list of extend order
   * @param consignment consignment to get the optimizedShippingEndDate
   * @return optimizedShippingEndDate
   */
  private Date getDateFromExtendOrderCopyList(final AbstractOrderModel abstractOrderModel,
      final ConsignmentModel consignment) {
    final AtomicReference<Date> optimizedShippingEndDate = new AtomicReference<>();
      final List<AbstractOrderModel> orderModelList = abstractOrderModel.getExtendedOrderCopyList();
        final int size = orderModelList.size();
        for (final AbstractOrderModel extendOrder :orderModelList) {
          if (BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) && extendOrder
              .getExtendOrderStatus().getCode()
              .equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())
              && orderModelList.get(size - 1).getPk()
              .equals(extendOrder.getPk())) {
            extendOrder.getConsignments().forEach(consignmentModel -> {
              if(consignmentModel.getCode().equalsIgnoreCase(consignment.getCode())){
                optimizedShippingEndDate.set(consignmentModel.getOptimizedShippingEndDate());
              }
            });
          }
      }
    return optimizedShippingEndDate.get();
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


  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(BlOrderDao orderDao) {
    this.orderDao = orderDao;
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
