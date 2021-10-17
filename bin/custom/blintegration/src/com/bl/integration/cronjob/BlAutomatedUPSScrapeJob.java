package com.bl.integration.cronjob;

import com.bl.BlloggingStandalone;
import com.bl.core.model.UPSScrapeCronJobModel;
import com.bl.integration.dao.DefaultBlOrderDao;
import com.bl.integration.services.impl.DefaultBlTrackWebServiceImpl;
import com.bl.integration.services.impl.DefaultBlUPSTrackServiceImpl;
import com.bl.integration.upsscrape.impl.BlUpdateSerialService;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import javolution.io.Struct.Bool;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This cronjob created to execute automatically to call UPS Scrape service
 * @author Manikandan
 */
public class BlAutomatedUPSScrapeJob extends AbstractJobPerformable<UPSScrapeCronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlAutomatedUPSScrapeJob.class);
  private BlUpdateSerialService blUpdateSerialService;
  private DefaultBlTrackWebServiceImpl defaultBlTrackWebService;
  private BaseStoreService baseStoreService;
  private UserService userService;
  private CustomerAccountService customerAccountService;
  private DefaultBlOrderDao defaultBlOrderDao;
  private ModelService modelService;
  private DefaultBlUPSTrackServiceImpl defaultBlUPSTrackServiceImpl;

  /**
   * This method perform cronjob
   */
  @Override
  public PerformResult perform(UPSScrapeCronJobModel upsScrapeCronJobModel) {
    BlLogger.logMessage(LOG , Level.INFO , "Executing BlUPSScrapeJob perform method");
    try {
      final AtomicReference<Map<String, Object>> stringObjectMap = new AtomicReference<>();
      final List<AbstractOrderModel> orderModelList = getDefaultBlOrderDao().getOrdersForUPSScrape();

      BlLogger.logMessage(LOG , Level.INFO , String.valueOf(orderModelList.size()));

        orderModelList.forEach(abstractOrderModel -> abstractOrderModel.getConsignments().forEach(consignmentModel ->
          consignmentModel.getPackaginginfos().forEach(packagingInfoModel -> {
            try {
             // if(BooleanUtils.isFalse(packagingInfoModel.isDelivered())) {
                performFedexService(abstractOrderModel , packagingInfoModel, stringObjectMap);
                performUPSService(abstractOrderModel , packagingInfoModel , stringObjectMap);
              //}
              final Map<String, Object> stringObjectMap1 = stringObjectMap.get();
                postResponseAction(abstractOrderModel , packagingInfoModel , stringObjectMap1);
            }
            catch (Exception e){
              BlLogger.logFormattedMessage(LOG , Level.ERROR , "Error while fetching package{} from Order {}" , e.getMessage() , packagingInfoModel.getPk() , abstractOrderModel.getCode());
            }
      })));

      final List<PackagingInfoModel> packagingInfoModels = getDefaultBlOrderDao().getRescheduledPackagesForUPSScrape();
      BlLogger.logMessage(LOG , Level.ERROR , String.valueOf(packagingInfoModels.size()));

        packagingInfoModels.forEach(packagingInfoModel -> {
          final AbstractOrderModel abstractOrderModel = packagingInfoModel.getConsignment().getOrder();
          try {
            /*if(BooleanUtils.isFalse(packagingInfoModel.isDelivered())) {*/
              performFedexService(abstractOrderModel , packagingInfoModel, stringObjectMap);
              performUPSService(abstractOrderModel , packagingInfoModel , stringObjectMap);
           // }
            final Map<String, Object> stringObjectMap1 = stringObjectMap.get();
            postResponseAction(abstractOrderModel , packagingInfoModel , stringObjectMap1);
          }
          catch (Exception e){
            BlLogger.logFormattedMessage(LOG , Level.ERROR , "Error while fetching package{} from Order {}" , e.getMessage() , packagingInfoModel.getPk() , abstractOrderModel.getCode());
          }
        });

    }
    catch (final Exception e){
      BlLogger.logMessage(LOG , Level.ERROR , "Error while executing BlUPSScrapeJob"  , e.getMessage());
      return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);
    }
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  private void performFedexService(final AbstractOrderModel abstractOrderModel , final PackagingInfoModel packagingInfoModel ,
      final AtomicReference<Map<String, Object>> stringObjectMap) {
    stringObjectMap.set(getDefaultBlTrackWebService()
        .trackService(abstractOrderModel, packagingInfoModel));
  }


  private void performUPSService(final AbstractOrderModel abstractOrderModel , final PackagingInfoModel packagingInfoModel ,
      final AtomicReference<Map<String, Object>> stringObjectMap) {
     stringObjectMap.set(getDefaultBlUPSTrackServiceImpl()
        .trackUPSService(abstractOrderModel, packagingInfoModel));
  }

  private void updatePackageDetails(final AbstractOrderModel orderModel, final String trackingNumber , final
      PackagingInfoModel packagingInfoModel) {
    packagingInfoModel.setNumberOfRepetitions(0);
    packagingInfoModel.setDelivered(Boolean.TRUE);
    packagingInfoModel.setIsScrapeScanCompleted(Boolean.TRUE);
    getModelService().save(packagingInfoModel);
    getModelService().refresh(packagingInfoModel);
    getModelService().save(orderModel);
    getModelService().refresh(orderModel);
  }

  private void postResponseAction(final AbstractOrderModel abstractOrderModel , final PackagingInfoModel packagingInfoModel ,
      final Map<String, Object>  stringObjectMap1){
    if (MapUtils.isNotEmpty(stringObjectMap1)) {

      if (Objects.nonNull(stringObjectMap1.get("EstimatedDeliveryTimestamp"))) {
        final Date estimatedDeliveryTimestamp = (Date) stringObjectMap1
            .get("EstimatedDeliveryTimestamp");
        if (estimatedDeliveryTimestamp.before(new Date())) {
          updatePackageDetails(abstractOrderModel,
              String.valueOf(stringObjectMap1.get("TrackingNumber")),
              packagingInfoModel);
        }
      } else if (StringUtils.equalsIgnoreCase("Delivered",
          String.valueOf(stringObjectMap1.get("StatusDescription")))) {
        updatePackageDetails(abstractOrderModel,
            String.valueOf(stringObjectMap1.get("TrackingNumber")), packagingInfoModel);
      } else if (Objects.nonNull(stringObjectMap1.get("TrackEvents"))) {
        final List<Map<String, Object>> list = (List<Map<String, Object>>) stringObjectMap1
            .get("TrackEvents");
        list.forEach(objectMap -> {
          if (StringUtils.equalsIgnoreCase("Delivered",
              (CharSequence) objectMap.get("Description"))) {
            updatePackageDetails(abstractOrderModel,
                String.valueOf(stringObjectMap1.get("TrackingNumber")),
                packagingInfoModel);
          }
        });
      } else {
        getBlUpdateSerialService()
            .updateSerialProducts(
                String.valueOf(stringObjectMap1.get("TrackingNumber")),
                abstractOrderModel.getCode(), new Date(),
                packagingInfoModel.getNumberOfRepetitions(),
                packagingInfoModel);
      }
    }
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

  public ModelService getModelService() {
    return modelService;
  }

  @Override
  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }


  public DefaultBlUPSTrackServiceImpl getDefaultBlUPSTrackServiceImpl() {
    return defaultBlUPSTrackServiceImpl;
  }

  public void setDefaultBlUPSTrackServiceImpl(
      DefaultBlUPSTrackServiceImpl defaultBlUPSTrackServiceImpl) {
    this.defaultBlUPSTrackServiceImpl = defaultBlUPSTrackServiceImpl;
  }


}
