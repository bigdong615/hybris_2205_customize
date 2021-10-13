package com.bl.integration.cronjob;

import com.bl.core.model.UPSScrapeCronJobModel;
import com.bl.integration.dao.DefaultBlOrderDao;
import com.bl.integration.services.impl.DefaultBlTrackWebServiceImpl;
import com.bl.integration.upsscrape.impl.BlUpdateSerialService;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
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
import org.apache.commons.collections.MapUtils;
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

  /**
   * This method perform cronjob
   */
  @Override
  public PerformResult perform(UPSScrapeCronJobModel upsScrapeCronJobModel) {
    BlLogger.logMessage(LOG , Level.INFO , "Executing BlUPSScrapeJob perform method");
    try {
      final List<AbstractOrderModel> orderModelList = getDefaultBlOrderDao().getOrdersForUPSScrape();
      BlLogger.logMessage(LOG , Level.INFO , String.valueOf(orderModelList.size()));

      orderModelList.forEach(abstractOrderModel -> abstractOrderModel.getConsignments().forEach(consignmentModel ->
          consignmentModel.getPackaginginfos().forEach(packagingInfoModel -> {
            try {
              final Map<String, Object> stringObjectMap = getDefaultBlTrackWebService()
                  .trackService(abstractOrderModel, packagingInfoModel);
              if (MapUtils.isNotEmpty(stringObjectMap)) {
                if (Objects.nonNull(stringObjectMap.get("EstimatedDeliveryTimestamp"))) {
                  final Date estimatedDeliveryTimestamp = (Date) stringObjectMap
                      .get("EstimatedDeliveryTimestamp");
                  if (estimatedDeliveryTimestamp.before(new Date())) {
                    updatePackageDetails(abstractOrderModel,
                        String.valueOf(stringObjectMap.get("TrackingNumber")), packagingInfoModel);
                  }
                } else if (StringUtils.equalsIgnoreCase("Delivered",
                    String.valueOf(stringObjectMap.get("StatusDescription")))) {
                  updatePackageDetails(abstractOrderModel,
                      String.valueOf(stringObjectMap.get("TrackingNumber")), packagingInfoModel);
                } else if (Objects.nonNull(stringObjectMap.get("TrackEvents"))) {
                  final List<Map<String, Object>> list = (List<Map<String, Object>>) stringObjectMap
                      .get("TrackEvents");
                  list.forEach(objectMap -> {
                    if (StringUtils.equalsIgnoreCase("Delivered",
                        (CharSequence) objectMap.get("Description"))) {
                      updatePackageDetails(abstractOrderModel,
                          String.valueOf(stringObjectMap.get("TrackingNumber")),
                          packagingInfoModel);
                    }
                  });
                } else {
                  getBlUpdateSerialService()
                      .updateSerialProducts(String.valueOf(stringObjectMap.get("TrackingNumber")),
                          abstractOrderModel.getCode(), new Date(),
                          packagingInfoModel.getNumberOfRepetitions(),
                          packagingInfoModel);
                }
              }
            }
            catch (Exception e){
              BlLogger.logFormattedMessage(LOG , Level.ERROR , "Error while fetching package{} from Order {}" , e.getMessage() , packagingInfoModel.getPk() , abstractOrderModel.getCode());
            }
      })));

    }
    catch (final Exception e){
      BlLogger.logMessage(LOG , Level.ERROR , "Error while executing BlUPSScrapeJob"  , e.getMessage());
      return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);
    }
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  private void updatePackageDetails(final AbstractOrderModel orderModel, final String trackingNumber , final
      PackagingInfoModel packagingInfoModel) {
    packagingInfoModel.setNumberOfRepetitions(0);
    packagingInfoModel.setIsDelivered(Boolean.TRUE);
    getModelService().save(packagingInfoModel);
    getModelService().refresh(packagingInfoModel);
    getModelService().save(orderModel);
    getModelService().refresh(orderModel);
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

}
