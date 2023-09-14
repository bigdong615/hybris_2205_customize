package com.bl.core.services.upsscrape.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.NotesEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.services.upsscrape.UpdateSerialService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlUpdateStagedProductUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;

/**
 * This class created to update the serial products
 * @author Manikandan
 */
public class BlUpdateSerialService implements UpdateSerialService {

  private static final Logger LOG = Logger.getLogger(BlUpdateSerialService.class);
  private BaseStoreService baseStoreService;
  private UserService userService;
  private CustomerAccountService customerAccountService;
  private ModelService modelService;
  private BlOrderDao orderDao;
  private BlStockLevelDao blStockLevelDao;
  private ConfigurationService configurationService;
  @Resource(name = "defaultBlUserService")
  private DefaultBlUserService defaultBlUserService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateSerialProducts(final String packageCode, final String orderCode,
      final Date upsDeliveryDate, final int numberOfRepetition, final PackagingInfoModel packagingInfoModel , final Date trackDate) {
    BlLogger.logFormattedMessage(LOG , Level.INFO , "Started Performing Update serial products for order {} -> package {} -> number of repetitions  {}"
        , orderCode , packageCode , numberOfRepetition);
    final AbstractOrderModel orderModel = getOrderDao().getOrderByCode(orderCode);
	 if (Objects.nonNull(orderModel))
	 {
		 orderModel.setStatus(OrderStatus.LATE);
		 if (!orderModel.getOrderNotes().stream().anyMatch(note -> note.getType().equals(NotesEnum.LATE_NOTES)))
		 {
			 final NotesModel notesModel = getModelService().create(NotesModel.class);
			 notesModel.setType(NotesEnum.LATE_NOTES);
			 notesModel.setNote("Order is not returned on expected time stamp .. So marking order as late");
			 notesModel.setUserID(orderModel.getUser().getUid());
			 getModelService().save(notesModel);
			 //BLS-107
			 if (CollectionUtils.isNotEmpty(orderModel.getOrderNotes()))
			 {
				 final List<NotesModel> allOrderNotes = new ArrayList<>(orderModel.getOrderNotes());
				 allOrderNotes.add(notesModel);
				 orderModel.setOrderNotes(allOrderNotes);
			 }
			 else
			 {
				 orderModel.setOrderNotes(Lists.newArrayList(notesModel));
			 }
		 }
		 saveAndRefreshOrderModel(orderModel);
		 performSerialUpdate(orderModel, packagingInfoModel, numberOfRepetition, upsDeliveryDate, trackDate);
		 BlLogger.logFormattedMessage(LOG, Level.INFO,
				 "Finished Performing Update serial products for order{} -> package {} -> number of repetitions  {} ", orderCode,
				 packageCode, numberOfRepetition);
	 }
  }


  /**
   * This method created to update the serial product status based on response
   * @param orderModel order model to update after UPS scrape
   * @param packagingInfoModel to update the serial products which belongs to package
   * @param numberOfRepetition total number of repetition of packages
   * @param upsDeliveryDate ups delivery date from response
   */
  private void performSerialUpdate(final AbstractOrderModel orderModel ,final PackagingInfoModel packagingInfoModel , final int numberOfRepetition ,
      final Date upsDeliveryDate , final Date trackDate){
      orderModel.getConsignments().forEach(consignmentModel -> consignmentModel.getPackaginginfos().forEach(infoModel ->{
        if(packagingInfoModel.getPk().equals(infoModel.getPk())) {
          infoModel.getSerialProducts().forEach(blProductModel ->
              updateSerialStatusBasedOnResponse(blProductModel , numberOfRepetition , packagingInfoModel , upsDeliveryDate , trackDate));
        }
      }));
  }

  /**
   * This method created to update serial status based on response
   * @param blProductModel blProductModel to update after UPS Scrape
   * @param numberOfRepetition total number of repetition of packages
   * @param packagingInfoModel to update the serial products which belongs to package
   * @param upsDeliveryDate ups delivery date from response
   */
  private void updateSerialStatusBasedOnResponse(final BlProductModel blProductModel, final int numberOfRepetition,
      final PackagingInfoModel packagingInfoModel, final Date upsDeliveryDate , final Date trackDate){
      updateSerialProduct(blProductModel , numberOfRepetition , packagingInfoModel , upsDeliveryDate , trackDate);
  }

  /**
   * This method created to update serial status based on number Of Repetition
   * @param blProductModel blProductModel to update after UPS Scrape
   * @param numberOfRepetition total number of repetition of packages
   * @param packagingInfoModel to update the serial products which belongs to package
   * @param upsDeliveryDate ups delivery date from response
   */
  private void updateSerialProduct(final BlProductModel blProductModel, final int numberOfRepetition,
      final PackagingInfoModel packagingInfoModel, final Date upsDeliveryDate , final Date trackDate) {
    if (blProductModel instanceof BlSerialProductModel) {
      final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) blProductModel;
      if (Objects.isNull(numberOfRepetition) || numberOfRepetition < getRepetitions()) {
        updateSerialStatus(blSerialProductModel, packagingInfoModel, numberOfRepetition,
            upsDeliveryDate , trackDate);
      } else if (numberOfRepetition == getRepetitions()) {
        updateStolenSerialStatus(blSerialProductModel, packagingInfoModel);
      }
      saveAndRefreshSerialModel(blSerialProductModel);

    }
  }

  /**
   * This method created to update serial status as Late based on number Of Repetition
   * @param blSerialProductModel blSerialProductModel to update after UPS Scrape
   * @param numberOfRepetition total number of repetition of packages
   * @param packagingInfoModel to update the serial products which belongs to package
   * @param upsDeliveryDate ups delivery date from response
   */
  private void updateSerialStatus(final BlSerialProductModel blSerialProductModel,
      final PackagingInfoModel packagingInfoModel, final int numberOfRepetition, final Date upsDeliveryDate , final Date trackDate)
  {
    ConsignmentModel consignmentModel = packagingInfoModel.getConsignment();
    updateStockBasedOnUPSScrapeResponse(blSerialProductModel , consignmentModel , packagingInfoModel , trackDate);
    packagingInfoModel.setNumberOfRepetitions(Objects.isNull(numberOfRepetition) ? 0 : numberOfRepetition + 1);
    packagingInfoModel.setPackageReturnedToWarehouse(Boolean.FALSE);
    packagingInfoModel.setIsScrapeScanCompleted(Boolean.TRUE);
    if(Objects.isNull(packagingInfoModel.getReturningDate())) {
      packagingInfoModel.setReturningDate(trackDate);
    }
    Calendar latePackageDate = Calendar.getInstance();
    latePackageDate.setTime(upsDeliveryDate);
    latePackageDate.add(Calendar.DAY_OF_MONTH ,getConfigurationService().getConfiguration().getInt(BlCoreConstants.UPS_SCRAPE_JOB_NO_OF_ADDED_DAYS_KEY));
    packagingInfoModel.setLatePackageDate(latePackageDate.getTime());

    if(packagingInfoModel.getNumberOfRepetitions() == getRepetitions()){
      updateStolenSerialStatus(blSerialProductModel, packagingInfoModel);
    }
    else if(packagingInfoModel.getNumberOfRepetitions() < getRepetitions() && consignmentModel.getOrder().getStatus().equals(OrderStatus.LATE)) {
   	 blSerialProductModel.setSerialStatus(SerialStatusEnum.LATE);
       BlUpdateStagedProductUtils.changeSerialStatusInStagedVersion(blSerialProductModel.getCode(), SerialStatusEnum.LATE);
    }
    getModelService().save(packagingInfoModel);
    getModelService().refresh(packagingInfoModel);
  }

  /**
   * get No of allowed repetitions
   * @return
   */
  private int getRepetitions() {
    return getConfigurationService().getConfiguration().getInt(BlCoreConstants.UPS_SCRAPE_JOB_NO_OF_REPETITIONS_KEY);
  }

  /**
   * This method created to update serial status as STOLEN based on number Of Repetition
   * @param blSerialProductModel blSerialProductModel to update after UPS Scrape
   * @param packagingInfoModel to update the serial products which belongs to package
   */
  private void updateStolenSerialStatus(final BlSerialProductModel blSerialProductModel, final PackagingInfoModel packagingInfoModel){
    blSerialProductModel.setSerialStatus(SerialStatusEnum.LOST_UNDER_INVESTIGATION);//BRLN-2224
    BlUpdateStagedProductUtils.changeSerialStatusInStagedVersion(blSerialProductModel.getCode(), SerialStatusEnum.LOST_UNDER_INVESTIGATION);
    final AbstractOrderModel abstractOrderModel = packagingInfoModel.getConsignment().getOrder();
    abstractOrderModel.setStatus(OrderStatus.INCOMPLETE_MISSING_ITEMS);
    packagingInfoModel.setPackageReturnedToWarehouse(Boolean.FALSE);
    getModelService().save(packagingInfoModel);
    getModelService().refresh(packagingInfoModel);
    getModelService().save(abstractOrderModel);
    getModelService().refresh(abstractOrderModel);
  }


  /**
   * This common method created to save and refresh BLSerial product
   * @param blSerialProductModel blSerialProductModel to be save and refresh
   */
  private void saveAndRefreshSerialModel(final BlSerialProductModel blSerialProductModel) {
    getModelService().save(blSerialProductModel);
    getModelService().refresh(blSerialProductModel);

  }



  public void updateStockBasedOnUPSScrapeResponse(final BlProductModel serialProduct,
                                                  final ConsignmentModel consignmentModel, final PackagingInfoModel packagingInfoModel , final Date trackDate)
  {

    final AbstractOrderModel abstractOrderModel = consignmentModel.getOrder();
    final Date optimizedShippingStartDate = consignmentModel.getOptimizedShippingEndDate();
    Calendar optimizedShippingEndDate = Calendar.getInstance();
    if(Objects.nonNull(packagingInfoModel.getReturningDate())){
      optimizedShippingEndDate.setTime(packagingInfoModel.getReturningDate());
    }
    else if(Objects.nonNull(trackDate)){
      optimizedShippingEndDate.setTime(trackDate);
  }
    else {
      optimizedShippingEndDate.setTime(consignmentModel.getOptimizedShippingEndDate());
      optimizedShippingEndDate.add(Calendar.DAY_OF_MONTH ,2);
    }
    final Collection<StockLevelModel> findSerialStockLevelForDate = getBlStockLevelDao()
            .findSerialStockLevelForDate(serialProduct.getCode(), optimizedShippingStartDate, optimizedShippingEndDate.getTime());
    if (CollectionUtils.isNotEmpty(findSerialStockLevelForDate))
    {
      findSerialStockLevelForDate.forEach(stockLevel -> {
        try {
          BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                  "Reserve stock for serial product {}, for stock date {} while  USP scrub response before change Hard Assign {} , reserve status {}, associated order {} "
                          + ",current date {} current user {}", stockLevel.getSerialProductCode(), stockLevel.getDate(), stockLevel.getHardAssigned(), stockLevel.getReservedStatus(),
                  stockLevel.getOrder(), new Date(), (defaultBlUserService.getCurrentUser() != null ? defaultBlUserService.getCurrentUser().getUid() : "In Automation"));
        } catch (Exception e) {
          BlLogger.logMessage(LOG, Level.ERROR, "Some error occur while reserve stock in UPS scrub response flow", e);
        }
        stockLevel.setHardAssigned(true);
        stockLevel.setReservedStatus(true);
        stockLevel.setOrder(abstractOrderModel.getCode());


        ((BlSerialProductModel) serialProduct).setHardAssigned(true); // NOSONAR
        getModelService().save(stockLevel);
        getModelService().save(serialProduct);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Reserved status set to {} and Hard Assigned set to {} for serial {}",
                stockLevel.getReservedStatus(), stockLevel.getHardAssigned(), serialProduct.getCode());
      });
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock level updated for serial {}", serialProduct.getCode());
    }
  }

  /**
   * This common method created to save and refresh abstractOrderModel
   * @param abstractOrderModel AbstractOrderModel to be save and refresh
   */
  private void saveAndRefreshOrderModel(final AbstractOrderModel abstractOrderModel) {
    getModelService().save(abstractOrderModel);
    getModelService().refresh(abstractOrderModel);

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

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  public void setConfigurationService(ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }
}
