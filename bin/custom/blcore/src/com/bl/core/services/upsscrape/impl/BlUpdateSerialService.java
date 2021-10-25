package com.bl.core.services.upsscrape.impl;

import com.bl.core.enums.NotesEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.services.upsscrape.UpdateSerialService;
import com.bl.core.utils.BlUpdateStagedProductUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Calendar;
import java.util.Date;
import java.util.Objects;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

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

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateSerialProducts(final String packageCode, final String orderCode,
      final Date upsDeliveryDate, final int numberOfRepetition, final PackagingInfoModel packagingInfoModel) {
    BlLogger.logFormattedMessage(LOG , Level.INFO , "Started Performing Update serial products for order {} -> package {} -> number of repetitions  {}"
        , orderCode , packageCode , numberOfRepetition);
    final AbstractOrderModel orderModel = getOrderDao().getOrderByCode(orderCode);
    if (Objects.nonNull(orderModel)) {
      orderModel.setStatus(OrderStatus.LATE);
      final NotesModel notesModel = getModelService().create(NotesModel.class);
      notesModel.setType(NotesEnum.LATE_NOTES);
      notesModel.setNote("Order is not returned on expected time stamp .. So marking order as late");
      notesModel.setUserID(orderModel.getUser().getUid());
      getModelService().save(notesModel);
      orderModel.setOrderNotes(Lists.newArrayList(notesModel));
      saveAndRefreshOrderModel(orderModel);
      performSerialUpdate(orderModel, packagingInfoModel, numberOfRepetition, upsDeliveryDate);
      BlLogger.logFormattedMessage(LOG , Level.INFO , "Finished Performing Update serial products for order{} -> package {} -> number of repetitions  {} "
          , orderCode , packageCode , numberOfRepetition);
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
      final Date upsDeliveryDate){
      orderModel.getConsignments().forEach(consignmentModel -> consignmentModel.getPackaginginfos().forEach(infoModel ->{
        if(packagingInfoModel.getPk().equals(infoModel.getPk())) {
          infoModel.getSerialProducts().forEach(blProductModel ->
              updateSerialStatusBasedOnResponse(blProductModel , numberOfRepetition , packagingInfoModel , upsDeliveryDate));
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
      final PackagingInfoModel packagingInfoModel, final Date upsDeliveryDate){
      updateSerialProduct(blProductModel , numberOfRepetition , packagingInfoModel , upsDeliveryDate);
  }

  /**
   * This method created to update serial status based on number Of Repetition
   * @param blProductModel blProductModel to update after UPS Scrape
   * @param numberOfRepetition total number of repetition of packages
   * @param packagingInfoModel to update the serial products which belongs to package
   * @param upsDeliveryDate ups delivery date from response
   */
  private void updateSerialProduct(final BlProductModel blProductModel, final int numberOfRepetition,
      final PackagingInfoModel packagingInfoModel, final Date upsDeliveryDate) {
    if (blProductModel instanceof BlSerialProductModel) {
      final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) blProductModel;
      if (Objects.isNull(numberOfRepetition) || numberOfRepetition < 3) {
        updateSerialStatus(blSerialProductModel, packagingInfoModel, numberOfRepetition,
            upsDeliveryDate);
      } else if (numberOfRepetition == 3) {
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
      final PackagingInfoModel packagingInfoModel, final int numberOfRepetition, final Date upsDeliveryDate)
  {
    blSerialProductModel.setSerialStatus(SerialStatusEnum.LATE);
    BlUpdateStagedProductUtils
        .changeSerialStatusInStagedVersion(blSerialProductModel.getCode(),SerialStatusEnum.LATE);
    packagingInfoModel.setNumberOfRepetitions(Objects.isNull(numberOfRepetition) ? 0 : numberOfRepetition + 1);
    packagingInfoModel.setPackageReturnedToWarehouse(Boolean.FALSE);
    packagingInfoModel.setIsScrapeScanCompleted(Boolean.TRUE);
    Calendar latePackageDate = Calendar.getInstance();
    latePackageDate.setTime(upsDeliveryDate);
    latePackageDate.add(Calendar.DAY_OF_MONTH ,2);
    packagingInfoModel.setLatePackageDate(latePackageDate.getTime());

    if(packagingInfoModel.getNumberOfRepetitions() == 3){
      updateStolenSerialStatus(blSerialProductModel, packagingInfoModel);
    }
    getModelService().save(packagingInfoModel);
    getModelService().refresh(packagingInfoModel);
  }


  /**
   * This method created to update serial status as STOLEN based on number Of Repetition
   * @param blSerialProductModel blSerialProductModel to update after UPS Scrape
   * @param packagingInfoModel to update the serial products which belongs to package
   */
  private void updateStolenSerialStatus(final BlSerialProductModel blSerialProductModel, final PackagingInfoModel packagingInfoModel){
    blSerialProductModel.setSerialStatus(SerialStatusEnum.STOLEN);
    BlUpdateStagedProductUtils.changeSerialStatusInStagedVersion(blSerialProductModel.getCode(), SerialStatusEnum.STOLEN);
    final AbstractOrderModel abstractOrderModel = packagingInfoModel.getConsignment().getOrder();
    abstractOrderModel.setStatus(OrderStatus.RECEIVED_IN_VERIFICATION);
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
}
