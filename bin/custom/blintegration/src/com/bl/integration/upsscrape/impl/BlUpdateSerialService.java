package com.bl.integration.upsscrape.impl;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.integration.Utils.BlUpdateStagedProductUtils;
import com.bl.integration.dao.impl.DefaultBlOrderDao;
import com.bl.integration.upsscrape.UpdateSerialService;
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

/**
 * This class created to update the
 * @author Manikandan
 */
public class BlUpdateSerialService implements UpdateSerialService {

  private BaseStoreService baseStoreService;
  private UserService userService;
  private CustomerAccountService customerAccountService;
  private ModelService modelService;
  private DefaultBlOrderDao defaultBlOrderDao;

  /**
   * This method created to update the serial product status
   * @param packageCode packageCode
   * @param orderCode orderCode
   * @param upsDeliveryDate upsDeliveryDate
   * @param numberOfRepetition numberOfRepetition
   * @param packagingInfoModel packagingInfoModel
   */
  @Override
  public void updateSerialProducts(final String packageCode, final String orderCode,
      final Date upsDeliveryDate, final int numberOfRepetition, final PackagingInfoModel packagingInfoModel) {
    final AbstractOrderModel orderModel = getDefaultBlOrderDao().getOrderByCode(orderCode);
    if (Objects.nonNull(orderModel)) {
      orderModel.setStatus(OrderStatus.LATE);
      saveAndRefershOrderModel(orderModel);
      performSerialUpdate(orderModel, packagingInfoModel, numberOfRepetition, upsDeliveryDate);
    }
  }


  /**
   * This method created to update the serial product status
   * @param upsDeliveryDate upsDeliveryDate
   * @param numberOfRepetition numberOfRepetition
   * @param packagingInfoModel packagingInfoModel
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
   * @param blProductModel blProductModel
   * @param numberOfRepetition numberOfRepetition
   * @param packagingInfoModel packagingInfoModel
   * @param upsDeliveryDate upsDeliveryDate
   */
  private void updateSerialStatusBasedOnResponse(final BlProductModel blProductModel, final int numberOfRepetition,
      final PackagingInfoModel packagingInfoModel, final Date upsDeliveryDate){
    if (blProductModel instanceof BlSerialProductModel) {
      updateSerialProduct(blProductModel , numberOfRepetition , packagingInfoModel , upsDeliveryDate);
      final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) blProductModel;
      if (Objects.isNull(numberOfRepetition) || numberOfRepetition < 3) {
        updateSerialStatus(blSerialProductModel, packagingInfoModel, numberOfRepetition, upsDeliveryDate ); }
      else if (numberOfRepetition == 3) {
        updateStolenSerialStatus(blSerialProductModel, packagingInfoModel); }
      saveAndRefershSerialModel(blSerialProductModel);
    }
  }

  /**
   * This method created to update the serial status
   * @param blProductModel blProductModel
   * @param numberOfRepetition numberOfRepetition
   * @param packagingInfoModel packagingInfoModel
   * @param upsDeliveryDate upsDeliveryDate
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
      saveAndRefershSerialModel(blSerialProductModel);

    }
  }

  /**
   * This method created to update serial status
   * @param blSerialProductModel blSerialProductModel
   * @param packagingInfoModel packagingInfoModel
   * @param numberOfRepetition numberOfRepetition
   * @param upsDeliveryDate upsDeliveryDate
   */
  private void updateSerialStatus(final BlSerialProductModel blSerialProductModel,
      final PackagingInfoModel packagingInfoModel, final int numberOfRepetition, final Date upsDeliveryDate)
  {
    blSerialProductModel.setSerialStatus(SerialStatusEnum.LATE);
    /* BlUpdateStagedProductUtils.changeSerialStatusInStagedVersion(blSerialProductModel.getCode(),SerialStatusEnum.LATE);*/ // NOSONAR
    packagingInfoModel.setNumberOfRepetitions(Objects.isNull(numberOfRepetition) ? 0 : numberOfRepetition + 1);
    packagingInfoModel.setPackageReturnedToWarehouse(Boolean.FALSE);
    packagingInfoModel.setIsScrapeScanCompleted(Boolean.TRUE);
    Calendar latePackageDate = Calendar.getInstance();
    latePackageDate.setTime(upsDeliveryDate);
    latePackageDate.add(Calendar.DAY_OF_MONTH ,2);
    packagingInfoModel.setLatePackageDate(latePackageDate.getTime());
    getModelService().save(packagingInfoModel);
    getModelService().refresh(packagingInfoModel);
  }

  /**
   * This method created to update serial status as stolen
   * @param blSerialProductModel blSerialProductModel
   * @param packagingInfoModel packagingInfoModel
   */
  private void updateStolenSerialStatus(final BlSerialProductModel blSerialProductModel, final PackagingInfoModel packagingInfoModel){
    blSerialProductModel.setSerialStatus(SerialStatusEnum.STOLEN);
    BlUpdateStagedProductUtils
        .changeSerialStatusInStagedVersion(blSerialProductModel.getCode(),
            SerialStatusEnum.STOLEN);
    packagingInfoModel.setPackageReturnedToWarehouse(Boolean.FALSE);
    getModelService().save(packagingInfoModel);
    getModelService().refresh(packagingInfoModel);
  }


  /**
   * This common method created to save and refresh blserial products
   * @param blSerialProductModel blSerialProductModel
   */
  private void saveAndRefershSerialModel(final BlSerialProductModel blSerialProductModel) {
    getModelService().save(blSerialProductModel);
    getModelService().refresh(blSerialProductModel);

  }

  /**
   * This common method created to save and refresh abstractOrderModel
   * @param abstractOrderModel AbstractOrderModel
   */
  private void saveAndRefershOrderModel(final AbstractOrderModel abstractOrderModel) {
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


  public DefaultBlOrderDao getDefaultBlOrderDao() {
    return defaultBlOrderDao;
  }

  public void setDefaultBlOrderDao(DefaultBlOrderDao defaultBlOrderDao) {
    this.defaultBlOrderDao = defaultBlOrderDao;
  }


}
