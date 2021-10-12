package com.bl.integration.upsscrape.impl;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.integration.Utils.BlUpdateStagedProductUtils;
import com.bl.integration.dao.DefaultBlOrderDao;
import com.bl.integration.upsscrape.UpdateSerialService;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
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

  @Override
  public void updateSerialProducts(final String packageCode , final String orderCode , final Date upsDeliveryDate , final int numberOfRepetition , final
      PackagingInfoModel packagingInfoModel) {

    final AbstractOrderModel orderModel = getDefaultBlOrderDao().getOrderByCode(orderCode);
    if(Objects.nonNull(orderModel)) {
      orderModel.setStatus(OrderStatus.LATE); // It should change to late .
      getModelService().save(orderModel);
      getModelService().refresh(orderModel);


      // To set serial as late or stolen based on UPS response


      orderModel.getConsignments().forEach(consignmentModel -> consignmentModel.getPackaginginfos().forEach(infoModel -> {
        if(packagingInfoModel.getPk().equals(infoModel.getPk())){
                  infoModel.getSerialProducts().forEach(blProductModel -> {
                    if(blProductModel instanceof BlSerialProductModel){
                      final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) blProductModel;
                      if (Objects.isNull(numberOfRepetition) || numberOfRepetition < 3) {
                        blSerialProductModel.setSerialStatus(SerialStatusEnum.LATE);
                        BlUpdateStagedProductUtils
                            .changeSerialStatusInStagedVersion(blSerialProductModel.getCode(),
                                SerialStatusEnum.LATE);
                        packagingInfoModel.setNumberOfRepetitions(Objects.isNull(numberOfRepetition) ? 0 : numberOfRepetition + 1);
                        packagingInfoModel.setIsDelivered(Boolean.FALSE);
                        getModelService().save(packagingInfoModel);
                        getModelService().refresh(packagingInfoModel);
                      } else if (numberOfRepetition == 3) {
                        blSerialProductModel.setSerialStatus(SerialStatusEnum.STOLEN);
                        BlUpdateStagedProductUtils
                            .changeSerialStatusInStagedVersion(blSerialProductModel.getCode(),
                                SerialStatusEnum.STOLEN);
                        packagingInfoModel.setIsDelivered(Boolean.FALSE);
                        getModelService().save(packagingInfoModel);
                        getModelService().refresh(packagingInfoModel);
                      }
                      getModelService().save(blSerialProductModel);
                      getModelService().refresh(blSerialProductModel);
                    }
                  });
        }
      }));

    }
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
