package com.bl.integration.upsscrape.impl;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.integration.Utils.BlUpdateStagedProductUtils;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.Date;
import java.util.Objects;

/**
 * This class created to update the
 * @author Manikandan
 */
public class BlUpdateSerialService {

  private BaseStoreService baseStoreService;
  private UserService userService;
  private CustomerAccountService customerAccountService;
  private ModelService modelService;

  public void updateSerialProducts(final String packageCode , final String orderCode , final Date upsDeliveryDate , final int numberOfRepetition ) {

    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    final OrderModel orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode,
        baseStoreModel);

    if(Objects.nonNull(orderModel)) {
      orderModel.setStatus(OrderStatus.LATE); // It should change to late .
      getModelService().save(orderModel);
      getModelService().refresh(orderModel);


      // To set serial as late or stolen based on UPS response

      orderModel.getConsignments().forEach(
          consignmentModel -> consignmentModel.getPackaginginfos().forEach(packagingInfoModel ->
              packagingInfoModel.getSerialProducts().forEach(blProductModel -> {
                if (blProductModel instanceof BlSerialProductModel) {
                  final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) blProductModel;
                  if (numberOfRepetition < 3) {
                    blSerialProductModel.setSerialStatus(SerialStatusEnum.LATE);
                    BlUpdateStagedProductUtils
                        .changeSerialStatusInStagedVersion(blSerialProductModel.getCode(),
                            SerialStatusEnum.LATE);
                  } else if (numberOfRepetition == 3) {
                    blSerialProductModel.setSerialStatus(SerialStatusEnum.STOLEN);
                    BlUpdateStagedProductUtils
                        .changeSerialStatusInStagedVersion(blSerialProductModel.getCode(),
                            SerialStatusEnum.STOLEN);
                  }
                  getModelService().save(blSerialProductModel);
                  getModelService().refresh(blSerialProductModel);
                }
              })));
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

}
