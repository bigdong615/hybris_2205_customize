package com.bl.core.order.hook.impl;

import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import de.hybris.platform.commerceservices.order.hook.CommercePlaceOrderMethodHook;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import org.apache.commons.lang.BooleanUtils;

/**
 * It is a custom implementation of OOTB class {@link CommercePlaceOrderMethodHook} to do change new gear order status
 * when order has been placed.
 *
 * @author Ritika
 *
 */
public class BlNewGearOrderStatusChangeHook implements CommercePlaceOrderMethodHook {

  private ModelService modelService;
  private DefaultBlESPEventService defaultBlESPEventService;

  /**
   * Executed after the place order
   *
   * @param parameter  object containing all the information for checkout
   * @param orderModel
   */
  @Override
  public void afterPlaceOrder(CommerceCheckoutParameter parameter, CommerceOrderResult orderModel)
      throws InvalidCartException {

    final OrderModel order = orderModel.getOrder();
    getModelService().refresh(order);
    ServicesUtil.validateParameterNotNullStandardMessage("order", order);
    if(BooleanUtils.isTrue(order.getIsNewGearOrder())){
      order.setStatus(OrderStatus.SOLD);
      modelService.save(order);
      getModelService().refresh(order);
      // To Call Order confirmation ESP event
      getDefaultBlESPEventService().sendOrderConfirmation(order);
    }
  }

  /**
   * Executed before the place order
   *
   * @param parameter object containing all the information for checkout
   */
  @Override
  public void beforePlaceOrder(CommerceCheckoutParameter parameter) throws InvalidCartException {

  }

  /**
   * Executed before the submit order
   *
   * @param parameter object containing all the information for checkout
   * @param result
   */
  @Override
  public void beforeSubmitOrder(CommerceCheckoutParameter parameter, CommerceOrderResult result)
      throws InvalidCartException {

  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }


  public DefaultBlESPEventService getDefaultBlESPEventService() {
    return defaultBlESPEventService;
  }

  public void setDefaultBlESPEventService(
      DefaultBlESPEventService defaultBlESPEventService) {
    this.defaultBlESPEventService = defaultBlESPEventService;
  }

}
