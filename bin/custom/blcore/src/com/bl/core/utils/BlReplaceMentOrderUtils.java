package com.bl.core.utils;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;

/**
 * @author Manikandan
 * This class is created for replacement order
 */
public class BlReplaceMentOrderUtils {

  private static SessionService sessionService;
  private static ModelService modelService;
  private static UserService userService;


  private BlReplaceMentOrderUtils() {
    //empty to avoid instantiating utils class
  }

  /**
   * This method created to update the cart for replacement order
   */
  public static void updateCartForReplacementOrder(final AbstractOrderModel abstractOrderModel) {
    if (BooleanUtils.isTrue(isReplaceMentOrder())) {
      for (final OrderModel orderModel : abstractOrderModel.getUser().getOrders()) {
        if (CollectionUtils.isNotEmpty(orderModel.getReturnRequests())) {
          // needs to remove this code , after merging  , complete flow of return request .
          getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST);


          for (final ReturnRequestModel returnRequestModel : orderModel.getReturnRequests()) {
            setCartPrice(abstractOrderModel , returnRequestModel);
          }
        }
      }
    }
  }

  /**
   * This method created to set cart price as zero for replacement order.
   */
  public static void setCartPrice(final AbstractOrderModel abstractOrderModel , final ReturnRequestModel returnRequestModel){
  if(null != getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST)){
    final ReturnRequestModel returnRequest = getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST);
    if(returnRequest.getOrder().getCode().equalsIgnoreCase(returnRequestModel.getOrder().getCode())){
      abstractOrderModel.setTotalPrice(0.0);
      abstractOrderModel.setDeliveryCost(0.0);
      abstractOrderModel.setTotalTax(0.0);
      abstractOrderModel.setSubtotal(0.0);
      abstractOrderModel.setTotalDamageWaiverCost(0.0);
      abstractOrderModel.setReturnRequestForOrder(returnRequestModel);

      final List<AbstractOrderEntryModel> abstractOrderEntryModels = new ArrayList<>();
      setPriceForOrderEntries(abstractOrderModel , abstractOrderEntryModels);
      abstractOrderModel.setEntries(abstractOrderEntryModels);
      abstractOrderModel.setCalculated(Boolean.TRUE);
      getModelService().save(abstractOrderModel);
      getModelService().refresh(abstractOrderModel);
    }
  }
}

  /**
   * This method created set the cart entry price as zero for replacement order.
   */
  public static void setPriceForOrderEntries(final AbstractOrderModel abstractOrderModel  ,
      final List<AbstractOrderEntryModel> abstractOrderEntryModels) {
    for(final AbstractOrderEntryModel abstractOrderEntryModel : abstractOrderModel.getEntries()){
      abstractOrderEntryModel.setTotalPrice(0.0);
      abstractOrderEntryModel.setTaxValues(Collections.emptyList());
      abstractOrderEntryModel.setGearGuardWaiverPrice(0.0);
      abstractOrderEntryModel.setGearGuardProFullWaiverPrice(0.0);
      abstractOrderEntryModel.setCalculated(Boolean.TRUE);
      abstractOrderEntryModels.add(abstractOrderEntryModel);
      getModelService().save(abstractOrderEntryModel);
      getModelService().refresh(abstractOrderEntryModel);
    }
  }

  /**
   * This method created to check , whether is ASM user is active or not .
   */
  public static boolean isReplaceMentOrder() {
      return null != getSessionService().getAttribute(BlCoreConstants.ACTING_USER_UID) &&
        null != getSessionService().getAttribute(BlCoreConstants.ASM_SESSION_PARAMETER);
  }


  /**
   * This method created to set the payment info same as original order.
   */
  public static void setIsCartUsedForReplacementOrder(final AbstractOrderModel abstractOrderModel) {
    if (BooleanUtils.isTrue(isReplaceMentOrder())) {
      if (null != getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST)) {
        final ReturnRequestModel returnRequestModel = getSessionService()
            .getAttribute(BlCoreConstants.RETURN_REQUEST);
        abstractOrderModel
            .setReturnRequestForOrder(returnRequestModel);
        PaymentInfoModel paymentInfoModel = getModelService().clone(returnRequestModel.getOrder().getPaymentInfo());
        abstractOrderModel.setPaymentInfo(paymentInfoModel);
      }

      abstractOrderModel.setIsReplacementOrder(Boolean.TRUE);
      if (BooleanUtils.isFalse(abstractOrderModel.getCalculated())) {
        abstractOrderModel.setCalculated(Boolean.TRUE);
      }
      getModelService().save(abstractOrderModel);
      getModelService().refresh(abstractOrderModel);

    }
  }

  /**
   * This method created to check , whether is cart used for order replacement .
   */
  public static boolean isCartForReplacement(final AbstractOrderModel abstractOrderModel){
    return Objects.nonNull(abstractOrderModel.getReturnRequestForOrder()) &&
        BooleanUtils.isTrue(abstractOrderModel.getIsReplacementOrder());
  }

  public static SessionService getSessionService() {
    return null == sessionService ? (SessionService) Registry.getApplicationContext()
        .getBean("sessionService") : sessionService;
  }

  public static void setSessionService(
      SessionService sessionService) {
    BlReplaceMentOrderUtils.sessionService = sessionService;
  }


  public static ModelService getModelService() {
    return null == modelService ? (ModelService) Registry.getApplicationContext()
        .getBean("modelService") : modelService;
  }

  public static void setModelService(
      ModelService modelService) {
    BlReplaceMentOrderUtils.modelService = modelService;
  }

  public static UserService getUserService() {
    return null == userService ? (UserService) Registry.getApplicationContext()
        .getBean("userService") : userService;
  }

  public static void setUserService(UserService userService) {
    BlReplaceMentOrderUtils.userService = userService;
  }


}
