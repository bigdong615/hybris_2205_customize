package com.bl.core.utils;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.ui.Model;

public class BlReplaceMentOrderUtils {

  private static SessionService sessionService;
  private static ModelService modelService;
  private static UserService userService;


  private BlReplaceMentOrderUtils() {
    //empty to avoid instantiating utils class
  }

  public static void updateCartForReplacementOrder(final AbstractOrderModel abstractOrderModel , final Model model) {
    if (BooleanUtils.isTrue(isReplaceMentOrder())) {
      for (OrderModel orderModel : abstractOrderModel.getUser().getOrders()) {
        if (CollectionUtils.isNotEmpty(orderModel.getReturnRequests())) {
          getSessionService().setAttribute(BlCoreConstants.RETURN_REQUEST, orderModel.getReturnRequests().iterator().next());
          for (ReturnRequestModel returnRequestModel : orderModel.getReturnRequests()) {
            setCartPrice(abstractOrderModel , returnRequestModel);
          }
        }
      }
    }
  }

public static void setCartPrice(final AbstractOrderModel abstractOrderModel , final ReturnRequestModel returnRequestModel){
  if(null != getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST)){
    ReturnRequestModel returnRequest = getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST);
    if(returnRequest.getOrder().getCode().equalsIgnoreCase(returnRequestModel.getOrder().getCode())){
      abstractOrderModel.setTotalPrice(0.0);
      abstractOrderModel.setDeliveryCost(0.0);
      abstractOrderModel.setTotalTax(0.0);
      abstractOrderModel.setSubtotal(0.0);
      abstractOrderModel.setTotalDamageWaiverCost(0.0);
      abstractOrderModel.setReplacementOrder(returnRequestModel); // Name to be changes replacementRequest
      final List<AbstractOrderEntryModel> abstractOrderEntryModels = new ArrayList<>();
      setPriceForOrderEntries(abstractOrderModel , abstractOrderEntryModels);
      abstractOrderModel.setEntries(abstractOrderEntryModels);
      getModelService().save(abstractOrderModel);
      getModelService().refresh(abstractOrderModel);
    }
  }
}

  public static void setPriceForOrderEntries(final AbstractOrderModel abstractOrderModel  ,
      final List<AbstractOrderEntryModel> abstractOrderEntryModels) {
    for(AbstractOrderEntryModel abstractOrderEntryModel : abstractOrderModel.getEntries()){
      abstractOrderEntryModel.setTotalPrice(0.0);
      abstractOrderEntryModel.setTaxValues(Collections.emptyList());
      abstractOrderEntryModel.setGearGuardWaiverPrice(0.0);
      abstractOrderEntryModel.setGearGuardProFullWaiverPrice(0.0);
      abstractOrderEntryModels.add(abstractOrderEntryModel);
      getModelService().save(abstractOrderEntryModel);
      getModelService().refresh(abstractOrderEntryModel);
    }
  }

  public static boolean isReplaceMentOrder() {
      return null != getSessionService().getAttribute(BlCoreConstants.ACTING_USER_UID) &&
        null != getSessionService().getAttribute(BlCoreConstants.ASM_SESSION_PARAMETER);
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
