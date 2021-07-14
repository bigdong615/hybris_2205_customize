package com.bl.core.utils;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.session.SessionService;
import org.apache.commons.lang.StringUtils;

public class BlExtendOrderUtils {

  private static SessionService sessionService;

  public BlExtendOrderUtils() {
    //empty to avoid instantiating utils class
  }

  public static OrderModel getExtendOrder(final String orderCode , final AbstractOrderModel orderModel) {

    if(StringUtils.isBlank(orderCode) || !StringUtils.containsIgnoreCase(orderCode , orderModel.getCode())) {
      getSessionService().setAttribute(BlCoreConstants.EXTEND_ORDER , orderModel);
    }
    return getSessionService().getAttribute(BlCoreConstants.EXTEND_ORDER);
  }

  public static OrderModel getCurrentExtendOrderToSession() {
   return getSessionService().getAttribute(BlCoreConstants.EXTEND_ORDER);
  }

  public static void setCurrentExtendOrderToSession(final AbstractOrderModel extendOrder) {
    getSessionService().setAttribute(BlCoreConstants.EXTEND_ORDER , extendOrder);
  }

  public static void removeCurrentExtendOrderToSession() {
    getSessionService().removeAttribute(BlCoreConstants.EXTEND_ORDER);
  }

  public static SessionService getSessionService() {
    return null == sessionService ? (SessionService) Registry.getApplicationContext()
        .getBean("sessionService") : sessionService;
  }

  public static void setSessionService(
      SessionService sessionService) {
    BlExtendOrderUtils.sessionService = sessionService;
  }
}
