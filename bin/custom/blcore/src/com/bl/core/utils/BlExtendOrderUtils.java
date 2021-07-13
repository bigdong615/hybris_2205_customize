package com.bl.core.utils;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.session.SessionService;

public class BlExtendOrderUtils {

  private static SessionService sessionService;

  public BlExtendOrderUtils() {
    //empty to avoid instantiating utils class
  }

  public static OrderModel getCurrentExtendOrderToSession() {
   return getSessionService().getAttribute(BlCoreConstants.EXTEND_ORDER);
  }

  public static void setCurrentExtendOrderToSession(OrderModel extendOrder) {
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
