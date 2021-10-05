package com.bl.core.model.interceptor;


import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This Interceptor is used to modify Address Model.
 *
 * @author Avani Patel
 */
public class BlAddressPrepareInterceptor implements PrepareInterceptor<AddressModel> {

  private static final Logger LOG = Logger.getLogger(BlAddressPrepareInterceptor.class);
  private DefaultBlESPEventService blEspEventService;
  private UserService userService;

  @Override
  public void onPrepare(final AddressModel addressModel,final InterceptorContext interceptorContext)
      throws InterceptorException {

    triggerNewShippingInfoEven(addressModel, interceptorContext);
  }

  /**
   * New Shipping event
   *
   * @param addressModel the abstract order model
   * @param interceptorContext the interceptor context
   */
  private void triggerNewShippingInfoEven(final AddressModel addressModel,
      final InterceptorContext interceptorContext) {
    if (isCsUser()  && interceptorContext.isModified(addressModel) && addressModel.getOwner() instanceof OrderModel && BooleanUtils.toBoolean(addressModel.getShippingAddress())) {
      try {
        getBlEspEventService().sendOrderNewShippingEvent((OrderModel) addressModel.getOwner());

      } catch (final Exception e) {
        BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
            "New Shipping Info Event failed.", e);
      }
    }

  }

  /**
   * method will called to check is logged in user is CS user or not
   *
   * @return
   */
  private boolean isCsUser() {
    boolean isCsAgent = false;
    final UserModel currentUser = getUserService().getCurrentUser();
    for (final PrincipalGroupModel userGroup : currentUser.getGroups()) {
      if (BlInventoryScanLoggingConstants.CUSTOMER_SUPPORT_AGENT_GROUP.equals(userGroup.getUid())) {
        isCsAgent = true;
        BlLogger
            .logFormatMessageInfo(LOG, Level.DEBUG, "Logged in user {} is cs user", currentUser);
        break;
      }
    }
    return isCsAgent;
  }

  /**
   * @return the userService
   */
  public UserService getUserService() {
    return userService;
  }


  /**
   * @param userService the userService to set
   */
  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public DefaultBlESPEventService getBlEspEventService() {
    return blEspEventService;
  }

  public void setBlEspEventService(final DefaultBlESPEventService blEspEventService) {
    this.blEspEventService = blEspEventService;
  }
}
