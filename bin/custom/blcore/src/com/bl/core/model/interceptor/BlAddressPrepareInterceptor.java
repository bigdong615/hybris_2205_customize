package com.bl.core.model.interceptor;


import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
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

  private DefaultBlUserService defaultBlUserService;
  private ModelService modelService;

  @Override
  public void onPrepare(final AddressModel addressModel,final InterceptorContext interceptorContext)
      throws InterceptorException {

    triggerNewShippingInfoEvent(addressModel, interceptorContext);
  }

  /**
   * New Shipping event
   *
   * @param addressModel the abstract order model
   * @param interceptorContext the interceptor context
   */
  private void triggerNewShippingInfoEvent(final AddressModel addressModel,
      final InterceptorContext interceptorContext) {
    if (getDefaultBlUserService().isCsUser()  && interceptorContext.isModified(addressModel) &&
        addressModel.getOwner() instanceof OrderModel && BooleanUtils.toBoolean(addressModel.getShippingAddress()) &&
        (StringUtils.isBlank(((OrderModel) addressModel.getOwner()).getVersionID()))) {
      try {
        getBlEspEventService().sendOrderNewShippingEvent((OrderModel) addressModel.getOwner());
      } catch (final Exception e) {
        BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
            "New Shipping Info Event failed.", e);
      }
    }
  }


  public DefaultBlUserService getDefaultBlUserService() {
    return defaultBlUserService;
  }

  public void setDefaultBlUserService(
      DefaultBlUserService defaultBlUserService) {
    this.defaultBlUserService = defaultBlUserService;
  }

  public DefaultBlESPEventService getBlEspEventService() {
    return blEspEventService;
  }

  public void setBlEspEventService(final DefaultBlESPEventService blEspEventService) {
    this.blEspEventService = blEspEventService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }
}
