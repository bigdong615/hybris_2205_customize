package com.bl.core.replacementorder.replacementorder;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.commerceservices.order.impl.DefaultCommerceDeliveryModeValidationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import java.util.Collection;
import org.apache.commons.lang3.BooleanUtils;

public class DefaultBlCommerceDeliveryModeValidationStrategy extends
    DefaultCommerceDeliveryModeValidationStrategy {

  @Override
  public void validateDeliveryMode(final CommerceCheckoutParameter parameter)
  {
    super.validateDeliveryMode(parameter);
    /*final CartModel cartModel = parameter.getCart();
    validateParameterNotNull(cartModel, "Cart model cannot be null");

    final DeliveryModeModel currentDeliveryMode = cartModel.getDeliveryMode();
    if (currentDeliveryMode != null)
    {
      final Collection<DeliveryModeModel> supportedDeliveryModes = getDeliveryService().getSupportedDeliveryModeListForOrder(
          cartModel);
      final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
      commerceCartParameter.setEnableHooks(true);
      commerceCartParameter.setCart(cartModel);

      if(BooleanUtils.isTrue(parameter.getIsCartForReplacementOrder())) {
        getCommerceCartCalculationStrategy().calculateCart(commerceCartParameter);
      }

      if (!supportedDeliveryModes.contains(currentDeliveryMode))
      {
        cartModel.setDeliveryMode(null);
        getModelService().save(cartModel);
        getModelService().refresh(cartModel);


        if(BooleanUtils.isTrue(parameter.getIsCartForReplacementOrder())) {
          // Calculate cart after removing delivery mode
          getCommerceCartCalculationStrategy().calculateCart(commerceCartParameter);
        }
      }
    }*/
  }
}
