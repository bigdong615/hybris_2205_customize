package com.bl.core.replacementorder.replacementorder;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.commerceservices.order.impl.DefaultCommerceDeliveryAddressStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.UserModel;
import org.apache.commons.lang3.BooleanUtils;

public class DefaultBlCommerceDeliveryAddressStrategy extends
    DefaultCommerceDeliveryAddressStrategy {

  @Override
  public boolean storeDeliveryAddress(final CommerceCheckoutParameter parameter)
  {
    return super.storeDeliveryAddress(parameter);
  }
   /* final CartModel cartModel = parameter.getCart();
    final AddressModel addressModel = parameter.getAddress();
    final boolean flagAsDeliveryAddress = parameter.isIsDeliveryAddress();

    validateParameterNotNull(cartModel, "Cart model cannot be null");
    getModelService().refresh(cartModel);

    final UserModel user = cartModel.getUser();
    getModelService().refresh(user);

    cartModel.setDeliveryAddress(addressModel);

    // Check that the address model belongs to the same user as the cart
    if (isValidDeliveryAddress(cartModel, addressModel))
    {
      getModelService().save(cartModel);

      if (addressModel != null && flagAsDeliveryAddress && !Boolean.TRUE.equals(addressModel.getShippingAddress()))
      {
        // Flag the address as a delivery address
        addressModel.setShippingAddress(Boolean.TRUE);
        getModelService().save(addressModel);
      }

      final CommerceCartParameter calculateCartParameter = new CommerceCartParameter();
      calculateCartParameter.setEnableHooks(true);
      calculateCartParameter.setCart(cartModel);
      if(BooleanUtils.isFalse(parameter.getIsCartForReplacementOrder())) {
        getCommerceCartCalculationStrategy().calculateCart(calculateCartParameter);
      }
      else {
        calculateCartParameter.setIsCartForReplacementOrder(Boolean.TRUE);
      }

      // verify if the current delivery mode is still valid for this address
      getCommerceDeliveryModeValidationStrategy().validateDeliveryMode(parameter);
      getModelService().refresh(cartModel);

      return true;
    }

    return false;
  }*/
}
