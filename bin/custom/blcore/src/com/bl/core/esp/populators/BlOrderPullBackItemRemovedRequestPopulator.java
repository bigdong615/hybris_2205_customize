package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderpullback.OrderPullBackRequest;
import com.bl.esp.dto.orderpullback.data.OrderPullBackItems;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.util.Objects;
import org.springframework.util.Assert;

/**
 * This class created to populate the Order pull back items removed request
 * @author Manikandan
 */
public class BlOrderPullBackItemRemovedRequestPopulator  extends ESPEventCommonPopulator<OrderModel, OrderPullBackRequest> {

  /**
   * This method created to populate the order pull back items removed request
   * @param order order to get the values
   * @param orderPullBackRequest request to be get updated
   * @throws ConversionException exception
   */
  @Override
  public void populate(final OrderModel order, final OrderPullBackRequest orderPullBackRequest)
      throws ConversionException {
    Assert.notNull(order, "Parameter order cannot be null.");
    Assert.notNull(orderPullBackRequest, "Parameter orderPullBackItem Removed Request cannot be null.");
    final UserModel userModel = order.getUser();
    if(Objects.nonNull(userModel)) {
      orderPullBackRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderPullBackRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_PULL_BACK_REMOVED_ITEMS_EVENT_DEFINITION_KEY)));
    final OrderPullBackItems data = new OrderPullBackItems();
    data.setOrderid(order.getCode());
    data.setSubscriberid(getRequestValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.BORROW_LENSES_SUBSCRIBER_ID)));
    populateOrderDataForOrderPullBackItems(order , data , BlCoreConstants.ORDER_PULL_BACK_REMOVED_ITEMS_EVENT_TEMPLATE);
    orderPullBackRequest.setData(data);
  }
}
