package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderpullback.OrderPullBackRequest;
import com.bl.esp.dto.orderpullback.data.OrderPullBackItems;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.util.Collections;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.hsqldb.lib.Collection;
import org.springframework.util.Assert;

/**
 * This populator created for order pull back added items ESP Event service
 * @author Manikandan
 */
public class BlOrderPullBackItemsAddedRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderPullBackRequest> {

  /**
   * Populate the  instance with values from the OrderModel.
   *
   * @param order the source object
   * @param orderPullBackRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel order, final OrderPullBackRequest orderPullBackRequest) throws ConversionException {
    Assert.notNull(order, "Parameter order cannot be null.");
    Assert.notNull(orderPullBackRequest, "Parameter orderPullBackRequest cannot be null.");
    final UserModel userModel = order.getUser();
    if(Objects.nonNull(userModel)) {
      orderPullBackRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderPullBackRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_PULL_BACK_ADDED_ITEMS_EVENT_DEFINITION_KEY)));
    final OrderPullBackItems data = new OrderPullBackItems();
    data.setOrderid(order.getCode());
    data.setSubscriberid(getRequestValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.BORROW_LENSES_SUBSCRIBER_ID)));
    populateOrderDataForOrderPullBackItems(order , data , StringUtils.EMPTY,
        Collections.emptyList());
    orderPullBackRequest.setData(data);
  }

}
