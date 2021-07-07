package com.bl.core.attributes;

import com.bl.core.enums.OrderTypeEnum;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.attribute.AbstractDynamicAttributeHandler;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * It assigns dynamic order type to consignment.
 *
 * @author Sunil
 */
public class ConsignmentOrderTypeHandler extends
    AbstractDynamicAttributeHandler<OrderTypeEnum, ConsignmentModel> {

  private static final Logger LOGGER = Logger.getLogger(ConsignmentOrderTypeHandler.class);

  @Override
  public OrderTypeEnum get(final ConsignmentModel consignmentModel) {
    OrderTypeEnum orderTypeEnum = null;

    try {
      final AbstractOrderModel orderModel = consignmentModel.getOrder();
      if (null != orderModel && null != orderModel.getOrderType()) {
        orderTypeEnum = orderModel.getOrderType();
      }

    } catch (final Exception exception) {
      BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR,
          "Error occurred while assigning order type to consignment for code: {}",
          consignmentModel.getCode(), exception);
    }
    return orderTypeEnum;
  }
}
