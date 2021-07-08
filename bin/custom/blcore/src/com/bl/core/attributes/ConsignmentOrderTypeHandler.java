package com.bl.core.attributes;

import com.bl.core.enums.OrderTypeEnum;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.attribute.AbstractDynamicAttributeHandler;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Handler to display order type value in Order fulfillment and CS cockpits.
 *
 * @author Sunil
 */
public class ConsignmentOrderTypeHandler extends
    AbstractDynamicAttributeHandler<OrderTypeEnum, ConsignmentModel> {

  private static final Logger LOGGER = Logger.getLogger(ConsignmentOrderTypeHandler.class);

  @Override
  public OrderTypeEnum get(final ConsignmentModel consignmentModel) {

      final AbstractOrderModel orderModel = consignmentModel.getOrder();

    if (null == orderModel || null == orderModel.getOrderType()) {

      BlLogger.logFormatMessageInfo(LOGGER, Level.INFO,
          "Returning null value for order type of consignment with code {}, as orderModel or order type is found to be null",
          consignmentModel.getCode());

      return null;
      }

    BlLogger.logFormatMessageInfo(LOGGER, Level.INFO,
        "Order type - {} is being set to consignment with code {}.",
        orderModel.getOrderType(), consignmentModel.getCode());

    return orderModel.getOrderType();
  }

}
