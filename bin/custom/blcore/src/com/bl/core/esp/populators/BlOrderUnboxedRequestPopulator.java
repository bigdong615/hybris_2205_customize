package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderunboxed.OrderUnBoxedEventRequest;
import com.bl.esp.dto.orderunboxed.data.OrderUnBoxedData;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

/**
 * This populator created to  populate order unboxed event ESP
 */
public class BlOrderUnboxedRequestPopulator  extends ESPEventCommonPopulator<OrderModel, OrderUnBoxedEventRequest> {

  private static final Logger LOG = Logger.getLogger(BlOrderUnboxedRequestPopulator.class);



  /**
   * Populate the Order Unboex Request instance with values from the OrderModel.
   *
   * @param order the source object
   * @param orderUnBoxedEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel order, final OrderUnBoxedEventRequest orderUnBoxedEventRequest)
      throws ConversionException {

    Assert.notNull(order, "Parameter order cannot be null.");
    Assert.notNull(orderUnBoxedEventRequest, "Parameter orderUnBoxedEventRequest cannot be null.");

    final UserModel userModel = order.getUser();
    if(Objects.nonNull(userModel)) {
      orderUnBoxedEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderUnBoxedEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_UNBOXED_EVENT_DEFINITION_KEY)));
    populateOrderData(order, orderUnBoxedEventRequest);

  }

  /**
   * This method populate order data from order model
   * @param order order
   * @param orderUnBoxedEventRequest request to be get updated
   */

  private void populateOrderData(final OrderModel order, final OrderUnBoxedEventRequest orderUnBoxedEventRequest) {

    try {

      final OrderUnBoxedData orderUnBoxedData = new OrderUnBoxedData();
      final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
      populateCommonData(order, orderUnBoxedData);
      orderUnBoxedData.setOldorderid(StringUtils.EMPTY);
      orderUnBoxedData.setTemplate(getRequestValue(getConfigurationService().getConfiguration()
          .getString(BlCoreConstants.ORDER_UNBOXED_EVENT_TEMPLATE)));
      orderUnBoxedData.setStatus(getRequestValue(BlCoreConstants.UN_BOXED));
      orderUnBoxedData.setType(BooleanUtils.isTrue(order.getIsRentalOrder()) ? BlCoreConstants.RENTAL
          : BlCoreConstants.USED_GEAR);
      orderUnBoxedData.setDateplaced(formatter.format(order.getDate()));
      orderUnBoxedData.setReturndate(formatter.format(order.getRentalEndDate()));
      orderUnBoxedData.setActualreturndate(formatter.format(order.getActualRentalEndDate()));
      orderUnBoxedEventRequest.setData(orderUnBoxedData);
    }
    catch (final Exception exception){
      BlLogger.logMessage(LOG , Level.ERROR , "Error while populating data for Unboxed ESP event" , exception);
    }
  }
}
