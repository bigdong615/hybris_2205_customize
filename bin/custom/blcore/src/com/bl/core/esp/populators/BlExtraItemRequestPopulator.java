package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.NotesEnum;
import com.bl.esp.dto.extraItem.OrderExtraItemRequest;
import com.bl.esp.dto.extraItem.data.OrderExtraItemData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

/**
 * This populator created for order Extra Item ESP Event
 * @author Manikandan
 */
public class BlExtraItemRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderExtraItemRequest> {


  /**
   * Populate the order Extra Item instance with values from the OrderModel.
   *
   * @param orderModel the source object
   * @param orderExtraItemRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel orderModel, final OrderExtraItemRequest orderExtraItemRequest)
      throws ConversionException {

    Assert.notNull(orderModel, "Parameter order cannot be null.");
    Assert.notNull(orderExtraItemRequest, "Parameter orderConfirmationEventRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if(Objects.nonNull(userModel)) {
      orderExtraItemRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderExtraItemRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_EXTRA_ITEM_EVENT_DEFINITION_KEY)));
    populateOrderData(orderModel, orderExtraItemRequest);

  }

  /**
   * This method populate order data from order model
   * @param orderModel orderodel
   * @param orderExtraItemRequest request to be get updated
   */

  private void populateOrderData(final OrderModel orderModel, final OrderExtraItemRequest orderExtraItemRequest) {

    final OrderExtraItemData orderExtraItemData  = new OrderExtraItemData();
    populateCommonData(orderModel , orderExtraItemData);
    orderExtraItemData.setOldorderid(StringUtils.EMPTY);
    orderExtraItemData.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_EXTRA_ITEM_EVENT_TEMPLATE)));
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    orderExtraItemData.setDateplaced(formatter.format(orderModel.getDate()));
    orderExtraItemData.setItems(getCustomerOrderNotes(orderModel));
    orderExtraItemData.setReturnfee(String.valueOf(100.00)); // TODO Setting dummy value, once we got the actual value then set actual value one
    orderExtraItemRequest.setData(orderExtraItemData);

  }

  /**
   * This method is to get the customer order notes from order model
   * @param orderModel ordermodel
   * @return values to set on request
   */
  protected String getCustomerOrderNotes(final OrderModel orderModel) {
    final StringBuilder orderNotes = new StringBuilder();

    if(CollectionUtils.isNotEmpty(orderModel.getOrderNotes())){
      orderModel.getOrderNotes().forEach(notesModel -> {
        if(NotesEnum.CUSTOMER_OWNED_ITEMS_NOTES.getCode().equalsIgnoreCase(notesModel.getType().getCode())) {
          orderNotes.append(StringUtils.EMPTY + notesModel.getNote());
        }
      });
    }
    return orderNotes.toString();
  }
}
