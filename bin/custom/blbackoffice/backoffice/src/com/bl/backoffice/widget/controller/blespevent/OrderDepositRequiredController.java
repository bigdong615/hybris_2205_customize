package com.bl.backoffice.widget.controller.blespevent;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.util.localization.Localization;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

/**
 * This method created for Order Deposit Required ESP Event
 * @author Manikandan
 */
public class OrderDepositRequiredController extends DefaultWidgetController {  // NOSONAR

  private static final Logger LOG = Logger.getLogger(OrderDepositRequiredController.class);

  protected static final String OUT_CONFIRM = "orderdepositrequired";
  protected static final String COMPLETE = "completed";
  private static final String MESSAGE_BOX_TITLE = "success.message.depositRequired.title";
  private static final String MESSAGE_BOX_TEXT = "success.message.depositRequired.emailsent";
  private static final String MESSAGE_BOX_ERROR_TEXT = "error.message.depositRequired.emailsent";

  @Wire
  private Textbox amount;

  private OrderModel orderModel;

  private DefaultBlESPEventService blEspEventService;


  /**
   * This method is used to show the default values of deposit required
   * @param inputObject inputObject
   */
  @SocketEvent(socketId = "inputObject")
  public void initDepositRequiredForm(final OrderModel inputObject) {
    this.setOrderModel(inputObject);
    this.amount.setValue("");
    this.getWidgetInstanceManager()
        .setTitle(String.valueOf(
            this.getWidgetInstanceManager().getLabel("blbackoffice.depositrequired.confirm.title"))
            .concat(BlInventoryScanLoggingConstants.EMPTY_SPACE)
            .concat(this.getOrderModel().getCode()));
  }

  /**
   * This method will be used to reset the popup values
   */
  @ViewEvent(componentID = "undochanges", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
  public void reset() {
    this.amount.setValue("");
  }

  /**
   * This method will be used to confirm/ Save the modified values to trigger ESP event
   */
  @ViewEvent(componentID = "confirmTriggerEmail", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
  public void triggerESPEvent() {

    this.validateRequest();

    try {
      triggerDepositRequiredESPEvent(this.orderModel, getDoubleValue(this.amount.getValue()));

    } catch (final Exception ex) {

      BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger Deposit required event.", ex);
      this.showMessageBox(true);
    }

    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }

  /**
   * This method will be used to trigger Deposit required ESP event
   */
  private void triggerDepositRequiredESPEvent(final OrderModel orderModel, final Double amount) {

    try {
      getBlEspEventService().sendOrderDepositRequired(orderModel, amount);
      this.showMessageBox(false);

    } catch (final Exception e) {
      BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger deposit required event.", e);
    }
  }


  /**
   * This method will be used to validate modified data
   */
  protected void validateRequest() {

    if (StringUtils.isEmpty(this.amount.getValue())) {
      throw new WrongValueException(this.amount,
          this.getLabel("blbackoffice.depositrequired.missing.amount"));
    } else {
      getDoubleValue(this.amount.getValue());
    }

  }

  /**
   * This method created to get double value
   * @param amount
   * @return
   */
  private Double getDoubleValue(final String amount) {

    Double doubleValue = 0d;
    try {
      doubleValue = Double.parseDouble(amount);
    } catch (final NumberFormatException e) {
      throw new WrongValueException(this.amount,
          this.getLabel("blbackoffice.depositrequired.amount.notanumber"));
    }

    return doubleValue;
  }

  /**
   * This method will be used to show success message
   *  @param isErrorMessage the is error message
   */
  protected void showMessageBox(final boolean isErrorMessage) {

    if (isErrorMessage) {

      Messagebox
          .show(Localization.getLocalizedString(MESSAGE_BOX_ERROR_TEXT),
              Localization.getLocalizedString(MESSAGE_BOX_TITLE), Messagebox.OK,
              Messagebox.ERROR);

    } else {
      Messagebox
          .show(Localization.getLocalizedString(MESSAGE_BOX_TEXT),
              Localization.getLocalizedString(MESSAGE_BOX_TITLE), Messagebox.OK,
              Messagebox.INFORMATION);
    }
  }


  public OrderModel getOrderModel()
  {
    return orderModel;
  }

  public void setOrderModel(final OrderModel orderModel)
  {
    this.orderModel = orderModel;
  }

  public DefaultBlESPEventService getBlEspEventService(){
    return blEspEventService;
  }

  public void setBlEspEventService(final DefaultBlESPEventService blEspEventService){
    this.blEspEventService = blEspEventService;
  }

}
