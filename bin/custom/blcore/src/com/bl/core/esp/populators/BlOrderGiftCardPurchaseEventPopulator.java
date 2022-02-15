package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.GiftCardModel;
import com.bl.esp.dto.giftcard.GiftCardPurchaseEventRequest;
import com.bl.esp.dto.giftcard.data.GiftCardPurchaseData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

/**
 * This Populator created to prepare request for gift card purchase event
 * @author Manikandan
 */
public class BlOrderGiftCardPurchaseEventPopulator<SOURCE extends GiftCardModel , TARGET extends GiftCardPurchaseEventRequest>  implements
    Populator<SOURCE, TARGET>  {

  private ConfigurationService configurationService;

  /**
   * This method created to populate request from giftCardMovementModel
   * @param giftCardModel giftCardModel
   * @param giftCardPurchaseEventRequest giftCardPurchaseEventRequest
   * @throws ConversionException ConversionException
   */

  @Override
  public void populate(final GiftCardModel giftCardModel, final GiftCardPurchaseEventRequest giftCardPurchaseEventRequest) throws ConversionException {

    Assert.notNull(giftCardModel, "Parameter giftCardModel cannot be null.");
    Assert.notNull(giftCardPurchaseEventRequest,
        "Parameter giftCardPurchaseEventRequest cannot be null.");

    final UserModel userModel = giftCardModel.getCustomer();
    if (Objects.nonNull(userModel)) {
      giftCardPurchaseEventRequest.setContactKey(getObjectValue(userModel.getUid()));
    }
    giftCardPurchaseEventRequest
        .setEventDefinitionKey(getObjectValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_GIFT_CARD_EVENT_DEFINITION_KEY)));
     final GiftCardPurchaseData giftCardPurchaseData = new GiftCardPurchaseData();
     populateGiftCardDetails(giftCardModel , giftCardPurchaseData , giftCardPurchaseEventRequest.getOrderModel());
     giftCardPurchaseEventRequest.setData(giftCardPurchaseData);
     giftCardPurchaseEventRequest.setOrderModel(null);
  }

  /**
   * This method created to populate gift card details
   * @param giftCardModel giftCardModel
   * @param giftCardPurchaseData giftCardPurchaseData
   * @param abstractOrderModel abstractOrderModel
   */
  private void populateGiftCardDetails(final GiftCardModel giftCardModel,
      final GiftCardPurchaseData giftCardPurchaseData, final AbstractOrderModel abstractOrderModel) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    giftCardPurchaseData.setSubscriberid(getObjectValue(giftCardModel.getCustomerEmail()));
    giftCardPurchaseData.setOrderid(abstractOrderModel.getCode());
    giftCardPurchaseData.setTemplate(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_GIFT_CARD_EVENT_TEMPLATE));
    giftCardPurchaseData.setGiftcardamount(formatAmount(giftCardModel.getAmount()));
    giftCardPurchaseData.setGiftcardcode(getObjectValue(giftCardModel.getCode()));
    giftCardPurchaseData.setCustomername(getObjectValue(giftCardModel.getName()));
    giftCardPurchaseData.setCustomeremail(getObjectValue(giftCardModel.getCustomerEmail()));
    giftCardPurchaseData.setDatePlaced(formatter.format(abstractOrderModel.getDate()));
  }

  /**
     * To get value and return value empty if empty
     * @param value value get from order
     * @return value to set on request
     */
    protected String getObjectValue(final String value){
      return StringUtils.isBlank(value) ? StringUtils.EMPTY :value;
    }

  /**
   * This method created to format the amount for double
   * @param amount the amount
   * @return the string
   */
  protected String formatAmount(final Double amount) {
    final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat.getNumberInstance(Locale.ENGLISH);
    decimalFormat.applyPattern(BlCoreConstants.FORMAT_STRING);
    return decimalFormat.format(amount);
  }

  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  public void setConfigurationService(
      ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }

  }





