package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.GiftCardModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.esp.dto.giftcard.FreeGiftCardPurchaseEventRequest;
import com.bl.esp.dto.giftcard.data.GiftCardPurchaseData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

/**
 *This class created to Trigger Free Gift Card ESP Event
 * @author Manikandan
 */
public class BlFreeGiftCardPurchaseEventPopulator <SOURCE extends GiftCardModel, TARGET extends FreeGiftCardPurchaseEventRequest>  implements
    Populator<SOURCE, TARGET> {

  private ConfigurationService configurationService;

  /**
   * This method created to populate request from giftCardModel
   * @param giftCardModel giftCardModel
   * @param freeGiftCardPurchaseEventRequest freeGiftCardPurchaseEventRequest
   * @throws ConversionException ConversionException
   */

  @Override
  public void populate(final GiftCardModel giftCardModel, final FreeGiftCardPurchaseEventRequest freeGiftCardPurchaseEventRequest) throws ConversionException {

    Assert.notNull(giftCardModel, "Parameter giftCardModel cannot be null.");
    Assert.notNull(freeGiftCardPurchaseEventRequest,
        "Parameter freeGiftCardPurchaseEventRequest cannot be null.");

    final UserModel userModel = giftCardModel.getCustomer();
    if (Objects.nonNull(userModel)) {
      freeGiftCardPurchaseEventRequest.setContactKey(getObjectValue(userModel.getUid()));
    }
    freeGiftCardPurchaseEventRequest
        .setEventDefinitionKey(getObjectValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_GIFT_CARD_EVENT_DEFINITION_KEY)));
    final GiftCardPurchaseData giftCardPurchaseData = new GiftCardPurchaseData();
    populateGiftCardDetails(giftCardModel , giftCardPurchaseData);
    freeGiftCardPurchaseEventRequest.setData(giftCardPurchaseData);
  }

  /**
   * This method created to populate gift card details
   * @param giftCardModel giftCardModel
   * @param giftCardPurchaseData giftCardPurchaseData
   */
  private void populateGiftCardDetails(final GiftCardModel giftCardModel, final GiftCardPurchaseData giftCardPurchaseData) {
    giftCardPurchaseData.setSubscriberid(
        getObjectValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.BORROW_LENSES_SUBSCRIBER_ID)));
    giftCardPurchaseData.setTemplate(getConfigurationService().getConfiguration().getString(BlCoreConstants.FREE_GIFT_CARD_EVENT_TEMPLATE));
    giftCardPurchaseData.setGiftcardamount(BlDateTimeUtils.formatAmount(giftCardModel.getAmount()));
    giftCardPurchaseData.setGiftcardcode(getObjectValue(giftCardModel.getCode()));
    giftCardPurchaseData.setCustomername(getObjectValue(giftCardModel.getName()));
    giftCardPurchaseData.setCustomeremail(getObjectValue(giftCardModel.getCustomerEmail()));
  }

  /**
   * To get value and return value empty if empty
   * @param value value get from order
   * @return value to set on request
   */
  protected String getObjectValue(final String value){
    return StringUtils.isBlank(value) ? StringUtils.EMPTY :value;
  }



  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  public void setConfigurationService(
      ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }
}
