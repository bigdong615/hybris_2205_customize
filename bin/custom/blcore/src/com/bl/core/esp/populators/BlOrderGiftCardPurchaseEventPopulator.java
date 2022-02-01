package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlProductModel;
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
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
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
      giftCardPurchaseEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    giftCardPurchaseEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_GIFT_CARD_EVENT_DEFINITION_KEY)));
     final GiftCardPurchaseData giftCardPurchaseData = new GiftCardPurchaseData();
     populateGiftCardDetails(giftCardModel , giftCardPurchaseData);
     giftCardPurchaseEventRequest.setData(giftCardPurchaseData);
  }

  /**
   * This method created to populate gift card details
   * @param giftCardModel giftCardModel
   * @param giftCardPurchaseData giftCardPurchaseData
   */
  private void populateGiftCardDetails(final GiftCardModel giftCardModel, final GiftCardPurchaseData giftCardPurchaseData) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final AbstractOrderModel abstractOrderModel = getGiftCardOrderModel(giftCardModel.getOrder());
    giftCardPurchaseData.setSubscriberid(getRequestValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.BORROW_LENSES_SUBSCRIBER_ID)));
    giftCardPurchaseData.setOrderid(abstractOrderModel.getCode());
    giftCardPurchaseData.setTemplate(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_GIFT_CARD_EVENT_TEMPLATE));
    giftCardPurchaseData.setGiftcardamount(formatAmount(giftCardModel.getAmount()));
    giftCardPurchaseData.setGiftcardcode(getRequestValue(giftCardModel.getCode()));
    giftCardPurchaseData.setCustomername(getRequestValue(giftCardModel.getName()));
    giftCardPurchaseData.setCustomeremail(getRequestValue(giftCardModel.getCustomerEmail()));
    giftCardPurchaseData.setDatePlaced(formatter.format(abstractOrderModel.getDate()));
  }

  /**
   * This method created to get order model from list
   * @param abstractOrderModelList abstractOrderModel
   * @return abstractOrderModel
   */
  private AbstractOrderModel getGiftCardOrderModel(final List<AbstractOrderModel> abstractOrderModelList) {
    final AtomicReference<AbstractOrderModel> orderModel = new AtomicReference<>();
    if(CollectionUtils.isNotEmpty(abstractOrderModelList)) {
      abstractOrderModelList.forEach(abstractOrderModel -> {
        if(BooleanUtils.isTrue(abstractOrderModel.isGiftCardOrder()) && CollectionUtils.isNotEmpty(abstractOrderModel.getEntries())) {
          checkOrderEntryHasGiftCard(abstractOrderModel , orderModel);
        }
      });
    }
    return orderModel.get();
  }

  /**
   * This method created to check whether the entries as gift card
   * @param abstractOrderModel  abstractOrderModel
   * @param orderModel orderModel
   */
  private void checkOrderEntryHasGiftCard(final AbstractOrderModel abstractOrderModel,
      final AtomicReference<AbstractOrderModel> orderModel){
    abstractOrderModel.getEntries().forEach(abstractOrderEntryModel -> {
      if(abstractOrderEntryModel.getProduct() instanceof BlProductModel) {
        final BlProductModel blProductModel = (BlProductModel) abstractOrderEntryModel.getProduct();
        if(ProductTypeEnum.GIFTCARD.getCode().equalsIgnoreCase(blProductModel.getProductType().getCode())){
          orderModel.set(abstractOrderModel);
        }
      }
    });
  }
  /**
     * To get the request value based
     * @param value value get from order
     * @return value to set on request
     */
    protected String getRequestValue(final String value){
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





