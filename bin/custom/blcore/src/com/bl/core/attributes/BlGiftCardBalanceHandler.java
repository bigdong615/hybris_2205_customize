package com.bl.core.attributes;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.servicelayer.model.attribute.AbstractDynamicAttributeHandler;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * It assigns dynamic amount to gift card balance.
 *
 * @author Neeraj Singh
 */
public class BlGiftCardBalanceHandler extends
    AbstractDynamicAttributeHandler<Double, GiftCardModel> {

  private static final Logger LOGGER = Logger.getLogger(BlGiftCardBalanceHandler.class);

  @Override
  public Double get(final GiftCardModel giftCardModel) {
    double balance = 0;

    try {
      final List<GiftCardMovementModel> movements = giftCardModel.getMovements();
      if (CollectionUtils.isNotEmpty(movements)) {
        for (final GiftCardMovementModel giftCardMovementModel : movements) {
          balance += giftCardMovementModel.getAmount();
        }
      }

    } catch (final Exception exception) {
      BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR,
          "Error occurred while assigning amount to gift card balance for gift card code: {}",
          giftCardModel.getCode(), exception);
    }
    return balance;
  }
}
