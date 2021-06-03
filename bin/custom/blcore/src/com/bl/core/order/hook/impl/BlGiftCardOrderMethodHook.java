package com.bl.core.order.hook.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.gitfcard.BlGiftCardService;
import de.hybris.platform.commerceservices.order.hook.CommercePlaceOrderMethodHook;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.util.DiscountValue;
import java.util.Date;
import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;


/**
 * It is a custom implementation of OOTB class {@link CommercePlaceOrderMethodHook} to do the adjustments in case of gift card is applied on the order.
 * @author Neeraj Singh
 */
public class BlGiftCardOrderMethodHook implements CommercePlaceOrderMethodHook {

  private EventService eventService;
  private ModelService modelService;

  @Resource(name = "giftCardService")
  private BlGiftCardService giftCardService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void afterPlaceOrder(final CommerceCheckoutParameter commerceCheckoutParameter,
      final CommerceOrderResult commerceOrderResult) {
    final OrderModel order = commerceOrderResult.getOrder();
    getModelService().refresh(order);
    ServicesUtil.validateParameterNotNullStandardMessage("order", order);

    double discountValue = 0.00D;
    if (CollectionUtils.isNotEmpty(order.getGlobalDiscountValues())) {
      for (final DiscountValue orderDiscounts : order.getGlobalDiscountValues()) {
        discountValue += orderDiscounts.getValue();
      }
    }

    // add tax to total only if order is NET
    double totalplustax = order.getTotalPrice().doubleValue();
    if (Boolean.TRUE.equals(order.getNet())) {
      totalplustax += order.getTotalTax().doubleValue();
      //commented code can be used while implementing place order using gift card. If not required remove it later on.
      			/*for (final AbstractOrderEntryModel abstractOrderEntryModel : order.getEntries())
      			{
      				final Double oraclePrice = (abstractOrderEntryModel.getTotalPrice() / abstractOrderEntryModel.getQuantity());
      				BigDecimal rounded = new BigDecimal(oraclePrice);
      				rounded = rounded.setScale(2, RoundingMode.HALF_UP);
      				abstractOrderEntryModel.setOraclePrice(rounded.doubleValue());
      				getModelService().save(abstractOrderEntryModel);
      			}
    } else {
      			try
      			{
      				final double total = order.getTotalPrice().doubleValue();
      				final double totalTax = order.getTotalTax().doubleValue();
      				//Calculating discount tax SUS-1210
      				final double discountTax = (discountValue * order.getTaxRate()) / 100.0;
      				final double tax = totalTax - discountTax;
      				double taxRate = tax / (total - tax);
      				BigDecimal bd = new BigDecimal(taxRate);
      				bd = bd.setScale(2, RoundingMode.HALF_UP);
      				taxRate = bd.doubleValue() * 100;
      				               order.setTaxRate(taxRate);
      				order.setTotalTax(tax);

      				for (final AbstractOrderEntryModel abstractOrderEntryModel : order.getEntries())
      				{
      					final Double oraclePrice = (abstractOrderEntryModel.getTotalPrice() / abstractOrderEntryModel.getQuantity())
      							/ (1 + (order.getTaxRate().doubleValue() / 100));
      					BigDecimal rounded = new BigDecimal(oraclePrice);
      					rounded = rounded.setScale(2, RoundingMode.HALF_UP);
      					abstractOrderEntryModel.setOraclePrice(rounded.doubleValue());
      					getModelService().save(abstractOrderEntryModel);
      				}
      			}
      			catch (final Exception e)
      			{
                     LOG.error("Error calculating taxrate", e);
      			}
     }*/
    }
    giftCardService.calculateGiftCard(order, totalplustax);

    final List<GiftCardModel> giftCards = order.getGiftCard();
    if (giftCards != null) {
      for (final GiftCardModel giftCard : giftCards) {
        getModelService().refresh(giftCard);
        final List<GiftCardMovementModel> movements = giftCard.getMovements();
        for (final GiftCardMovementModel giftCardMovementModel : movements) {
          if (Boolean.FALSE.equals(giftCardMovementModel.getCommitted())) {
            giftCardMovementModel.setCommitted(Boolean.TRUE);
            giftCardMovementModel.setOrder(order);
            giftCardMovementModel.setRedeemDate(new Date());
            getModelService().save(giftCardMovementModel);
          }
        }
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void beforePlaceOrder(final CommerceCheckoutParameter commerceCheckoutParameter) {
    // not implemented
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void beforeSubmitOrder(final CommerceCheckoutParameter commerceCheckoutParameter,
      final CommerceOrderResult commerceOrderResult) {
    // not implemented
  }

  protected EventService getEventService() {
    return eventService;
  }

  public void setEventService(final EventService eventService) {
    this.eventService = eventService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }
}

