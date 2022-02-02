package com.bl.core.event;

import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlProductModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.acceleratorservices.site.AbstractAcceleratorSiteEventListener;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.commerceservices.enums.SiteChannel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is a custom implementation of OOTB class {@link AbstractAcceleratorSiteEventListener} to
 * trigger the gift card email.
 *
 * @author Neeraj Singh
 */
public class BlGiftCardEmailEventListener extends
    AbstractAcceleratorSiteEventListener<BlGiftCardEmailEvent> {

  private static final Logger LOGGER = Logger.getLogger(BlGiftCardEmailEventListener.class);

  private DefaultBlESPEventService defaultBlESPEventService;

  /**
   * It returns site channel.
   * @param event
   * @return SiteChannel
   */
  @Override
  protected SiteChannel getSiteChannelForEvent(BlGiftCardEmailEvent event) {
    final BaseSiteModel site = event.getSite();
    ServicesUtil.validateParameterNotNullStandardMessage("event.site", site);
    return site.getChannel();
  }

  /**
   * It creates giftCardEmailProcess and starts it.
   * @param event
   */
  @Override
  protected void onSiteEvent(final BlGiftCardEmailEvent event) {
    BlLogger.logMessage(LOGGER, Level.INFO, "GiftCard Customer event listener for user: {}",
        event.getUserEmail());
    AtomicReference<AbstractOrderModel> abstractOrderModel = new AtomicReference<>();
    try {
      if(CollectionUtils.isNotEmpty(event.getGiftcard().getOrder()) && BooleanUtils.isTrue(event.getGiftcard().getIsPurchased()) &&
          isGiftCardAllowedToSendESPRequest(event.getGiftcard().getOrder() , abstractOrderModel)) {
        getDefaultBlESPEventService().sendGiftCardPurchase(event.getGiftcard() , abstractOrderModel);
      }
    }
    catch (final Exception e) {
      BlLogger.logMessage(LOGGER, Level.ERROR, "Failed to trigger Gift Card event.", e);
    }
  }



  /**
   * This method created to get whether the order purchased through storefront
   * @param order order model
   * @param orderModel orderModel
   * @return boolean
   */
  private boolean isGiftCardAllowedToSendESPRequest(final List<AbstractOrderModel> order,
      final AtomicReference<AbstractOrderModel> orderModel) {
    order.forEach(abstractOrderModel -> {
      if(BooleanUtils.isTrue(abstractOrderModel.isGiftCardOrder()) && CollectionUtils.isNotEmpty(abstractOrderModel.getEntries())) {
        abstractOrderModel.getEntries().forEach(abstractOrderEntryModel -> {
          if(abstractOrderEntryModel.getProduct() instanceof BlProductModel) {
            final BlProductModel blProductModel = (BlProductModel) abstractOrderEntryModel.getProduct();
            if(ProductTypeEnum.GIFTCARD.getCode().equalsIgnoreCase(blProductModel.getProductType().getCode())){
              orderModel.set(abstractOrderModel);
            }
          }
        });
      }
    });
      return Objects.nonNull(orderModel.get());
  }

  public DefaultBlESPEventService getDefaultBlESPEventService() {
    return defaultBlESPEventService;
  }

  public void setDefaultBlESPEventService(
      DefaultBlESPEventService defaultBlESPEventService) {
    this.defaultBlESPEventService = defaultBlESPEventService;
  }

}
