package com.bl.core.event;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.GiftCardEmailProcessModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.acceleratorservices.site.AbstractAcceleratorSiteEventListener;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.commerceservices.enums.SiteChannel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.Collections;
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

  private ModelService modelService;
  private BusinessProcessService businessProcessService;


  private DefaultBlESPEventService defaultBlESPEventService;

  protected BusinessProcessService getBusinessProcessService() {
    return businessProcessService;
  }

  public void setBusinessProcessService(final BusinessProcessService businessProcessService) {
    this.businessProcessService = businessProcessService;
  }

  protected ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

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

    final GiftCardEmailProcessModel giftCardEmailProcessModel = (GiftCardEmailProcessModel) getBusinessProcessService()
        .createProcess(
            BlCoreConstants.GIFT_CARD + event.getUserEmail() + BlCoreConstants.HYPHEN + System
                .currentTimeMillis(), BlCoreConstants.GIFT_CARD_EMAIL_PROCESS);
    giftCardEmailProcessModel.setSite(event.getSite());
    giftCardEmailProcessModel.setLanguage(event.getLanguage());
    giftCardEmailProcessModel.setCurrency(event.getCurrency());
    giftCardEmailProcessModel.setStore(event.getBaseStore());
    giftCardEmailProcessModel.setCustomerEmail(event.getUserEmail());
    giftCardEmailProcessModel.setGiftcard(event.getGiftcard());
    getModelService().save(giftCardEmailProcessModel);
    try {
      if(CollectionUtils.isNotEmpty(event.getGiftcard().getOrder()) && BooleanUtils.isTrue(event.getGiftcard().getIsPurchased())) {
        getDefaultBlESPEventService().sendGiftCardPurchase(event.getGiftcard());
      }
    }
    catch (final Exception e) {
      BlLogger.logMessage(LOGGER, Level.ERROR, "Failed to trigger Gift Card event.", e);
    }
    getBusinessProcessService().startProcess(giftCardEmailProcessModel);
  }

  public DefaultBlESPEventService getDefaultBlESPEventService() {
    return defaultBlESPEventService;
  }

  public void setDefaultBlESPEventService(
      DefaultBlESPEventService defaultBlESPEventService) {
    this.defaultBlESPEventService = defaultBlESPEventService;
  }


}
