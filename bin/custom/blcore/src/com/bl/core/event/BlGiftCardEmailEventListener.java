package com.bl.core.event;

import com.bl.core.model.GiftCardEmailProcessModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.acceleratorservices.site.AbstractAcceleratorSiteEventListener;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import de.hybris.platform.commerceservices.enums.SiteChannel;

/**
 * It is a custom implementation of OOTB class {@link AbstractAcceleratorSiteEventListener}.
 * @author Neeraj Singh
 */
public class BlGiftCardEmailEventListener extends AbstractAcceleratorSiteEventListener<BlGiftCardEmailEvent>
{
  private static final Logger LOGGER = Logger.getLogger(BlGiftCardEmailEventListener.class);

  private ModelService modelService;
  private BusinessProcessService businessProcessService;

  protected BusinessProcessService getBusinessProcessService()
  {
    return businessProcessService;
  }

  public void setBusinessProcessService(final BusinessProcessService businessProcessService)
  {
    this.businessProcessService = businessProcessService;
  }

  protected ModelService getModelService()
  {
    return modelService;
  }

  public void setModelService(final ModelService modelService)
  {
    this.modelService = modelService;
  }

  @Override
  protected SiteChannel getSiteChannelForEvent(BlGiftCardEmailEvent event)
  {
    final BaseSiteModel site = event.getSite();
    ServicesUtil.validateParameterNotNullStandardMessage("event.site", site);
    return site.getChannel();
  }

  @Override
  protected void onSiteEvent(BlGiftCardEmailEvent event)
  {
    BlLogger.logMessage(LOGGER, Level.INFO,"GiftCard Customer event listener: {}", event.getUserEmail());

    final GiftCardEmailProcessModel giftCardEmailProcessModel = (GiftCardEmailProcessModel) getBusinessProcessService()
        .createProcess("giftcard-" + event.getUserEmail() + "-" + System.currentTimeMillis(),
            "giftCardEmailProcess");
    giftCardEmailProcessModel.setSite(event.getSite());
    giftCardEmailProcessModel.setLanguage(event.getLanguage());
    giftCardEmailProcessModel.setCurrency(event.getCurrency());
    giftCardEmailProcessModel.setStore(event.getBaseStore());
    giftCardEmailProcessModel.setCustomerEmail(event.getUserEmail());
    giftCardEmailProcessModel.setGiftcard(event.getGiftcard());
    getModelService().save(giftCardEmailProcessModel);
    getBusinessProcessService().startProcess(giftCardEmailProcessModel);
  }

}
