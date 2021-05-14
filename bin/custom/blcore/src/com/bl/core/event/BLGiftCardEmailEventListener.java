package com.bl.core.event;

import com.bl.core.model.GiftCardEmailProcessModel;
import de.hybris.platform.acceleratorservices.site.AbstractAcceleratorSiteEventListener;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import org.springframework.beans.factory.annotation.Required;

import de.hybris.platform.commerceservices.enums.SiteChannel;

/**
 * @author
 *
 */
public class BLGiftCardEmailEventListener extends AbstractAcceleratorSiteEventListener<BLGiftCardEmailEvent>
{
   // private static final Logger LOG = Logger.getLogger(GiftCardEmailEventListener.class);

    private ModelService modelService;
    private BusinessProcessService businessProcessService;

    protected BusinessProcessService getBusinessProcessService()
    {
        return businessProcessService;
    }

    @Required
    public void setBusinessProcessService(final BusinessProcessService businessProcessService)
    {
        this.businessProcessService = businessProcessService;
    }

    protected ModelService getModelService()
    {
        return modelService;
    }

    @Required
    public void setModelService(final ModelService modelService)
    {
        this.modelService = modelService;
    }

    @Override
    protected SiteChannel getSiteChannelForEvent(BLGiftCardEmailEvent event)
    {
        final BaseSiteModel site = event.getSite();
        ServicesUtil.validateParameterNotNullStandardMessage("event.site", site);
        return site.getChannel();
    }

    @Override
    protected void onSiteEvent(BLGiftCardEmailEvent event)
    {
        //LOG.info("GiftCard Customer event listener: " + event.getCustomer());

        final GiftCardEmailProcessModel giftCardEmailProcessModel = (GiftCardEmailProcessModel) getBusinessProcessService()
                .createProcess("giftcard-" + event.getCustomer().getUid() + "-" + System.currentTimeMillis(),
                        "giftCardEmailProcess");
        giftCardEmailProcessModel.setSite(event.getSite());
        giftCardEmailProcessModel.setCustomer(event.getCustomer());
        giftCardEmailProcessModel.setLanguage(event.getLanguage());
        giftCardEmailProcessModel.setCurrency(event.getCurrency());
        giftCardEmailProcessModel.setStore(event.getBaseStore());
        giftCardEmailProcessModel.setCustomerEmail(event.getCustomer().getContactEmail());
        giftCardEmailProcessModel.setGiftcard(event.getGiftcard());

        getModelService().save(giftCardEmailProcessModel);
        getBusinessProcessService().startProcess(giftCardEmailProcessModel);
    }

}
