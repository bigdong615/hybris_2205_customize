package com.bl.blbackoffice.actions;

import com.bl.core.event.BLGiftCardEmailEvent;
import de.hybris.platform.commerceservices.event.AbstractCommerceUserEvent;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.lang.StringUtils;
import org.zkoss.zul.Messagebox;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;


/**
 * @author Admin
 *
 */
public class GiftcardEmailandMovementAction implements CockpitAction<GiftCardModel, String>
{
    @Resource(name = "modelService")
    private ModelService modelService;

    @Resource
    private EventService eventService;

    @Resource
    private BaseStoreService baseStoreService;

    @Resource
    private CommonI18NService commonI18NService;

    @Resource
    private BaseSiteService baseSiteService;


    @Override
    public ActionResult<String> perform(final ActionContext<GiftCardModel> actionContext)
    {
        final GiftCardModel giftCardModel = actionContext.getData();

        GiftCardMovementModel movement;
        List<GiftCardMovementModel> movements = null;
        String emailid;

        if (giftCardModel != null)
        {

            try {
                if (giftCardModel.getAmount().doubleValue() != 0 && giftCardModel.getMovements().isEmpty()) {
                    movement = modelService.create(GiftCardMovementModel.class);
                    movement.setAmount(giftCardModel.getAmount());
                    movement.setCommited(Boolean.TRUE);
                    movement.setCurrency(giftCardModel.getCurrency());
                    movement.setGiftCard(giftCardModel);
                    movement.setTransactionId(giftCardModel.getCode());

                    final String user = JaloSession.getCurrentSession().getUser().getUid();
                    giftCardModel.setIssuer(user);


                    modelService.save(movement);
                    modelService.save(giftCardModel);
                    modelService.refresh(giftCardModel);
                    modelService.refresh(movement);

                }

                if (giftCardModel.getEmail().booleanValue()) {
                    //				if (!giftCardModel.getDisputeorder().isEmpty())
                    //				{
                    //					emailid = giftCardModel.getDisputeorder().get(0).getUser().getUid();
                    //				}
                    //
                    //				else
                    //				{
                    emailid = giftCardModel.getCustomer().getUid();
                    //				}

                    if (StringUtils.isNotEmpty(emailid)) {
                        //send gift card email to customer
                        eventService.publishEvent(initializeEvent(new BLGiftCardEmailEvent(), giftCardModel));

                        Messagebox.show(actionContext.getLabel("action.send.registration.invite.sent.title"),
                                actionContext.getLabel("action.send.registration.invite.sent"), Messagebox.OK, Messagebox.INFORMATION);
                        return new ActionResult<String>(ActionResult.SUCCESS);

                    } else {

                        Messagebox.show(actionContext.getLabel("action.send.registration.invite.sent.error"),
                                actionContext.getLabel("action.send.registration.invite.sent.error"), Messagebox.OK, Messagebox.ERROR);
                        return new ActionResult<String>(ActionResult.ERROR);

                    }
                }
                else
            {
                Messagebox.show(actionContext.getLabel("action.send.saved"), actionContext.getLabel("action.send.saved"),
                        Messagebox.OK, Messagebox.INFORMATION);
                return new ActionResult<String>(ActionResult.SUCCESS);
            }
            }catch(Exception exception){
                  //Put log here.
            }
        }

        Messagebox.show(giftCardModel + " (" + ActionResult.ERROR + ")",
                actionContext.getLabel("action.send.registration.invite.sent.title"), Messagebox.OK, Messagebox.ERROR);
        return new ActionResult<String>(ActionResult.ERROR);

    }


    @Override
    public boolean canPerform(final ActionContext<GiftCardModel> ctx)
    {
        final GiftCardModel giftCardModel = ctx.getData();
        if (giftCardModel != null)
        {
            boolean canPerform = false;


            if (giftCardModel.getEmail() != null && giftCardModel.getActive() != null && giftCardModel.getActive().booleanValue() && giftCardModel.getEmail().booleanValue())
            {


                    canPerform = true;


            }

            return canPerform;
        }



        return false;

    }

    @Override
    public boolean needsConfirmation(final ActionContext<GiftCardModel> ctx)
    {
        return true;
    }

    @Override
    public String getConfirmationMessage(final ActionContext<GiftCardModel> ctx)
    {
        return ctx.getLabel("action.send.registration.invite.confirm");
    }



   protected AbstractCommerceUserEvent initializeEvent(final BLGiftCardEmailEvent event, final GiftCardModel giftCardModel)
    {

    //		LOG.info("Gift Code: " + giftCardModel.getCode());

            /*if (!giftCardModel.getDisputeorder().isEmpty() && giftCardModel.getDisputeorder() != null)
    		{
    			event.setBaseStore(giftCardModel.getDisputeorder().get(0).getStore());
    			event.setSite(giftCardModel.getDisputeorder().get(0).getSite());
    			event.setCurrency(giftCardModel.getDisputeorder().get(0).getCurrency());
    			event.setCustomer((CustomerModel) giftCardModel.getDisputeorder().get(0).getUser());
    			event.setGiftcard(giftCardModel);

    		}*/

                event.setBaseStore(baseStoreService.getAllBaseStores().iterator().next());
    			event.setSite(baseSiteService.getAllBaseSites().iterator().next());
    			event.setGiftcard(giftCardModel);
    			if (giftCardModel.getCustomer() != null)
    			{
    				event.setCustomer(giftCardModel.getCustomer());
    			}
    			event.setCurrency(commonI18NService.getCurrentCurrency());

    			event.setLanguage(commonI18NService.getCurrentLanguage());

            return event;
    	}

}

