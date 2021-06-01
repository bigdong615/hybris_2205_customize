package com.bl.blbackoffice.actions;

import com.bl.core.event.BlGiftCardEmailEvent;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import de.hybris.platform.commerceservices.event.AbstractCommerceUserEvent;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.services.BaseStoreService;
import javax.annotation.Resource;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zul.Messagebox;


/**
 * This class creates gift card movement and triggers email event.
 * @author Neeraj Singh
 */
public class GiftcardEmailandMovementAction implements CockpitAction<GiftCardModel, String> {

  private static final Logger LOGGER = Logger.getLogger(GiftcardEmailandMovementAction.class);

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
  public ActionResult<String> perform(final ActionContext<GiftCardModel> actionContext) {
    final GiftCardModel giftCardModel = actionContext.getData();

    GiftCardMovementModel movement;
    //String emailid;
    if (giftCardModel != null) {
      try {
        if (giftCardModel.getAmount().doubleValue() != 0 && giftCardModel.getMovements()
            .isEmpty()) {
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
      }catch(Exception exception){
        BlLogger.logMessage(LOGGER, Level.ERROR,"Unable to create gift card movement for gift card {}", giftCardModel.getCode(), exception);
      }
      try{
        if (giftCardModel.getEmail().booleanValue()) {
          //Commented code can be used for order cancel. If not required then can be removed later on.
          /*if (!giftCardModel.getDisputeorder().isEmpty())
          {
            emailid = giftCardModel.getDisputeorder().get(0).getUser().getUid();
          }
          else{
          emailid = giftCardModel.getCustomerEmail();
          }*/
          if (StringUtils.isNotEmpty(giftCardModel.getCustomerEmail())) {
            //send gift card email to customer
            eventService.publishEvent(initializeEvent(new BlGiftCardEmailEvent(), giftCardModel));

            Messagebox.show(actionContext.getLabel("action.send.registration.invite.sent.title"),
                actionContext.getLabel("action.send.registration.invite.sent"), Messagebox.OK,
                Messagebox.INFORMATION);
            return new ActionResult<>(ActionResult.SUCCESS);
          } else {
            Messagebox.show(actionContext.getLabel("action.send.registration.invite.sent.error"),
                actionContext.getLabel("action.send.registration.invite.sent.error"), Messagebox.OK,
                Messagebox.ERROR);
            return new ActionResult<>(ActionResult.ERROR);

          }
        } else {
          Messagebox.show(actionContext.getLabel("action.send.saved"),
              actionContext.getLabel("action.send.saved"),
              Messagebox.OK, Messagebox.INFORMATION);
          return new ActionResult<>(ActionResult.SUCCESS);
        }
      } catch (Exception exception) {
        BlLogger.logMessage(LOGGER, Level.ERROR,"Unable to send gift card email to {}", giftCardModel.getCustomerEmail(), exception);
      }
    }
    Messagebox.show(giftCardModel + " (" + ActionResult.ERROR + ")",
        actionContext.getLabel("action.send.registration.invite.sent.title"), Messagebox.OK,
        Messagebox.ERROR);
    return new ActionResult<>(ActionResult.ERROR);
  }

  @Override
  public boolean canPerform(final ActionContext<GiftCardModel> ctx) {
    final GiftCardModel giftCardModel = ctx.getData();
    if (giftCardModel != null) {
      boolean canPerform = false;
      if (giftCardModel.getActive() != null && giftCardModel.getActive()) {
        canPerform = true;
      }
      return canPerform;
    }
    return false;
  }

  @Override
  public boolean needsConfirmation(final ActionContext<GiftCardModel> ctx) {
    return true;
  }

  @Override
  public String getConfirmationMessage(final ActionContext<GiftCardModel> ctx) {
    return ctx.getLabel("action.send.registration.invite.confirm");
  }

  protected AbstractCommerceUserEvent initializeEvent(final BlGiftCardEmailEvent event,
      final GiftCardModel giftCardModel) {

    //Commented code can be used for order cancel. Remove this later on if not required.
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
      if (StringUtils.isNotEmpty(giftCardModel.getCustomerEmail())) {
        event.setUserEmail(giftCardModel.getCustomerEmail());
      }
      event.setCurrency(commonI18NService.getCurrentCurrency());
      event.setLanguage(commonI18NService.getCurrentLanguage());
      return event;
  }
}