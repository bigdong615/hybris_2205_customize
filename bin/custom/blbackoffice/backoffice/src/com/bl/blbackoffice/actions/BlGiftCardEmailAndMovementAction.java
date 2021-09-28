package com.bl.blbackoffice.actions;

import com.bl.core.event.BlGiftCardEmailEvent;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import de.hybris.platform.commerceservices.event.AbstractCommerceUserEvent;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.services.BaseStoreService;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zul.Messagebox;
import de.hybris.platform.core.enums.OrderStatus;
/**
 * This class creates gift card movement and triggers email event.
 * @author Neeraj Singh
 */
public class BlGiftCardEmailAndMovementAction implements CockpitAction<GiftCardModel, String> {

  private static final Logger LOGGER = Logger.getLogger(BlGiftCardEmailAndMovementAction.class);

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

  /**
   * It creates gift card movement and sends an email.
   */
  @Override
  public ActionResult<String> perform(final ActionContext<GiftCardModel> actionContext) {
    final GiftCardModel giftCardModel = actionContext.getData();
    if (giftCardModel != null) {
      createGiftCardMovement(giftCardModel);
      ActionResult<String> actionResultForEmail = getStringActionResultForEmail(actionContext,
          giftCardModel);
      if (actionResultForEmail != null ) {
        if(CollectionUtils.isNotEmpty(giftCardModel.getOrder())){
      	final OrderModel order =(OrderModel) giftCardModel.getOrder().get(0);
      	// After send gift card purchase email to customer change order status
      	if(order.isGiftCardOrder()) {
          order.setStatus(OrderStatus.COMPLETED);
          modelService.save(order);
          modelService.refresh(order);
        }
      	}
      	return actionResultForEmail;
      }
    }
    Messagebox.show(giftCardModel + " (" + ActionResult.ERROR + ")",
        actionContext.getLabel("action.send.registration.invite.sent.title"), Messagebox.OK,
        Messagebox.ERROR);
    return new ActionResult<>(ActionResult.ERROR);
  }

  /**
   * Create GiftCardMovement for GiftCard.
   *
   * @param giftCardModel
   */
  private void createGiftCardMovement(final GiftCardModel giftCardModel) {
    GiftCardMovementModel movement;
    try {
      if (giftCardModel.getAmount() != null && giftCardModel.getAmount().doubleValue()  != 0 && CollectionUtils.isEmpty(giftCardModel.getMovements())) {
        movement = modelService.create(GiftCardMovementModel.class);
        movement.setAmount(giftCardModel.getAmount());
        movement.setCommitted(Boolean.TRUE);
        movement.setCurrency(giftCardModel.getCurrency());
        movement.setGiftCard(giftCardModel);
        movement.setTransactionId(giftCardModel.getCode());
        if(JaloSession.getCurrentSession() != null) {
          final String user = JaloSession.getCurrentSession().getUser().getUid();
          giftCardModel.setIssuer(user);
        }
        modelService.save(movement);
        modelService.save(giftCardModel);
        modelService.refresh(giftCardModel);
        modelService.refresh(movement);
      }
    } catch (final Exception exception) {
      BlLogger
          .logMessage(LOGGER, Level.ERROR, "Unable to create gift card movement for gift card {}",
              giftCardModel.getCode(), exception);
    }
  }

  /**
   * It sends an email to customer, if email attribute is true in GiftCardModel.
   *
   * @param actionContext
   * @param giftCardModel
   * @return
   */
  private ActionResult<String> getStringActionResultForEmail(
      final ActionContext<GiftCardModel> actionContext, final GiftCardModel giftCardModel) {
    try {
      if (Boolean.TRUE.equals(giftCardModel.getEmail())) {
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
      BlLogger.logMessage(LOGGER, Level.ERROR, "Unable to send gift card email to {}",
          giftCardModel.getCustomerEmail(), exception);
    }
    return null;
  }

  /**
   * It enables create movement icon to create the gift card movement, when Active attribute is true .
   */
  @Override
  public boolean canPerform(final ActionContext<GiftCardModel> ctx) {
    final GiftCardModel giftCardModel = ctx.getData();

    return (giftCardModel != null && giftCardModel.getActive() != null && giftCardModel.getActive());
  }

  /**
   * It gets called when we click on the create movement icon.
   */
  @Override
  public boolean needsConfirmation(final ActionContext<GiftCardModel> ctx) {
    return true;
  }

  /**
   * It opens create movement confirmation model.
   * @param giftCardModelActionContext
   * @return String
   */
  @Override
  public String getConfirmationMessage(final ActionContext<GiftCardModel> giftCardModelActionContext) {
    return giftCardModelActionContext.getLabel("action.send.registration.invite.confirm");
  }

  /**
   * It sets the required value to BlGiftCardEmailEvent.
   */
  private AbstractCommerceUserEvent initializeEvent(final BlGiftCardEmailEvent event,
      final GiftCardModel giftCardModel) {

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