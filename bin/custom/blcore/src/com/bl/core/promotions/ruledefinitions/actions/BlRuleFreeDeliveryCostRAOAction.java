package com.bl.core.promotions.ruledefinitions.actions;

import com.bl.core.services.calculation.impl.DefaultBlRuleEngineCalculationService;
import com.bl.core.services.cart.BlCartService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.DeliveryModeService;
import de.hybris.platform.ruleengineservices.rao.AbstractRuleActionRAO;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rao.DeliveryModeRAO;
import de.hybris.platform.ruleengineservices.rao.RuleEngineResultRAO;
import de.hybris.platform.ruleengineservices.rao.ShipmentRAO;
import de.hybris.platform.ruleengineservices.rule.evaluation.RuleActionContext;
import de.hybris.platform.ruleengineservices.rule.evaluation.actions.AbstractRuleExecutableSupport;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Optional;
import java.util.function.Predicate;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class BlRuleFreeDeliveryCostRAOAction extends AbstractRuleExecutableSupport {
  private static final Logger LOG = Logger.getLogger(BlRuleFreeDeliveryCostRAOAction.class);
  private DefaultBlRuleEngineCalculationService defaultBlRuleEngineCalculationService;
  private DeliveryModeService deliveryModeService;
  private ModelService modelService;
  private BlCartService cartService;

  public boolean performActionInternal(RuleActionContext context) {
    Boolean value = (Boolean) context.getParameter("freeDeliveryCost");
    return this.performAction(context, value);
  }

  protected boolean performAction(RuleActionContext context, Boolean freeDeliveryMode) {
    CartRAO cartRao = context.getCartRao();
    BlLogger.logMessage(LOG, Level.INFO, "Entering BlRuleFreeDeliveryCostRAOAction");
    if(BooleanUtils.isTrue(freeDeliveryMode) && StringUtils.isNotBlank(cartRao.getOrderDeliveryMode())) {
      RuleEngineResultRAO result = context.getRuleEngineResultRao();
      Optional<DeliveryModeRAO> deliveryMode = this.getRaoLookupService().lookupRAOByType(DeliveryModeRAO.class, context, new Predicate[]{this.getDeliveryModeRAOFilter(cartRao.getOrderDeliveryMode())});
      if (deliveryMode.isPresent()) {
        this.changeDeliveryCost(cartRao, deliveryMode.get(), result, context);
        CartModel cartModel = getCartService().getSessionCart();
        if(cartModel != null){
          cartModel.setFreeShippingDeliverMode(getDeliveryModeService().getDeliveryModeForCode(cartRao.getOrderDeliveryMode()));
          getModelService().save(cartModel);
          getModelService().refresh(cartModel);
        }
        return true;
      }
    }
    return false;
  }


  public void changeDeliveryCost(CartRAO cartRao, DeliveryModeRAO mode, RuleEngineResultRAO result, RuleActionContext context) {
    this.validateRule(context);
    ShipmentRAO shipment = getDefaultBlRuleEngineCalculationService().changeDeliveryCost(cartRao ,mode);
    result.getActions().add(shipment);
    this.setRAOMetaData(context, new AbstractRuleActionRAO[]{shipment});
    context.scheduleForUpdate(new Object[]{cartRao, result});
    context.insertFacts(new Object[]{shipment});
    cartRao.getEntries().forEach((e) -> {
      this.getConsumptionSupport().consumeOrderEntry(e, shipment);
    });
  }

  protected Predicate<DeliveryModeRAO> getDeliveryModeRAOFilter(String deliveryModeCode) {
    return (o) -> {
      return this.isFactDeliveryAndHasCode(o, deliveryModeCode);
    };
  }
  protected boolean isFactDeliveryAndHasCode(Object fact, String deliveryModeCode) {
    return fact instanceof DeliveryModeRAO && deliveryModeCode.equals(((DeliveryModeRAO)fact).getCode());
  }

  public DefaultBlRuleEngineCalculationService getDefaultBlRuleEngineCalculationService() {
    return defaultBlRuleEngineCalculationService;
  }

  public void setDefaultBlRuleEngineCalculationService(
      DefaultBlRuleEngineCalculationService defaultBlRuleEngineCalculationService) {
    this.defaultBlRuleEngineCalculationService = defaultBlRuleEngineCalculationService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public BlCartService getCartService() {
    return cartService;
  }

  public void setCartService(BlCartService cartService) {
    this.cartService = cartService;
  }

  public DeliveryModeService getDeliveryModeService() {
    return deliveryModeService;
  }

  public void setDeliveryModeService(DeliveryModeService deliveryModeService) {
    this.deliveryModeService = deliveryModeService;
  }

}
