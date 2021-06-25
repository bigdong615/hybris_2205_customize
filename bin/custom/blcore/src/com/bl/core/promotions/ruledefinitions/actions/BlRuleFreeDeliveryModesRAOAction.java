package com.bl.core.promotions.ruledefinitions.actions;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.calculation.impl.DefaultBlRuleEngineCalculationService;
import com.bl.logging.BlLogger;
import de.hybris.platform.order.DeliveryModeService;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rao.DeliveryModeRAO;
import de.hybris.platform.ruleengineservices.rao.RuleEngineResultRAO;
import de.hybris.platform.ruleengineservices.rao.ShipmentRAO;
import de.hybris.platform.ruleengineservices.rule.evaluation.RuleActionContext;
import de.hybris.platform.ruleengineservices.rule.evaluation.actions.AbstractRuleExecutableSupport;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This action is created to allow certin modes as free delivery modes
 * @author Ritika
 */
public class BlRuleFreeDeliveryModesRAOAction extends AbstractRuleExecutableSupport {
  private static final Logger LOG = Logger.getLogger(BlRuleFreeDeliveryModesRAOAction.class);
  private DefaultBlRuleEngineCalculationService defaultBlRuleEngineCalculationService;
  private DeliveryModeService deliveryModeService;


  @Override
  public boolean performActionInternal(final RuleActionContext context) {
    List<String> deliveryModeCodes = (List<String>)(context.getParameter(BlCoreConstants.FREE_DELIVERY_MODES));
    BlLogger.logMessage(LOG, Level.INFO, "Delivery modes" + deliveryModeCodes.get(0));
    return this.performAction(context, deliveryModeCodes );
  }


  /**
   * Perform Action to make delivery cost zero
   * for applicable delivery modes
   * @param context
   * @param freeDeliveryModes
   * @return
   */
  protected boolean performAction(final RuleActionContext context,final List<String> freeDeliveryModes) {
    final CartRAO cartRao = context.getCartRao();
    RuleEngineResultRAO result = context.getRuleEngineResultRao();
    if(CollectionUtils.isNotEmpty(freeDeliveryModes)){
      BlLogger.logMessage(LOG, Level.DEBUG, "Size of Free DeliveryModes List : "+ freeDeliveryModes.size());
      for (String mode : freeDeliveryModes) {
        Optional<DeliveryModeRAO> deliveryMode = this.getRaoLookupService()
            .lookupRAOByType(DeliveryModeRAO.class, context,
                this.getDeliveryModeRAOFilter(mode));
        if (deliveryMode.isPresent()) {
          this.changeDeliveryCost(cartRao, deliveryMode.get(), result, context);
        }
      }
        return true;
    }
    return false;
  }

  /**
   * Change delivery cost
   * @param cartRao
   * @param mode
   * @param result
   * @param context
   */
  public void changeDeliveryCost(final CartRAO cartRao, final DeliveryModeRAO mode,final RuleEngineResultRAO result, RuleActionContext context) {
    this.validateRule(context);
    ShipmentRAO shipment = getDefaultBlRuleEngineCalculationService().changeDeliveryCost(cartRao ,mode);
    result.getActions().add(shipment);
    this.setRAOMetaData(context, shipment);
    context.scheduleForUpdate(cartRao, result);
    context.insertFacts(shipment);
    cartRao.getEntries().forEach(entryRAO -> this.getConsumptionSupport().consumeOrderEntry(entryRAO, shipment));
  }

  /**
   * Get Delivery ModeRAO by code
   * @param deliveryModeCode
   * @return
   */
  protected Predicate<DeliveryModeRAO> getDeliveryModeRAOFilter(String deliveryModeCode) {
    return deliveryModeRAO -> this.isFactDeliveryAndHasCode(deliveryModeRAO, deliveryModeCode);

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

  public DeliveryModeService getDeliveryModeService() {
    return deliveryModeService;
  }

  public void setDeliveryModeService(DeliveryModeService deliveryModeService) {
    this.deliveryModeService = deliveryModeService;
  }

}
