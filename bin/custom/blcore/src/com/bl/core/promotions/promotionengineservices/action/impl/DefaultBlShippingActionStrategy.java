package com.bl.core.promotions.promotionengineservices.action.impl;

import com.bl.core.model.RuleBasedFreeDeliveryModeActionModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.order.daos.DeliveryModeDao;
import de.hybris.platform.promotionengineservices.action.impl.AbstractRuleActionStrategy;
import de.hybris.platform.promotions.model.PromotionResultModel;
import de.hybris.platform.ruleengineservices.rao.AbstractRuleActionRAO;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rao.ShipmentRAO;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;

/**
 * @author Ritika
 * This Strategy is created to apply free shipping cost
 * apply and undo action on the promotion
 */
public class DefaultBlShippingActionStrategy extends AbstractRuleActionStrategy<RuleBasedFreeDeliveryModeActionModel> {

  private static final Logger LOG = Logger.getLogger(DefaultBlShippingActionStrategy.class);
  private DeliveryModeDao deliveryModeDao;

  public DefaultBlShippingActionStrategy() {
    //nothing to do
  }

  /**
   *
   * Apply promotion action
   * @param action
   * @return
   */
  public List<PromotionResultModel> apply(final AbstractRuleActionRAO action) {
    if (!(action instanceof ShipmentRAO)) {
      BlLogger.logMessage(LOG, Level.ERROR,"cannot apply {}, action is not of type ShipmentRAO, but {}"+ action);
      return Collections.emptyList();
    } else {
      final ShipmentRAO changeDeliveryCostAction = (ShipmentRAO)action;
      if (!(changeDeliveryCostAction.getAppliedToObject() instanceof CartRAO)) {
        BlLogger.logMessage(LOG, Level.ERROR,"cannot apply {}, appliedToObject is not of type CartRAO, but {}"+ action.getAppliedToObject());
        return Collections.emptyList();
      } else {
        final PromotionResultModel promoResult = this.getPromotionActionService().createPromotionResult(action);
        if (promoResult == null) {
          BlLogger.logMessage(LOG, Level.ERROR,"cannot apply {}, promotionResult could not be created." + this.getClass().getSimpleName());
          return Collections.emptyList();
        } else {
          AbstractOrderModel order = this.getPromotionResultUtils().getOrder(promoResult);
          if (Objects.isNull(order)) {
            BlLogger.logMessage(LOG, Level.ERROR,"cannot apply {}, order or cart not found: {}"+ order);
            if (this.getModelService().isNew(promoResult)) {
              this.getModelService().detach(promoResult);
            }
            return Collections.emptyList();
          } else {
            ShipmentRAO shipmentRAO = (ShipmentRAO)action;
            DeliveryModeModel shipmentModel = this.getDeliveryModeForCode(shipmentRAO.getMode().getCode());
            final Double deliveryCostToReplace = order.getDeliveryCost();
            if(shipmentModel != null && order.getDeliveryMode() != null && order.getDeliveryMode().getCode().equals(shipmentModel.getCode())) {
              order.setDeliveryCost(shipmentRAO.getMode().getCost().doubleValue());
              RuleBasedFreeDeliveryModeActionModel actionModel = this.createPromotionAction(promoResult, action);
              this.handleActionMetadata(action, actionModel);
              actionModel.setDeliveryMode(shipmentModel);
              actionModel.setDeliveryCost(shipmentRAO.getMode().getCost());
              actionModel.setReplacedDeliveryCost(BigDecimal.valueOf(deliveryCostToReplace));
              this.getModelService().saveAll(promoResult, actionModel, order);
              return Collections.singletonList(promoResult);
            }
            else{
              BlLogger.logMessage(LOG, Level.ERROR,"Delivery Mode for code {} not found!"+ shipmentRAO.getMode());
              return Collections.emptyList();
            }
          }
        }
      }
    }
  }

  /**
   * Undo action on promotion when condition not satisfied
   * @param item
   */
  public void undo(final ItemModel item) {
    if (item instanceof RuleBasedFreeDeliveryModeActionModel) {
      this.handleUndoActionMetadata((RuleBasedFreeDeliveryModeActionModel)item);
      RuleBasedFreeDeliveryModeActionModel action = (RuleBasedFreeDeliveryModeActionModel)item;
      AbstractOrderModel order = this.getPromotionResultUtils().getOrder(action.getPromotionResult());
      order.setDeliveryCost(action.getReplacedDeliveryCost().doubleValue());
      this.undoInternal(action);
      this.getModelService().save(order);
    }

  }

  /**
   * Get Delivery mode by code
   * @param code
   * @return
   */
  protected DeliveryModeModel getDeliveryModeForCode(final String code) {
    ServicesUtil.validateParameterNotNull(code, "Parameter code cannot be null");
    List<DeliveryModeModel> deliveryModes = this.getDeliveryModeDao().findDeliveryModesByCode(code);
    return CollectionUtils.isNotEmpty(deliveryModes) ? deliveryModes.get(0) : null;
  }

  protected DeliveryModeDao getDeliveryModeDao() {
    return this.deliveryModeDao;
  }

  @Required//NOSONAR
  public void setDeliveryModeDao(DeliveryModeDao deliveryModeDao) {
    this.deliveryModeDao = deliveryModeDao;
  }
}
