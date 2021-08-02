package com.bl.core.promotions.promotionengineservices.actions;

import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.promotionengineservices.action.impl.DefaultOrderAdjustTotalActionStrategy;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderAdjustTotalActionModel;
import de.hybris.platform.promotions.model.PromotionResultModel;
import de.hybris.platform.ruleengineservices.rao.AbstractRuleActionRAO;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rao.DiscountRAO;
import de.hybris.platform.ruleengineservices.rao.OrderEntryRAO;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class DefaultBlOrderAdjustTotalActionStrategy extends DefaultOrderAdjustTotalActionStrategy {

  private static final Logger LOG = Logger.getLogger(DefaultBlOrderAdjustTotalActionStrategy.class);

  public DefaultBlOrderAdjustTotalActionStrategy() {
  }

  @Override
  public List<PromotionResultModel> apply(AbstractRuleActionRAO action) {
    if (!(action instanceof DiscountRAO)) {
      BlLogger.logMessage(LOG,
          Level.ERROR, "cannot apply {}, action is not of type DiscountRAO",
          this.getClass().getSimpleName());
      return Collections.emptyList();
    } else {
      PromotionResultModel promoResult = this.getPromotionActionService()
          .createPromotionResult(action);
      if (promoResult == null) {
        BlLogger.logMessage(LOG,
            Level.ERROR, "cannot apply {}, promotionResult could not be created.",
            this.getClass().getSimpleName());
        return Collections.emptyList();
      } else {
        AbstractOrderModel order = this.getPromotionResultUtils().getOrder(promoResult);
        if (order == null) {
          BlLogger.logMessage(LOG,
              Level.ERROR, "cannot apply {}, order not found", this.getClass().getSimpleName());
          if (this.getModelService().isNew(promoResult)) {
            this.getModelService().detach(promoResult);
          }

          return Collections.emptyList();
        } else {
          DiscountRAO discountRao = (DiscountRAO) action;
          updateCartSubTotalAndEntries(discountRao,order);
          RuleBasedOrderAdjustTotalActionModel actionModel = this
              .createOrderAdjustTotalAction(promoResult, discountRao);
          this.handleActionMetadata(action, actionModel);
          this.getPromotionActionService()
              .createDiscountValue(discountRao, actionModel.getGuid(), order);
          this.getModelService().saveAll(new Object[]{promoResult, actionModel, order});
          this.recalculateIfNeeded(order);
          return Collections.singletonList(promoResult);
        }
      }
    }
  }

  private void updateCartSubTotalAndEntries(final DiscountRAO discountRao, final AbstractOrderModel order) {
    CartRAO cartRAO = (CartRAO) discountRao.getAppliedToObject();
    order.setSubtotal(cartRAO.getSubTotal().doubleValue());
   /* List<AbstractOrderEntryModel> updatedEntries = new ArrayList<>();
    Set<OrderEntryRAO> cartRAOEntries = cartRAO.getEntries();
    List<AbstractOrderEntryModel> orderEntryModels = order.getEntries();
    if(CollectionUtils.isNotEmpty(cartRAOEntries) && CollectionUtils.isNotEmpty(orderEntryModels)){
      for(AbstractOrderEntryModel cartEntryModel : orderEntryModels){
        for(OrderEntryRAO entryRao: cartRAOEntries) {
          if (cartEntryModel.getProduct().getCode().equals(entryRao.getProductCode())) {
            cartEntryModel.setBasePrice(entryRao.getBasePrice().doubleValue());
            updatedEntries.add(cartEntryModel);
          }
        }
      }
      order.setEntries(updatedEntries);
    }*/
  }
}

