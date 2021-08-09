package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.promotions.promotionengineservices.service.BlPromotionService;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.voucher.converters.populator.OrderAppliedVouchersPopulator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderAdjustTotalActionModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderEntryAdjustActionModel;
import de.hybris.platform.promotions.model.AbstractPromotionActionModel;
import de.hybris.platform.promotions.model.PromotionResultModel;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

/**
 * This populator created to populate coupon codes and coupon amount
 * @author Manikandan
 */
public class BlOrderAppliedVouchersPopulator extends OrderAppliedVouchersPopulator {

  private BlPromotionService blPromotionService;

  /**
   * This method created to populate coupon codes and coupon amount based on promotional results
   */
  @Override
  public void populate(final AbstractOrderModel source, final AbstractOrderData target)
  {
    final Map<String, BigDecimal> amountMap = new HashMap<>();
    final Set<String> vouchers = new HashSet<>();
    if(CollectionUtils.isNotEmpty(source.getAllPromotionResults())) {
      for (final PromotionResultModel promotionResultModel : source.getAllPromotionResults()) {
        setCouponDiscountPrice(source,amountMap , promotionResultModel , vouchers);
      }
    }
    target.setPromotionAmountMap(amountMap);
    target.setAppliedVouchers((vouchers.stream().collect(Collectors.toList())));
  }

  /**
   * This method created to decide which promotional results needs to call based on condition
   */

  private void setCouponDiscountPrice(final AbstractOrderModel source, final Map<String, BigDecimal> amountMap, final PromotionResultModel promotionResultModel, final Collection<String> vouchers) {
    for (final AbstractPromotionActionModel abstractPromotionActionModel : promotionResultModel
        .getActions()) {
      if (abstractPromotionActionModel instanceof RuleBasedOrderEntryAdjustActionModel) {
        getOrderEntryDiscountPrice(source,amountMap, promotionResultModel ,vouchers);
      } else if (abstractPromotionActionModel instanceof RuleBasedOrderAdjustTotalActionModel) {
        getOrderDiscountPrice(amountMap, promotionResultModel ,vouchers);
      }
    }
  }

  /**
   * this method populate data for ruleBasedOrderEntryAdjustActionModel coupon and amount
   */
  private void getOrderEntryDiscountPrice(final AbstractOrderModel source,final Map<String, BigDecimal> amountMap,final PromotionResultModel promotionResultModel, final Collection<String> vouchers) {
    final Collection<AbstractPromotionActionModel> actions = promotionResultModel.getActions();
    final List<AbstractOrderEntryModel> entries= source.getEntries();
    final RuleBasedOrderEntryAdjustActionModel promotionActionModel = (RuleBasedOrderEntryAdjustActionModel) getPromotionActionWithCoupon(promotionResultModel);
    final String actionCouponCode = getCouponCode(promotionActionModel);
    if(CollectionUtils.isNotEmpty(actions) && StringUtils.isNotBlank(actionCouponCode) && CollectionUtils.isNotEmpty(entries)){
      getBlPromotionService().addCouponCodeToVouchers(actions, actionCouponCode, entries,amountMap,vouchers);
    }
  }

  /**
   * Get the coupon code for the promotion model
   * @param promotionActionModel
   * @return
   */
  private String getCouponCode(final RuleBasedOrderEntryAdjustActionModel promotionActionModel) {
    return  promotionActionModel != null && CollectionUtils.isNotEmpty(promotionActionModel.getUsedCouponCodes()) ? promotionActionModel.getUsedCouponCodes().iterator().next() : StringUtils.EMPTY;
  }


  /**
   * Check if the promotion action from promotion results has a associated coupon code
   * @param promotionResultModel
   * @return
   */
  private AbstractPromotionActionModel getPromotionActionWithCoupon(final PromotionResultModel promotionResultModel) {
    final Optional<AbstractPromotionActionModel> promotionAction = promotionResultModel.getActions().stream()
        .filter(action -> action instanceof RuleBasedOrderEntryAdjustActionModel
            && null != ((RuleBasedOrderEntryAdjustActionModel) action).getUsedCouponCodes() && !((RuleBasedOrderEntryAdjustActionModel) action).getUsedCouponCodes().isEmpty())
        .findAny();
    return promotionAction.orElse(null);
  }



  /**
   * this method populate data for ruleBasedOrderAdjustTotalActionModel coupon and amount
   */

  private void getOrderDiscountPrice(final Map<String, BigDecimal> amountMap,
      final PromotionResultModel promotionResultModel ,final Collection<String> vouchers) {
    final RuleBasedOrderAdjustTotalActionModel ruleBasedOrderAdjustTotalActionModel = (RuleBasedOrderAdjustTotalActionModel) promotionResultModel
        .getActions().iterator().next();
    if(null != ruleBasedOrderAdjustTotalActionModel && CollectionUtils.isNotEmpty(ruleBasedOrderAdjustTotalActionModel.getUsedCouponCodes())) {
      for (final String couponCode : ruleBasedOrderAdjustTotalActionModel.getUsedCouponCodes()) {
        final BigDecimal amount = ruleBasedOrderAdjustTotalActionModel.getAmount();
        if (null != amount && (Double.compare(amount.doubleValue(), 0.0) > 0)) {
          amountMap.put(couponCode, amount.setScale(
              BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
          vouchers.add(couponCode);
        }
      }
    }

  }

  public BlPromotionService getBlPromotionService() {
    return blPromotionService;
  }

  public void setBlPromotionService(
      BlPromotionService blPromotionService) {
    this.blPromotionService = blPromotionService;
  }
}
