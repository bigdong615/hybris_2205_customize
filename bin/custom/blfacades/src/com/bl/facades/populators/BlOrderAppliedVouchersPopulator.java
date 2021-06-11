package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.voucher.converters.populator.OrderAppliedVouchersPopulator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderAdjustTotalActionModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderEntryAdjustActionModel;
import de.hybris.platform.promotions.model.AbstractPromotionActionModel;
import de.hybris.platform.promotions.model.PromotionResultModel;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections.CollectionUtils;

/**
 * This populator created to populate coupon codes and coupon amount
 * @author Manikandan
 */
public class BlOrderAppliedVouchersPopulator extends OrderAppliedVouchersPopulator {

  /**
   * This method created to populate coupon codes and coupon amount based on promotional results
   */
  @Override
  public void populate(final AbstractOrderModel source, final AbstractOrderData target)
  {
    final Map<String, BigDecimal> amountMap = new HashMap<>();
    final List<String> vouchers = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(source.getAllPromotionResults())) {
      for (final PromotionResultModel promotionResultModel : source.getAllPromotionResults()) {
        setCouponDiscountPrice(amountMap , promotionResultModel , vouchers);
      }
    }
    target.setPromotionAmountMap(amountMap);
    target.setAppliedVouchers(vouchers);
  }

  /**
   * This method created to decide which promotional results needs to call based on condition
   */

  private void setCouponDiscountPrice(final Map<String, BigDecimal> amountMap,
      final PromotionResultModel promotionResultModel ,final Collection<String> vouchers) {
    for (final AbstractPromotionActionModel abstractPromotionActionModel : promotionResultModel
        .getActions()) {
      if (abstractPromotionActionModel instanceof RuleBasedOrderEntryAdjustActionModel) {
        getOrderEntryDiscountPrice(amountMap, promotionResultModel ,vouchers);
      } else if (abstractPromotionActionModel instanceof RuleBasedOrderAdjustTotalActionModel) {
        getOrderDiscountPrice(amountMap, promotionResultModel ,vouchers);
      }
    }
  }

  /**
   * this method populate data for ruleBasedOrderEntryAdjustActionModel coupon and amount
   */
  private void getOrderEntryDiscountPrice(final Map<String, BigDecimal> amountMap,
      final PromotionResultModel promotionResultModel ,final Collection<String> vouchers) {
    final RuleBasedOrderEntryAdjustActionModel ruleBasedOrderEntryAdjustActionModel = (RuleBasedOrderEntryAdjustActionModel) promotionResultModel
        .getActions().iterator().next();
    if(null != ruleBasedOrderEntryAdjustActionModel && CollectionUtils.isNotEmpty(ruleBasedOrderEntryAdjustActionModel.getUsedCouponCodes())) {
      for (final String couponCode : ruleBasedOrderEntryAdjustActionModel.getUsedCouponCodes()) {
        if (null != ruleBasedOrderEntryAdjustActionModel.getAmount() && (
            Double.compare(ruleBasedOrderEntryAdjustActionModel.getAmount().doubleValue(), 0.0)
                > 0)) {
          amountMap.put(couponCode, ruleBasedOrderEntryAdjustActionModel.getAmount()
              .setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
          vouchers.add(couponCode);
        }
      }
    }
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
        if (null != ruleBasedOrderAdjustTotalActionModel.getAmount() && (
            Double.compare(ruleBasedOrderAdjustTotalActionModel.getAmount().doubleValue(), 0.0)
                > 0)) {
          amountMap.put(couponCode, ruleBasedOrderAdjustTotalActionModel.getAmount().setScale(
              BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
          vouchers.add(couponCode);
        }
      }
    }

  }
}
