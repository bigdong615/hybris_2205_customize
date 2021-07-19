package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.voucher.converters.populator.OrderAppliedVouchersPopulator;
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
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This populator created to populate coupon codes and coupon amount
 * @author Manikandan
 */
public class BlOrderAppliedVouchersPopulator extends OrderAppliedVouchersPopulator {

  private static final Logger LOG = Logger.getLogger(BlOrderAppliedVouchersPopulator.class);

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
        setCouponDiscountPrice(amountMap , promotionResultModel , vouchers);
      }
    }
    target.setPromotionAmountMap(amountMap);
    target.setAppliedVouchers((List<String>) vouchers);
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
    Collection<AbstractPromotionActionModel> actions = promotionResultModel.getActions();
    if(CollectionUtils.isNotEmpty(actions)){
      for(AbstractPromotionActionModel action : actions){
        RuleBasedOrderEntryAdjustActionModel ruleBasedOrderEntryAdjustActionModel = (RuleBasedOrderEntryAdjustActionModel) action;
        BlLogger.logMessage(LOG, Level.INFO,"Entry Model product : "+ ruleBasedOrderEntryAdjustActionModel.getOrderEntryProduct());
        if(null != ruleBasedOrderEntryAdjustActionModel && CollectionUtils.isNotEmpty(ruleBasedOrderEntryAdjustActionModel.getUsedCouponCodes())) {
          for (final String couponCode : ruleBasedOrderEntryAdjustActionModel.getUsedCouponCodes()) {
            BlLogger.logMessage(LOG, Level.INFO,"Coupon Code : "+ couponCode);
            BigDecimal amount = ruleBasedOrderEntryAdjustActionModel.getAmount();
            if (null != amount && (Double.compare(amount.doubleValue(), 0.0) > 0)) {
              amountMap.put(couponCode,
                  amount.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
              if(MapUtils.isNotEmpty(amountMap) && amountMap.containsKey(couponCode)){
                amount = amountMap.get(couponCode).add(amount);
              }
              BlLogger.logMessage(LOG, Level.INFO,"Coupon Code Amount : "+ amount.doubleValue());
              vouchers.add(couponCode);
            }
          }
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
        final BigDecimal amount = ruleBasedOrderAdjustTotalActionModel.getAmount();
        if (null != amount && (Double.compare(amount.doubleValue(), 0.0) > 0)) {
          amountMap.put(couponCode, amount.setScale(
              BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
          vouchers.add(couponCode);
        }
      }
    }

  }
}
