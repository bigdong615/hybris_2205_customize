package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
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
        setCouponDiscountPrice(source,amountMap , promotionResultModel , vouchers);
      }
    }
    target.setPromotionAmountMap(amountMap);
    target.setAppliedVouchers((vouchers.stream().collect(Collectors.toList())));
  }

  /**
   * This method created to decide which promotional results needs to call based on condition
   */

  private void setCouponDiscountPrice(AbstractOrderModel source,
      final Map<String, BigDecimal> amountMap,
      final PromotionResultModel promotionResultModel, final Collection<String> vouchers) {
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
    Collection<AbstractPromotionActionModel> actions = promotionResultModel.getActions();
    List<AbstractOrderEntryModel> entries= source.getEntries();
    RuleBasedOrderEntryAdjustActionModel promotionActionModel = (RuleBasedOrderEntryAdjustActionModel) getPromotionActionWithCoupon(promotionResultModel);
    String actionCouponCode = getCouponCode(promotionActionModel);
    if(CollectionUtils.isNotEmpty(actions) && StringUtils.isNotBlank(actionCouponCode) && CollectionUtils.isNotEmpty(entries)){
      addCouponCodeToVouchers(actions, actionCouponCode, entries,amountMap,vouchers);
    }
  }

  /**
   * Add the applied Coupon codes to Voucher list
   * @param actions
   * @param actionCouponCode
   * @param entries
   * @param amountMap
   * @param vouchers
   */
  private void addCouponCodeToVouchers(final Collection<AbstractPromotionActionModel> actions,final String actionCouponCode, final List<AbstractOrderEntryModel> entries,Map<String, BigDecimal> amountMap, Collection<String> vouchers) {
    BigDecimal totalCouponDiscount = BigDecimal.ZERO;
    for(AbstractPromotionActionModel action : actions){
      RuleBasedOrderEntryAdjustActionModel ruleBasedOrderEntryAdjustActionModel = (RuleBasedOrderEntryAdjustActionModel) action;
      updateActionWithoutCouponCode(ruleBasedOrderEntryAdjustActionModel,actionCouponCode);
      for (final String couponCode : ruleBasedOrderEntryAdjustActionModel.getUsedCouponCodes()) {
        BigDecimal amount = ruleBasedOrderEntryAdjustActionModel.getAmount();
        if (null != amount && (Double.compare(amount.doubleValue(), 0.0) > 0)) {
          amount = getUpdatedCartAmount(amount,entries,ruleBasedOrderEntryAdjustActionModel);
          totalCouponDiscount = totalCouponDiscount.add(amount);
          amountMap.put(couponCode,
              totalCouponDiscount.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
          BlLogger.logMessage(LOG, Level.INFO,"Coupon Code Amount for : "+ruleBasedOrderEntryAdjustActionModel.getOrderEntryProduct().getCode()+"is :"+ amount.doubleValue());
          vouchers.add(couponCode);
        }
      }
    }
  }

  /**
   * Update the cart amount based on the quantity
   *
   */
  private BigDecimal getUpdatedCartAmount(BigDecimal amount, final List<AbstractOrderEntryModel> entries, final RuleBasedOrderEntryAdjustActionModel ruleBasedOrderEntryAdjustActionModel) {
    Optional<AbstractOrderEntryModel> entryModel = entries.stream().filter(entry -> entry.getEntryNumber().equals(ruleBasedOrderEntryAdjustActionModel.getOrderEntryNumber())).findAny();
    final long cartEntryQty = entryModel.isPresent() ? entryModel.get().getQuantity() : 0;
    return  cartEntryQty > 1 ? amount.multiply(new BigDecimal(cartEntryQty)) : amount;
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
    Optional<AbstractPromotionActionModel> promotionAction = promotionResultModel.getActions().stream()
        .filter(action -> action instanceof RuleBasedOrderEntryAdjustActionModel
            && !((RuleBasedOrderEntryAdjustActionModel) action).getUsedCouponCodes().isEmpty())
        .findAny();
    return promotionAction.orElse(null);
  }

  /**
   * Update the action which do not have the coupon code associated for
   * discount to apply for coupon promotion
   * @param ruleBasedOrderEntryAdjustActionModel
   * @param actionCouponCode
   */
  private void updateActionWithoutCouponCode(final RuleBasedOrderEntryAdjustActionModel ruleBasedOrderEntryAdjustActionModel,final String actionCouponCode) {
      if(CollectionUtils.isEmpty(ruleBasedOrderEntryAdjustActionModel.getUsedCouponCodes())){
        ruleBasedOrderEntryAdjustActionModel.setUsedCouponCodes(Collections.singleton(actionCouponCode));
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
