package com.bl.core.promotions.promotionengineservices.service.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.promotions.promotionengineservices.service.BlPromotionService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.promotionengineservices.dao.PromotionDao;
import de.hybris.platform.promotionengineservices.model.PromotionSourceRuleModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderEntryAdjustActionModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedPromotionModel;
import de.hybris.platform.promotions.PromotionsService;
import de.hybris.platform.promotions.model.AbstractPromotionActionModel;
import de.hybris.platform.promotions.model.PromotionGroupModel;
import de.hybris.platform.promotions.result.PromotionOrderResults;
import de.hybris.platform.ruleengineservices.enums.RuleStatus;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is addded to check if usedgearCategory promotion is active
 *
 * @author Ritika
 */
public class DefaultBlPromotionService implements BlPromotionService {

  private static final Logger LOG = Logger.getLogger(DefaultBlPromotionService.class);
  private PromotionDao promotionDao;
  private BaseStoreService baseStoreService;
  private PromotionsService promotionsService;


  @Override
  public boolean isUsedGearCategoryPromotionActive() {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    if(baseStoreModel != null && StringUtils.isNotBlank(baseStoreModel.getUsedGearPromotionCode())) {
      final RuleBasedPromotionModel promotionModel = (RuleBasedPromotionModel) getPromotionDao().findPromotionByCode(baseStoreModel.getUsedGearPromotionCode());
      if(promotionModel  != null && promotionModel.getPromotionGroup() != null && CollectionUtils.isNotEmpty(promotionModel.getPromotionGroup().getPromotionSourceRules())) {
        Collection<PromotionSourceRuleModel> promotionSourceRules = promotionModel.getPromotionGroup().getPromotionSourceRules();
        final Optional<PromotionSourceRuleModel> sourceRuleModel = promotionSourceRules.stream().filter(rule ->
            StringUtils.equalsIgnoreCase(rule.getCode(), baseStoreModel.getUsedGearPromotionCode())
                && RuleStatus.PUBLISHED.equals(rule.getStatus()) && validatePromotionDates(
                rule.getStartDate(), rule.getEndDate())).findAny();
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "The Used Gear Category Promotion is Active on Site {}", sourceRuleModel.isPresent());
        return  sourceRuleModel.isPresent();
      }
    }

    return false;
  }

  /**
   * Add the applied Coupon codes to Voucher list
   * @param actions
   * @param actionCouponCode
   * @param entries
   * @param amountMap
   * @param vouchers
   */
  public void addCouponCodeToVouchers(final Collection<AbstractPromotionActionModel> actions,final String actionCouponCode, final List<AbstractOrderEntryModel> entries,final Map<String, BigDecimal> amountMap,final Collection<String> vouchers) {
    BigDecimal totalCouponDiscount = BigDecimal.ZERO;
    for(AbstractPromotionActionModel action : actions){
      final RuleBasedOrderEntryAdjustActionModel ruleBasedOrderEntryAdjustActionModel = (RuleBasedOrderEntryAdjustActionModel) action;
      updateActionWithoutCouponCode(ruleBasedOrderEntryAdjustActionModel,actionCouponCode);
      for (final String couponCode : ruleBasedOrderEntryAdjustActionModel.getUsedCouponCodes()) {
        BigDecimal amount = ruleBasedOrderEntryAdjustActionModel.getAmount();
        if (null != amount && (Double.compare(amount.doubleValue(), 0.0) > 0)) {
          amount = getUpdatedCartAmount(amount,entries,ruleBasedOrderEntryAdjustActionModel);
          totalCouponDiscount = totalCouponDiscount.add(amount);
          amountMap.put(couponCode,totalCouponDiscount.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
          BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Coupon Code Amount for {} is : {}", ruleBasedOrderEntryAdjustActionModel.getOrderEntryProduct().getCode(),amount.doubleValue());
          vouchers.add(couponCode);
        }
      }
    }
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
   *
   * Update the cart amount based on the quantity
   * @param amount
   * @param entries
   * @param ruleBasedOrderEntryAdjustActionModel
   * @return
   */
  private BigDecimal getUpdatedCartAmount(BigDecimal amount, final List<AbstractOrderEntryModel> entries, final RuleBasedOrderEntryAdjustActionModel ruleBasedOrderEntryAdjustActionModel) {
    Optional<AbstractOrderEntryModel> entryModel = entries.stream().filter(entry -> entry.getEntryNumber().equals(ruleBasedOrderEntryAdjustActionModel.getOrderEntryNumber())).findAny();
    final long cartEntryQty = entryModel.isPresent() ? entryModel.get().getQuantity() : 0;
    return  cartEntryQty > 1 ? amount.multiply(new BigDecimal(cartEntryQty)) : amount;
  }

  /**
   * Validate if current date is eligible to apply promotion
   * @param startDate
   * @param endDate
   * @return
   */
  private boolean validatePromotionDates(final Date startDate, final Date endDate) {
    final Date currentDate = new Date();
    return startDate != null && endDate != null && currentDate.getTime() >= startDate.getTime() &&
        currentDate.getTime() <= endDate.getTime();
  }

  /**
   * is extended coupon code promotion applied on cart
   * @param cartModel
   * @return
   */
  public boolean isFreeDayCouponPromoApplied(final CartModel cartModel){
    final PromotionGroupModel blPromoGroup = getPromotionDao().findPromotionGroupByCode(BlCoreConstants.BL_PROMO_GROUP);
    final PromotionOrderResults promotionResults = getPromotionsService().getPromotionResults(cartModel);
    if (blPromoGroup != null && CollectionUtils.isNotEmpty(blPromoGroup.getPromotionSourceRules())) {
      final Date currentDate = new Date();
      final Optional<PromotionSourceRuleModel> couponExtendedRentalDayPromotion = blPromoGroup.getPromotionSourceRules().stream().filter(promotionSourceRuleModel -> promotionSourceRuleModel.getCode().contains(BlCoreConstants.BL_EXTENDED_RENTAL_DAYS_PROMOCODE) && RuleStatus.PUBLISHED.equals(promotionSourceRuleModel.getStatus()) && promotionSourceRuleModel.getConditions().contains(BlCoreConstants.QUALIFYING_COUPONS)).findAny();
      final boolean isCouponPromotionActiveInBackend = couponExtendedRentalDayPromotion.isPresent()  && couponExtendedRentalDayPromotion.get().getStartDate() != null
          && couponExtendedRentalDayPromotion.get().getEndDate() != null && currentDate.getTime() >= couponExtendedRentalDayPromotion.get().getStartDate().getTime() && currentDate.getTime() <= couponExtendedRentalDayPromotion.get().getEndDate().getTime();
      final boolean orderResult = Objects.nonNull(promotionResults) ? promotionResults.getFiredOrderPromotions().stream().anyMatch( promotionResult -> promotionResult.getPromotion().getCode().equals(couponExtendedRentalDayPromotion.get().getCode())) : Boolean.FALSE;
      return  isCouponPromotionActiveInBackend && orderResult;
    }
    return false;
  }

  public PromotionDao getPromotionDao() {
    return promotionDao;
  }

  public void setPromotionDao(PromotionDao promotionDao) {
    this.promotionDao = promotionDao;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }

  public PromotionsService getPromotionsService() {
    return promotionsService;
  }

  public void setPromotionsService(PromotionsService promotionsService) {
    this.promotionsService = promotionsService;
  }
}
