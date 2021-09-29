package com.bl.core.promotions.promotionengineservices.service;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.promotions.model.AbstractPromotionActionModel;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Promotion Service
 * @author Ritika
 */
public interface BlPromotionService {

  /**
   * This method will validate if used gear Category promotion is
   * active or not
   * @return
   */
  boolean isUsedGearCategoryPromotionActive();

  /**
   * Add the applied Coupon codes to Voucher list
   * @param actions
   * @param actionCouponCode
   * @param entries
   * @param amountMap
   * @param vouchers
   */
  void addCouponCodeToVouchers(final Collection<AbstractPromotionActionModel> actions,final String actionCouponCode, final List<AbstractOrderEntryModel> entries, Map<String, BigDecimal> amountMap, Collection<String> vouchers);

  /**
   * is extended coupon code promotion applied on cart
   * @param cartModel
   * @return
   */
  boolean isFreeDayCouponPromoApplied(final CartModel cartModel);

  }
