package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.voucher.converters.populator.OrderAppliedVouchersPopulator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderAdjustTotalActionModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderEntryAdjustActionModel;
import de.hybris.platform.promotions.model.AbstractPromotionActionModel;
import de.hybris.platform.promotions.model.PromotionResultModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;

public class BlOrderAppliedVouchersPopulator extends OrderAppliedVouchersPopulator {

  @Override
  public void populate(final AbstractOrderModel source, final AbstractOrderData target) throws ConversionException
  {
    final Map<String, BigDecimal> amountMap = new HashMap<>();
    List<String> vouchers = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(source.getAllPromotionResults())) {
      for (final PromotionResultModel promotionResultModel : source.getAllPromotionResults()) {
        setCouponDiscountPrice(amountMap , promotionResultModel , vouchers);
      }
    }
    target.setPromotionAmountMap(amountMap);
    target.setAppliedVouchers(vouchers);
  }


  private void setCouponDiscountPrice(final Map<String, BigDecimal> amountMap,
      final PromotionResultModel promotionResultModel ,final Collection<String> vouchers) {
    for (AbstractPromotionActionModel abstractPromotionActionModel : promotionResultModel
        .getActions()) {
      if (abstractPromotionActionModel instanceof RuleBasedOrderEntryAdjustActionModel) {
        getOrderEntryDiscountPrice(amountMap, promotionResultModel ,vouchers);
      } else if (abstractPromotionActionModel instanceof RuleBasedOrderAdjustTotalActionModel) {
        getOrderDiscountPrice(amountMap, promotionResultModel ,vouchers);
      }
    }
  }

  private void getOrderEntryDiscountPrice(final Map<String, BigDecimal> amountMap,
      final PromotionResultModel promotionResultModel ,final Collection<String> vouchers) {
    RuleBasedOrderEntryAdjustActionModel ruleBasedOrderEntryAdjustActionModel = (RuleBasedOrderEntryAdjustActionModel) promotionResultModel
        .getActions().iterator().next();
    for (final String couponCode : ruleBasedOrderEntryAdjustActionModel.getUsedCouponCodes()) {
      if(0.0 < ruleBasedOrderEntryAdjustActionModel.getAmount().doubleValue()) {
        amountMap.put(couponCode, ruleBasedOrderEntryAdjustActionModel.getAmount()
            .setScale(BlCoreConstants.DECIMAL_PRECISION, RoundingMode.HALF_DOWN));
        vouchers.add(couponCode);
      }
    }
  }

  private void getOrderDiscountPrice(final Map<String, BigDecimal> amountMap,
      final PromotionResultModel promotionResultModel  ,final Collection<String> vouchers) {
    final RuleBasedOrderAdjustTotalActionModel ruleBasedOrderAdjustTotalActionModel = (RuleBasedOrderAdjustTotalActionModel) promotionResultModel
        .getActions().iterator().next();
    for (final String couponCode : ruleBasedOrderAdjustTotalActionModel.getUsedCouponCodes()) {
      if(0.0 < ruleBasedOrderAdjustTotalActionModel.getAmount().doubleValue()) {
        amountMap.put(couponCode, ruleBasedOrderAdjustTotalActionModel.getAmount().setScale(
            BlCoreConstants.DECIMAL_PRECISION, RoundingMode.HALF_DOWN));
        vouchers.add(couponCode);
      }
    }

  }
}
