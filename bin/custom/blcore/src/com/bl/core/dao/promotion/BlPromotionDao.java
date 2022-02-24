package com.bl.core.dao.promotion;

import de.hybris.platform.promotionengineservices.dao.PromotionDao;
import de.hybris.platform.promotionengineservices.model.PromotionSourceRuleModel;


/**
 * @author Ravikumar
 *
 */
public interface BlPromotionDao extends PromotionDao
{

	/**
	 * Gets the promotion by code and coupon condition.
	 *
	 * @param promoGroup
	 *           the promo group
	 * @param ruleStatus
	 *           the rule status
	 * @param code
	 *           the code
	 * @return the promotion by code and coupon condition
	 */
	PromotionSourceRuleModel getPromotionByCodeAndCouponCondition(final String promoGroup, final String ruleStatus,
			final String code);

}
