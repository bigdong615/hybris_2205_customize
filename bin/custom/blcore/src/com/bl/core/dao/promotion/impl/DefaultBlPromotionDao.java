package com.bl.core.dao.promotion.impl;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.jalo.flexiblesearch.FlexibleSearchException;
import de.hybris.platform.promotionengineservices.dao.impl.DefaultPromotionDao;
import de.hybris.platform.promotionengineservices.model.PromotionSourceRuleModel;
import de.hybris.platform.promotions.model.PromotionGroupModel;
import de.hybris.platform.ruleengineservices.enums.RuleStatus;
import de.hybris.platform.ruleengineservices.model.AbstractRuleModel;
import de.hybris.platform.ruleengineservices.model.SourceRuleModel;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.promotion.BlPromotionDao;
import com.bl.logging.BlLogger;


/**
 * This Dao class contains the logic to get promotion related queries.
 *
 * @author Ravikumar
 *
 */
public class DefaultBlPromotionDao extends DefaultPromotionDao implements BlPromotionDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlPromotionDao.class);
	private static final String SELECT = "SELECT ";
	private static final String FROM = " from ";
	private static final String WHERE = "where";
	private static final String AND = "AND";
	private static final String PR = " {pr.";

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
	@Override
	public PromotionSourceRuleModel getPromotionByCodeAndCouponCondition(final String promoGroup, final String ruleStatus,
			final String code)
	{
		final String[] parts = code.split("-");

		final String codeString = "\"" + parts[0] + "\"";
		final String get_promotion_by_code_and_condition = SELECT + "{pr." + ItemModel.PK + "}" + FROM + "{"
				+ PromotionSourceRuleModel._TYPECODE + " as pr} " + WHERE + PR + PromotionSourceRuleModel.WEBSITE + "} = ({{" + SELECT
				+ "{pg." + ItemModel.PK + "}" + FROM + "{" + PromotionGroupModel._TYPECODE + " as pg} " + WHERE + " {pg."
				+ PromotionGroupModel.IDENTIFIER + "} = ?promoGroup}}) " + AND + PR + AbstractRuleModel.STATUS + "} = ({{" + SELECT
				+ "{rs." + ItemModel.PK + "}" + FROM + "{" + RuleStatus._TYPECODE + " as rs} " + WHERE
				+ " {rs.code} = ?ruleStatus}}) " + AND + PR + SourceRuleModel.CONDITIONS + "} like '%y_qualifying_coupons%' " + AND
				+ PR + SourceRuleModel.CONDITIONS + "} like '%" + "\"List(ItemType(AbstractCoupon))\"" + "%'" + AND + PR
				+ SourceRuleModel.CONDITIONS + "} like '%" + codeString + "%'";
		final Map<String, String> params = new HashMap<>();
		params.put(BlCoreConstants.PROMO_GROUP, promoGroup);
		params.put(BlCoreConstants.RULE_STATUS, ruleStatus);
		try
		{
			final SearchResult<PromotionSourceRuleModel> searchResult = getFlexibleSearchService()
					.search(get_promotion_by_code_and_condition, params);
			if (Objects.nonNull(searchResult) && CollectionUtils.isNotEmpty(searchResult.getResult()))
			{
				return searchResult.getResult().iterator().next();
			}
		}
		catch (final FlexibleSearchException exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while fetching Promotion Resilt for given code : {}", code);
		}

		return null;
	}

}
