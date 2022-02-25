package com.bl.storefront.promotion.validate.impl;

import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.promotionengineservices.model.PromotionSourceRuleModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedPromotionModel;
import de.hybris.platform.promotions.model.AbstractPromotionModel;
import de.hybris.platform.promotions.model.PromotionResultModel;
import de.hybris.platform.ruledefinitions.AmountOperator;
import de.hybris.platform.ruledefinitions.CollectionOperator;
import de.hybris.platform.ruleengine.model.AbstractRuleEngineRuleModel;
import de.hybris.platform.ruleengineservices.enums.RuleStatus;
import de.hybris.platform.ruleengineservices.model.AbstractRuleModel;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rule.data.RuleConditionData;
import de.hybris.platform.ruleengineservices.rule.data.RuleConditionDefinitionData;
import de.hybris.platform.ruleengineservices.rule.data.RuleParameterData;
import de.hybris.platform.ruleengineservices.rule.services.RuleConditionsRegistry;
import de.hybris.platform.ruleengineservices.rule.services.RuleConditionsService;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.context.MessageSource;
import org.springframework.stereotype.Component;
import org.springframework.ui.Model;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.coupon.BlCouponManagementService;
import com.bl.core.dao.promotion.BlPromotionDao;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlExtendOrderUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.bl.storefront.promotion.validate.BlPromotionValidator;
import com.google.common.collect.Lists;


/**
 *
 * Promotion Validator class to validated different conditions applied on Promotion
 *
 * @author Ravikumar
 *
 */

@Component("blPromotionValidator")
public class DefaultBlPromotionValidator implements BlPromotionValidator
{
	private static final Logger LOG = Logger.getLogger(DefaultBlPromotionValidator.class);

	private String voucherCode;
	private List<RuleConditionData> ruleConditions;
	private CartRAO cartRao;
	private String errorKey;
	private AbstractOrderModel extendedOrder;

	@Resource(name = "promotionDao")
	private BlPromotionDao promotionDao;

	@Resource(name = "cartService")
	private BlCartService blCartService;

	@Resource(name = "messageSource")
	private MessageSource messageSource;

	@Resource(name = "i18nService")
	private I18NService i18nService;

	@Resource(name = "userService")
	private UserService userService;

	@Resource(name = "ruleConditionsService")
	private RuleConditionsService ruleConditionsService;

	@Resource(name = "ruleConditionsRegistry")
	private RuleConditionsRegistry ruleConditionsRegistry;

	@Resource(name = "couponManagementService")
	private BlCouponManagementService blCouponManagementService;

	@Resource(name = "cartRaoConverter")
	private Converter<AbstractOrderModel, CartRAO> cartRaoConverter;

	@Resource(name = "productDao")
	private BlProductDao blProductDao;

	@Resource(name = "customerAccountService")
	private CustomerAccountService customerAccountService;

	@Resource(name = "baseStoreService")
	private BaseStoreService baseStoreService;

	@Override
	public boolean checkInvalidPromotionsForExtendOrder(final String voucherCode, final String errorKey, final Model model,
			final RedirectAttributes redirectAttributes, final String referer)
	{
		OrderModel orderModel = null;
		if (null != BlExtendOrderUtils.getCurrentExtendOrderToSession()
				&& BlExtendOrderUtils.getCurrentExtendOrderToSession().getCode().equalsIgnoreCase(getOrderCodeFromRequest(referer)))
		{
			orderModel = BlExtendOrderUtils.getCurrentExtendOrderToSession();
		}
		else
		{
			final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
			orderModel = getCustomerAccountService().getOrderForCode((CustomerModel) getUserService().getCurrentUser(),
					getOrderCodeFromRequest(referer), baseStoreModel);
		}
		setExtendedOrder(orderModel);
		setDefaults(true);
		return processValidation(voucherCode, errorKey, model, redirectAttributes);
	}

	/**
	 * Gets the order code from request.
	 *
	 * @param referer
	 *           the referer
	 * @return the order code from request
	 */
	private String getOrderCodeFromRequest(final String referer)
	{
		final String[] split = referer.split(BlFacadesConstants.URL_SEPERATOR);
		final int size = split.length - 1;
		return split[size];
	}

	@Override
	public boolean checkInvalidPromotions(final String voucherCode, final String errorKey, final Model model,
			final RedirectAttributes redirectAttributes)
	{
		setDefaults(false);
		return processValidation(voucherCode, errorKey, model, redirectAttributes);
	}

	/**
	 * Sets the defaults.
	 */
	private void setDefaults(final boolean isPromoValidationForExtendOrder)
	{
		setVoucherCode(StringUtils.EMPTY);
		setRuleConditions(Lists.newArrayList());
		setCartRao(null);
		setErrorKey(StringUtils.EMPTY);
		if (BooleanUtils.isFalse(isPromoValidationForExtendOrder))
		{
			setExtendedOrder(null);
		}
	}

	/**
	 * Process validation.
	 *
	 * @param voucherCode
	 *           the voucher code
	 * @param errorKey
	 *           the error key
	 * @param model
	 *           the model
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @return true, if successful
	 */
	private boolean processValidation(final String voucherCode, final String errorKey, final Model model,
			final RedirectAttributes redirectAttributes)
	{
		if (StringUtils.isNotBlank(voucherCode))
		{
			setVoucherCode(voucherCode);
			setErrorKey(errorKey);
			final PromotionSourceRuleModel promotionSourceRule = getPromotionDao()
					.getPromotionByCodeAndCouponCondition(BlCoreConstants.BL_PROMO_GROUP, RuleStatus.PUBLISHED.getCode(), voucherCode);
			return validateAllConditions(voucherCode, model, redirectAttributes, promotionSourceRule);
		}
		return false;
	}

	/**
	 * Validate all conditions.
	 *
	 * @param voucherCode
	 *           the voucher code
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean validateAllConditions(final String voucherCode, final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		final boolean isValidPromotion = isPromotionPresent(model, redirectAttributes, promotionSourceRule);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Promotion with code {} is Valid : {}", voucherCode, isValidPromotion);
		return isValidPromotion;
	}

	/**
	 * Checks if is promotion present.
	 *
	 * @param model
	 *           the model
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if is promotion present
	 */
	private boolean isPromotionPresent(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (Objects.isNull(promotionSourceRule))
		{
			addAndLogMessage("promotion.validation.message.not.found", null, model, redirectAttributes);
			return false;
		}
		convertRuleConditionsFromSourceRule(promotionSourceRule);
		if (Objects.isNull(getExtendedOrder()))
		{
			populateAndSetCartRao();
		}
		return isPromotionYetToStart(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Convert rule conditions from source rule.
	 *
	 * @param promotionSourceRule
	 *           the promotion source rule
	 */
	private void convertRuleConditionsFromSourceRule(final PromotionSourceRuleModel promotionSourceRule)
	{
		final Map<String, RuleConditionDefinitionData> conditionDefinitions = getRuleConditionsRegistry()
				.getConditionDefinitionsForRuleTypeAsMap(promotionSourceRule.getClass());
		setRuleConditions(
				getRuleConditionsService().convertConditionsFromString(promotionSourceRule.getConditions(), conditionDefinitions));
	}

	/**
	 * Populate and set cart rao.
	 */
	private void populateAndSetCartRao()
	{
		final CartModel sessionCart = getBlCartService().getSessionCart();
		if (Objects.nonNull(sessionCart))
		{
			final CartRAO populatedCartRAO = getCartRaoConverter().convert(sessionCart);
			setCartRao(Objects.nonNull(populatedCartRAO) ? populatedCartRAO : null);
		}
	}

	/**
	 * Checks if is promotion yet to start.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if is promotion yet to start
	 */
	private boolean isPromotionYetToStart(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (Objects.nonNull(promotionSourceRule.getStartDate()) && promotionSourceRule.getStartDate().compareTo(new Date()) > 0)
		{
			addAndLogMessage("promotion.validation.message.not.started", new Object[]
			{ getFormattedStringDate(promotionSourceRule.getStartDate()) }, model, redirectAttributes);
			return false;
		}
		return isPromotionIsExpired(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Checks if is promotion is expired.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if is promotion is expired
	 */
	private boolean isPromotionIsExpired(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (Objects.nonNull(promotionSourceRule.getEndDate()) && promotionSourceRule.getEndDate().compareTo(new Date()) < 0)
		{
			addAndLogMessage("promotion.validation.message.expired", null, model, redirectAttributes);
			return false;
		}
		return checkForStackability(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Check for stackability.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean checkForStackability(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (Objects.nonNull(promotionSourceRule) && Objects.nonNull(promotionSourceRule.getRuleGroup())
				&& promotionSourceRule.getRuleGroup().isExclusive())
		{
			final AbstractOrderModel cartOrOrder = Objects.nonNull(getExtendedOrder()) ? getExtendedOrder()
					: getBlCartService().getSessionCart();
			if (Objects.nonNull(cartOrOrder) && CollectionUtils.isNotEmpty(cartOrOrder.getAllPromotionResults()))
			{
				final AbstractRuleModel nonStackableAppliedPromo = getNonStackableAppliedPromo(cartOrOrder.getAllPromotionResults());
				if (isPromoNotStackable(promotionSourceRule, nonStackableAppliedPromo))
				{
					addAndLogMessage("promotion.validation.message.stack.error", null, model, redirectAttributes);
					return false;
				}
			}
		}
		return checkIfPromotionIsFirstTimeUse(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Checks if is promo not stackable.
	 *
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @param nonStackableAppliedPromo
	 *           the non stackable applied promo
	 * @return true, if is promo not stackable
	 */
	private boolean isPromoNotStackable(final PromotionSourceRuleModel promotionSourceRule,
			final AbstractRuleModel nonStackableAppliedPromo)
	{
		return Objects.nonNull(nonStackableAppliedPromo) && Objects.nonNull(nonStackableAppliedPromo.getPriority())
				&& Objects.nonNull(promotionSourceRule.getPriority())
				&& nonStackableAppliedPromo.getPriority().compareTo(promotionSourceRule.getPriority()) >= 0;
	}

	/**
	 * Gets the non stackable applied promo on cart.
	 *
	 * @param allPromotionResults
	 *           the all promotion results
	 * @return the non stackable applied promo
	 */
	private AbstractRuleModel getNonStackableAppliedPromo(final Set<PromotionResultModel> allPromotionResults)
	{
		for (final PromotionResultModel promotionResultModel : allPromotionResults)
		{
			final AbstractPromotionModel promotion = promotionResultModel.getPromotion();
			if (promotion instanceof RuleBasedPromotionModel)
			{
				final AbstractRuleEngineRuleModel rule = ((RuleBasedPromotionModel) promotion).getRule();
				final AbstractRuleModel sourceRule = rule.getSourceRule();
				if (Objects.nonNull(sourceRule) && Objects.nonNull(sourceRule.getRuleGroup())
						&& sourceRule.getRuleGroup().isExclusive())
				{
					return sourceRule;
				}
			}
		}
		return null;
	}

	/**
	 * Check if promotion is first time use.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean checkIfPromotionIsFirstTimeUse(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (promotionSourceRule.getConditions().contains(BlControllerConstants.Y_FIRST_TIME_USER_CONDITION)
				&& checkRuleForPromotionCondition(promotionSourceRule, BlControllerConstants.CHECK_FIRST_TIME_USER)
				&& BooleanUtils.isFalse(isFirstTimeUser()))
		{
			addAndLogMessage("promotion.validation.message.first.time.user", null, model, redirectAttributes);
			return false;
		}
		return checkIfPromotionAlreadyUsed(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Checks if is first time user.
	 *
	 * @return true, if is first time user
	 */
	private boolean isFirstTimeUser()
	{
		if (Objects.nonNull(getCartRao()))
		{
			return getCartRao().getFirstTimeCustomer();
		}
		final UserModel currentUser = getUserService().getCurrentUser();
		return Objects.nonNull(currentUser) && BooleanUtils.isFalse(getUserService().isAnonymousUser(currentUser))
				&& CollectionUtils.isEmpty(currentUser.getOrders());
	}

	/**
	 * Check if promotion already used.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean checkIfPromotionAlreadyUsed(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		final UserModel currentUser = getUserService().getCurrentUser();
		if (Objects.nonNull(currentUser) && BooleanUtils.isFalse(getUserService().isAnonymousUser(currentUser))
				&& BooleanUtils.isFalse(getBlCouponManagementService().isCouponAvailableForUse(getVoucherCode(), currentUser)))
		{
			addAndLogMessage("promotion.validation.message.already.used", null, model, redirectAttributes);
			return false;
		}
		return checkPromotionForAuthenticatedUser(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Check if user is logged in.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean checkPromotionForAuthenticatedUser(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (promotionSourceRule.getConditions().contains(BlControllerConstants.Y_TARGET_CUSTOMERS) && evaluateAuthenticationCheck())
		{
			addAndLogMessage("promotion.validation.message.is.logged.in", null, model, redirectAttributes);
			return false;
		}
		return checkPromotionOnBasisOfCartType(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Evaluate authentication check.
	 *
	 * @return true, if successful
	 */
	private boolean evaluateAuthenticationCheck()
	{
		final RuleConditionData ruleConditionData = getRuleConditionDataByDefinitionId(BlControllerConstants.Y_TARGET_CUSTOMERS);
		if (Objects.nonNull(ruleConditionData)
				&& checkIfRuleParameterDataIsAvailable(ruleConditionData, BlControllerConstants.CUSTOMERS))
		{
			final RuleParameterData ruleParameterData = ruleConditionData.getParameters().get(BlControllerConstants.CUSTOMERS);
			final Object value = ruleParameterData.getValue();
			if (value instanceof List && CollectionUtils.isNotEmpty((List) value))
			{
				final String anonymousUserPK = String.valueOf(((List) value).get(0));
				return getUserService().getAnonymousUser().getPk().toString().equals(anonymousUserPK)
						&& getUserService().getCurrentUser().getPk().toString().equals(anonymousUserPK);
			}
		}
		return false;
	}

	/**
	 * Check promotion for cart type.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean checkPromotionOnBasisOfCartType(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (promotionSourceRule.getConditions().contains(BlControllerConstants.Y_RENTAL_CART_CONDITION))
		{
			if (checkRuleForPromotionCondition(promotionSourceRule, BlControllerConstants.RENTAL_CART_TRUE))
			{
				if (Objects.isNull(getExtendedOrder()) && BooleanUtils.isFalse(getBlCartService().isRentalCartOnly()))
				{
					addAndLogMessage("promotion.validation.message.for.rental.cart", null, model, redirectAttributes);
					return false;
				}
				return checkIfPromotionIsEligibleForArrivalDate(model, redirectAttributes, promotionSourceRule);
			}
			if (checkRuleForPromotionCondition(promotionSourceRule, BlControllerConstants.RENTAL_CART_FALSE)
					&& BooleanUtils.isTrue(getBlCartService().isRentalCartOnly()))
			{
				addAndLogMessage("promotion.validation.message.for.used.cart", null, model, redirectAttributes);
				return false;
			}
		}
		return checkItemsForPromotion(model, redirectAttributes, promotionSourceRule);
	}



	/**
	 * Check if promotion is eligible for arrival date.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean checkIfPromotionIsEligibleForArrivalDate(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (promotionSourceRule.getConditions().contains(BlControllerConstants.Y_RENTAL_ARRIVAL_DATE_CONDITION))
		{
			final RuleConditionData ruleConditionData = getRuleConditionDataByDefinitionId(
					BlControllerConstants.Y_RENTAL_ARRIVAL_DATE_CONDITION);
			final RuleParameterData ruleParameterData = getRuleParameterData(ruleConditionData);
			if (Objects.nonNull(ruleParameterData) && ruleParameterData.getValue() instanceof Date)
			{
				final Date promoConditionArrivaldate = (Date) ruleParameterData.getValue();
				final Date arrivalDate = Objects.nonNull(getExtendedOrder()) ? getExtendedOrder().getRentalStartDate()
						: getCartRao().getRentalArrivalDate();
				if (Objects.isNull(arrivalDate))
				{
					addAndLogMessage("promotion.validation.message.promo.arrival.date", new Object[]
					{ getFormattedStringDate(promoConditionArrivaldate) }, model, redirectAttributes);
					return false;
				}
				if (BooleanUtils.isFalse(DateUtils.isSameDay(arrivalDate, promoConditionArrivaldate))
						&& arrivalDate.compareTo(promoConditionArrivaldate) > 0)
				{
					addAndLogMessage("promotion.validation.message.promo.arrival.date", new Object[]
					{ getFormattedStringDate(promoConditionArrivaldate) }, model, redirectAttributes);
					return false;
				}
			}
		}
		return checkPromotionForRentalDays(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Gets the rule parameter data.
	 *
	 * @param ruleConditionData
	 *           the rule condition data
	 * @return the rule parameter data
	 */
	private RuleParameterData getRuleParameterData(final RuleConditionData ruleConditionData)
	{
		return Objects.nonNull(ruleConditionData)
				&& checkIfRuleParameterDataIsAvailable(ruleConditionData, BlControllerConstants.RENTAL_ARRIVAL_DATE)
						? ruleConditionData.getParameters().get(BlControllerConstants.RENTAL_ARRIVAL_DATE)
						: null;
	}

	/**
	 * Check promotion for rental days.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean checkPromotionForRentalDays(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (promotionSourceRule.getConditions().contains(BlControllerConstants.Y_RENTAL_DURATION_CONDITION))
		{
			final RuleConditionData ruleConditionData = getRuleConditionDataByDefinitionId(
					BlControllerConstants.Y_RENTAL_DURATION_CONDITION);
			if (Objects.nonNull(ruleConditionData))
			{
				final RuleParameterData ruleParameterData = checkIfRuleParameterDataIsAvailable(ruleConditionData,
						BlControllerConstants.RENTAL_DURATION_OPERATOR)
								? ruleConditionData.getParameters().get(BlControllerConstants.RENTAL_DURATION_OPERATOR)
								: null;
				if (isInValidForRentalDays(ruleParameterData, ruleConditionData, model, redirectAttributes))
				{
					return false;
				}
			}
		}
		return checkItemsForPromotion(model, redirectAttributes, promotionSourceRule);
	}

	/**
	 * Checks if is in valid for rental days.
	 *
	 * @param ruleParameterData
	 *           the rule parameter data
	 * @param ruleConditionData
	 *           the rule condition data
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @return true, if is in valid for rental days
	 */
	private boolean isInValidForRentalDays(final RuleParameterData ruleParameterData, final RuleConditionData ruleConditionData,
			final Model model, final RedirectAttributes redirectAttributes)
	{
		if (Objects.nonNull(ruleParameterData) && ruleParameterData.getValue() instanceof AmountOperator)
		{
			final RuleParameterData rentalDurationData = checkIfRuleParameterDataIsAvailable(ruleConditionData,
					BlControllerConstants.RENTAL_DURATION)
							? ruleConditionData.getParameters().get(BlControllerConstants.RENTAL_DURATION)
							: null;
			if (Objects.nonNull(rentalDurationData) && rentalDurationData.getValue() instanceof Integer)
			{
				final int rentalDurationOnPromotion = ((Integer) rentalDurationData.getValue()).intValue();
				final int orderRentalDuration = getRentalDaysOnOrder();
				final String operatorToExecute = ((AmountOperator) ruleParameterData.getValue()).toString();
				switch (operatorToExecute)
				{
					case "EQUAL":
						return isRentalDaysExactToSpecifiedDays(rentalDurationOnPromotion, orderRentalDuration, model,
								redirectAttributes);
					case "GREATER_THAN_OR_EQUAL":
						return isRentalDaysLessThenSpecifiedDays(rentalDurationOnPromotion, orderRentalDuration, model,
								redirectAttributes);
					default:
						return false;
				}
			}
		}
		return false;
	}

	/**
	 * Gets the rental days on order.
	 *
	 * @return the rental days on order
	 */
	private int getRentalDaysOnOrder()
	{
		if (Objects.nonNull(getExtendedOrder()))
		{
			return Objects.nonNull(getExtendedOrder().getTotalExtendDays()) ? getExtendedOrder().getTotalExtendDays().intValue() : 0;
		}
		return Objects.nonNull(getCartRao()) && Objects.nonNull(getCartRao().getRentalDurationDays())
				? getCartRao().getRentalDurationDays().intValue()
				: 0;
	}

	/**
	 * Checks if is rental days less then specified days.
	 *
	 * @param promoRentalDurationDays
	 *           the promo rental duration days
	 * @param orderRentalDuration
	 *           the order rental duration
	 * @param model
	 *           the model
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @return true, if is rental days less then specified days
	 */
	private boolean isRentalDaysLessThenSpecifiedDays(final int promoRentalDurationDays, final int orderRentalDuration,
			final Model model, final RedirectAttributes redirectAttributes)
	{
		if (orderRentalDuration < promoRentalDurationDays)
		{
			addAndLogMessage("promotion.validation.message.is.less.days", new Object[]
			{ promoRentalDurationDays }, model, redirectAttributes);
			return true;
		}
		return false;
	}

	/**
	 * Checks if is rental days is exact to specified days.
	 *
	 * @param promoRentalDurationDays
	 *           the promo rental duration days
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @return the boolean
	 */
	private Boolean isRentalDaysExactToSpecifiedDays(final int promoRentalDurationDays, final int orderRentalDuration,
			final Model model, final RedirectAttributes redirectAttributes)
	{
		if (orderRentalDuration != promoRentalDurationDays)
		{
			addAndLogMessage("promotion.validation.message.is.exact.days", new Object[]
			{ promoRentalDurationDays }, model, redirectAttributes);
			return true;
		}
		return false;
	}

	/**
	 * Check items for promotion.
	 *
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @return true, if successful
	 */
	private boolean checkItemsForPromotion(final Model model, final RedirectAttributes redirectAttributes,
			final PromotionSourceRuleModel promotionSourceRule)
	{
		if (promotionSourceRule.getConditions().contains(BlControllerConstants.Y_QUALIFYING_PRODUCTS))
		{
			final RuleConditionData ruleConditionData = getRuleConditionDataByDefinitionId(
					BlControllerConstants.Y_QUALIFYING_PRODUCTS);
			if (Objects.nonNull(ruleConditionData))
			{
				final RuleParameterData operatorRuleParameterData = checkIfRuleParameterDataIsAvailable(ruleConditionData,
						BlControllerConstants.PRODUCTS_OPERATOR)
								? ruleConditionData.getParameters().get(BlControllerConstants.PRODUCTS_OPERATOR)
								: null;
				if (checkForItemsInEntries(ruleConditionData, operatorRuleParameterData, model, redirectAttributes))
				{
					return false;
				}
			}
		}
		return true;
	}

	/**
	 * Check for items in entries.
	 *
	 * @param ruleConditionData
	 *           the rule condition data
	 * @param operatorRuleParameterData
	 *           the operator rule parameter data
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @return true, if successful
	 */
	private boolean checkForItemsInEntries(final RuleConditionData ruleConditionData,
			final RuleParameterData operatorRuleParameterData, final Model model, final RedirectAttributes redirectAttributes)
	{
		if (Objects.nonNull(operatorRuleParameterData) && operatorRuleParameterData.getValue() instanceof CollectionOperator)
		{
			final RuleParameterData productsData = checkIfRuleParameterDataIsAvailable(ruleConditionData,
					BlControllerConstants.PRODUCTS) ? ruleConditionData.getParameters().get(BlControllerConstants.PRODUCTS) : null;
			if (Objects.nonNull(productsData) && productsData.getValue() instanceof List
					&& CollectionUtils.isNotEmpty((List) productsData.getValue()))
			{
				final List<String> productCodesFromPromotion = getProductCodesFromList(
						Lists.newArrayList((List) productsData.getValue()));
				final List<String> productCodesFromCartEntry = getProductCodesFromCartEntry();

				final String productCompareOperator = ((CollectionOperator) operatorRuleParameterData.getValue()).toString();
				switch (productCompareOperator)
				{
					case "CONTAINS_ANY":
						return evaluteForContainsAny(productCodesFromPromotion, productCodesFromCartEntry, model, redirectAttributes);

					case "CONTAINS_ALL":
						return evaluteForContainsAll(productCodesFromPromotion, productCodesFromCartEntry, model, redirectAttributes);

					default:
						return false;
				}
			}
		}
		return false;
	}

	/**
	 * Gets the product codes from cart entry.
	 *
	 * @return the product codes from cart entry
	 */
	private List<String> getProductCodesFromCartEntry()
	{
		if (Objects.nonNull(getExtendedOrder()))
		{
			return getExtendedOrder().getEntries().stream().map(entry -> entry.getProduct().getCode()).collect(Collectors.toList());
		}
		return getCartRao().getEntries().stream().map(entry -> entry.getProductCode()).collect(Collectors.toList());
	}

	/**
	 * Evalute for contains any.
	 *
	 * @param productCodesFromPromotion
	 *           the product codes from promotion
	 * @param productCodesFromCartEntry
	 *           the product codes from cart entry
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @return true, if successful
	 */
	private boolean evaluteForContainsAny(final List<String> productCodesFromPromotion,
			final List<String> productCodesFromCartEntry, final Model model, final RedirectAttributes redirectAttributes)
	{
		if (BooleanUtils.isFalse(productCodesFromPromotion.stream().anyMatch(productCodesFromCartEntry::contains)))
		{
			final List<String> productNamesFromCode = getProductNamesFromCode(productCodesFromPromotion);
			addAndLogMessage("promotion.validation.message.items.missing", new Object[]
			{ String.join(", ", productNamesFromCode) }, model, redirectAttributes);
			return true;
		}
		return false;
	}

	/**
	 * Evalute for contains all.
	 *
	 * @param productCodesFromPromotion
	 *           the product codes from promotion
	 * @param productCodesFromCartEntry
	 *           the product codes from cart entry
	 * @param redirectAttributes
	 *           the redirect attributes
	 * @return true, if successful
	 */
	private boolean evaluteForContainsAll(final List<String> productCodesFromPromotion,
			final List<String> productCodesFromCartEntry, final Model model, final RedirectAttributes redirectAttributes)
	{
		if (BooleanUtils.isFalse(productCodesFromPromotion.stream().allMatch(productCodesFromCartEntry::contains)))
		{
			final List<String> productNamesFromCode = getProductNamesFromCode(productCodesFromPromotion);
			addAndLogMessage("promotion.validation.message.items.missing", new Object[]
			{ String.join(", ", productNamesFromCode) }, model, redirectAttributes);
			return true;
		}
		return false;
	}

	/**
	 * Gets the product names from code.
	 *
	 * @param productCodesFromPromotion
	 *           the product codes from promotion
	 * @return the product names from code
	 */
	private List<String> getProductNamesFromCode(final List<String> productCodesFromPromotion)
	{
		final List<String> productNames = Lists.newArrayList();
		productCodesFromPromotion.forEach(code -> {
			final List<ProductModel> findProductsByCode = Lists.newArrayList();
			try
			{
				findProductsByCode.addAll(getBlProductDao().findProductsByCode(code));
			}
			catch (final Exception exception)
			{
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "No product found for code: {} with error message : {}", code,
						exception.getMessage());
			}
			if (CollectionUtils.isNotEmpty(findProductsByCode))
			{
				productNames.add(findProductsByCode.iterator().next().getName());
			}
		});
		return productNames;
	}

	/**
	 * Gets the product codes from list.
	 *
	 * @param lProductCodes
	 *           the l product codes
	 * @return the product codes from list
	 */
	private List<String> getProductCodesFromList(final List<String> lProductCodes)
	{
		return lProductCodes.stream().map(code -> {
			if (code.contains(BlControllerConstants.PRODUCT_CODE_SPLITTER))
			{
				final String[] split = code.split(BlControllerConstants.PRODUCT_CODE_SPLITTER);
				return split.length >= 1 ? split[0] : StringUtils.EMPTY;
			}
			return code;
		}).collect(Collectors.toList());
	}

	/**
	 * Check rule for promotion condition.
	 *
	 * @param promotionSourceRule
	 *           the promotion source rule
	 * @param promotionCondition
	 *           the promotion condition
	 * @return true, if successful
	 */
	private boolean checkRuleForPromotionCondition(final PromotionSourceRuleModel promotionSourceRule,
			final String promotionCondition)
	{
		final Optional<AbstractRuleEngineRuleModel> ruleModel = promotionSourceRule.getEngineRules().stream()
				.filter(engineRule -> engineRule.getActive() && engineRule.getCurrentVersion()).findFirst();
		return ruleModel.isPresent() && ruleModel.get().getRuleContent().contains(promotionCondition);
	}

	/**
	 * Adds the message in redirects and log message.
	 *
	 * @param key
	 *           the key
	 * @param arguments
	 *           the arguments
	 * @param redirectAttributes
	 *           the redirect attributes
	 */
	private void addAndLogMessage(final String key, final Object[] arguments, final Model model,
			final RedirectAttributes redirectAttributes)
	{
		final String message = getMessageForKey(key, arguments);
		if (Objects.nonNull(model))
		{
			model.addAttribute(getErrorKey(), message);
		}
		if (Objects.nonNull(redirectAttributes))
		{
			redirectAttributes.addFlashAttribute(getErrorKey(), message);
		}
		BlLogger.logMessage(LOG, Level.DEBUG, message);
	}

	/**
	 * Gets the rule condition data by definition id.
	 *
	 * @param definitionId
	 *           the definition id
	 * @return the rule condition data by definition id
	 */
	private RuleConditionData getRuleConditionDataByDefinitionId(final String definitionId)
	{
		final List<RuleConditionData> lRuleConditionData = getRuleConditions().stream()
				.filter(ruleConditionData -> ruleConditionData.getDefinitionId().equalsIgnoreCase(definitionId))
				.collect(Collectors.toList());
		return CollectionUtils.isNotEmpty(lRuleConditionData)
				? lRuleConditionData.get(lRuleConditionData.size() - BlCoreConstants.INT_ONE)
				: null;
	}

	/**
	 * Gets the formatted string date.
	 *
	 * @param date
	 *           the date
	 * @return the formatted string date
	 */
	private String getFormattedStringDate(final Date date)
	{
		return BlDateTimeUtils.convertDateToStringDate(date, BlControllerConstants.DATE_FORMAT);
	}

	/**
	 * Gets the message for key.
	 *
	 * @param key
	 *           the key
	 * @param args
	 *           the args
	 * @return the message for key
	 */
	private String getMessageForKey(final String key, final Object[] args)
	{
		return getMessageSource().getMessage(key, args, getI18nService().getCurrentLocale());
	}

	/**
	 * Check if rule parameter data is available for given key.
	 *
	 * @param ruleConditionData
	 *           the rule condition data
	 * @param key
	 *           the key
	 * @return true, if successful
	 */
	private boolean checkIfRuleParameterDataIsAvailable(final RuleConditionData ruleConditionData, final String key)
	{
		return MapUtils.isNotEmpty(ruleConditionData.getParameters()) && ruleConditionData.getParameters().containsKey(key);
	}

	public BlPromotionDao getPromotionDao()
	{
		return promotionDao;
	}

	public void setPromotionDao(final BlPromotionDao promotionDao)
	{
		this.promotionDao = promotionDao;
	}

	public BlCartService getBlCartService()
	{
		return blCartService;
	}

	public void setBlCartService(final BlCartService blCartService)
	{
		this.blCartService = blCartService;
	}

	public MessageSource getMessageSource()
	{
		return messageSource;
	}

	public void setMessageSource(final MessageSource messageSource)
	{
		this.messageSource = messageSource;
	}

	public I18NService getI18nService()
	{
		return i18nService;
	}

	public void setI18nService(final I18NService i18nService)
	{
		this.i18nService = i18nService;
	}

	public UserService getUserService()
	{
		return userService;
	}

	public void setUserService(final UserService userService)
	{
		this.userService = userService;
	}

	public String getVoucherCode()
	{
		return voucherCode;
	}

	public void setVoucherCode(final String voucherCode)
	{
		this.voucherCode = voucherCode;
	}

	public List<RuleConditionData> getRuleConditions()
	{
		return ruleConditions;
	}

	public void setRuleConditions(final List<RuleConditionData> ruleConditions)
	{
		this.ruleConditions = ruleConditions;
	}

	public RuleConditionsService getRuleConditionsService()
	{
		return ruleConditionsService;
	}

	public void setRuleConditionsService(final RuleConditionsService ruleConditionsService)
	{
		this.ruleConditionsService = ruleConditionsService;
	}

	public RuleConditionsRegistry getRuleConditionsRegistry()
	{
		return ruleConditionsRegistry;
	}

	public void setRuleConditionsRegistry(final RuleConditionsRegistry ruleConditionsRegistry)
	{
		this.ruleConditionsRegistry = ruleConditionsRegistry;
	}

	public BlCouponManagementService getBlCouponManagementService()
	{
		return blCouponManagementService;
	}

	public void setBlCouponManagementService(final BlCouponManagementService blCouponManagementService)
	{
		this.blCouponManagementService = blCouponManagementService;
	}

	public Converter<AbstractOrderModel, CartRAO> getCartRaoConverter()
	{
		return cartRaoConverter;
	}

	public void setCartRaoConverter(final Converter<AbstractOrderModel, CartRAO> cartRaoConverter)
	{
		this.cartRaoConverter = cartRaoConverter;
	}

	public CartRAO getCartRao()
	{
		return cartRao;
	}

	public void setCartRao(final CartRAO cartRao)
	{
		this.cartRao = cartRao;
	}

	public BlProductDao getBlProductDao()
	{
		return blProductDao;
	}

	public void setBlProductDao(final BlProductDao blProductDao)
	{
		this.blProductDao = blProductDao;
	}

	public String getErrorKey()
	{
		return errorKey;
	}

	public void setErrorKey(final String errorKey)
	{
		this.errorKey = StringUtils.isNotBlank(errorKey) ? errorKey : BlControllerConstants.ERROR_MSG_TYPE;
	}

	public CustomerAccountService getCustomerAccountService()
	{
		return customerAccountService;
	}

	public void setCustomerAccountService(final CustomerAccountService customerAccountService)
	{
		this.customerAccountService = customerAccountService;
	}

	public BaseStoreService getBaseStoreService()
	{
		return baseStoreService;
	}

	public void setBaseStoreService(final BaseStoreService baseStoreService)
	{
		this.baseStoreService = baseStoreService;
	}

	public AbstractOrderModel getExtendedOrder()
	{
		return extendedOrder;
	}

	public void setExtendedOrder(final AbstractOrderModel extendedOrder)
	{
		this.extendedOrder = extendedOrder;
	}

}
