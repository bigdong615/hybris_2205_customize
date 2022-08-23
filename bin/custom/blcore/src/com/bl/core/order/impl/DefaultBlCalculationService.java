package com.bl.core.order.impl;

import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.order.impl.DefaultCalculationService;
import de.hybris.platform.order.strategies.calculation.OrderRequiresCalculationStrategy;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.internal.dao.GenericDao;
import de.hybris.platform.util.DiscountValue;
import de.hybris.platform.util.PriceValue;
import de.hybris.platform.util.TaxValue;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.constants.BlCancelRefundLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.constants.GeneratedBlCoreConstants.Enumerations.ProductTypeEnum;
import com.bl.core.model.BlDamageWaiverPricingModel;
import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.BlCalculationService;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.services.tax.DefaultBlExternalTaxesService;
import com.bl.core.utils.BlExtendOrderUtils;
import com.bl.core.utils.BlReplaceMentOrderUtils;
import com.bl.logging.BlLogger;

/**
 * {@inheritDoc}
 *
 * @author Ravikumar
 *
 */
public class DefaultBlCalculationService extends DefaultCalculationService implements BlCalculationService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlCalculationService.class);
	private BlCommercePriceService commercePriceService;
	private GenericDao<BlDamageWaiverPricingModel> blDamageWaiverGenericDao;
	private OrderRequiresCalculationStrategy defaultOrderRequiresCalculationStrategy;
	private CommonI18NService defaultCommonI18NService;
	private DefaultBlExternalTaxesService defaultBlExternalTaxesService;
	private static final ThreadLocal<Boolean> saveOrderEntryUnneeded = new ThreadLocal<>();

	@Resource(name="orderRequiresCalculationStrategy")
	private OrderRequiresCalculationStrategy orderRequiresCalculationStrategy;

	@Resource(name="commonI18NService")
	private CommonI18NService commonI18NService;
	/**
	 * Reset all values of entry before calculation.
	 *
	 * @param entry
	 *           the entry
	 * @throws CalculationException
	 *            the calculation exception
	 */
	@Override
	protected void resetAllValues(final AbstractOrderEntryModel entry) throws CalculationException
	{
		final ProductModel product = entry.getProduct();
		final Collection<TaxValue> entryTaxes = findTaxValues(entry);
		entry.setTaxValues(entryTaxes);
		final AbstractOrderModel order = entry.getOrder();
		final PriceValue pv = getPriceForSkuOrSerial(order, entry, product);
		final PriceValue basePrice = convertPriceIfNecessary(pv, order.getNet().booleanValue(), order.getCurrency(), entryTaxes);
		LOG.info("BasePrice before dynamic" + basePrice.getValue());
		final PriceValue dynamicBasePrice = ((BlProductModel)product).isBundleProduct()? basePrice : getDynamicBasePriceForRentalSKU(basePrice, product, order);
		LOG.info("dynamicBasePrice dynamic" + dynamicBasePrice.getValue());
		entry.setBasePrice(Double.valueOf(dynamicBasePrice.getValue()));
		final List<DiscountValue> entryDiscounts = findDiscountValues(entry);
		entry.setDiscountValues(entryDiscounts);
		setDamageWaiverPrices(entry, product);
	}

	/**
	 * Calculate entries.
	 *
	 * @param order
	 *           the order
	 * @param forceRecalculate
	 *           the force recalculate
	 * @throws CalculationException
	 *            the calculation exception
	 */
	@Override
	public void calculateEntries(final AbstractOrderModel order, final boolean forceRecalculate) throws CalculationException
	{
		if(BooleanUtils.isTrue(BlReplaceMentOrderUtils.isReplaceMentOrder()) && null != getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST)) {
			BlReplaceMentOrderUtils.updateCartForReplacementOrder(order);
		}
		else {
			double totalOptionCost = 0.0;
			double subtotal = 0.0;
			double totalDamageWaiverCost = 0.0;
			final List<AbstractOrderEntryModel> entries =order.getEntries().stream().filter(entry -> !entry.isBundleEntry()).collect(
					Collectors.toList());
			for (final AbstractOrderEntryModel e : entries) {
				recalculateOrderEntryIfNeeded(e, forceRecalculate);
				subtotal += e.getTotalPrice().doubleValue();
				if(!BlCoreConstants.AQUATECH_BRAND_ID.equals(e.getProduct().getManufacturerAID())) {
					totalDamageWaiverCost += getDamageWaiverPriceFromEntry(e);
					totalOptionCost += getTotalOptionPrice(e);
				}
			}
			if(BooleanUtils.isFalse(order.isGiftCardOrder()) && BooleanUtils.isFalse(order.getIsRetailGearOrder())){
				final Double finaltotalDamageWaiverCost = Double.valueOf(totalDamageWaiverCost);
				order.setTotalDamageWaiverCost(finaltotalDamageWaiverCost);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total Damage Waiver Cost : {}",
						finaltotalDamageWaiverCost);

				final Double finaltotalOptionCost = Double.valueOf(totalOptionCost);
				order.setTotalOptionsCost(finaltotalOptionCost);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total Option Cost : {}", finaltotalOptionCost);

				final Double totalPriceWithDamageWaiverCostAndOption = Double
						.valueOf(subtotal + totalDamageWaiverCost+ totalOptionCost);

				order.setTotalPrice(totalPriceWithDamageWaiverCostAndOption);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total Price : {}",
						totalPriceWithDamageWaiverCostAndOption);
			}
			getDefaultBlExternalTaxesService().calculateExternalTaxes(order);
		}
	}
	/**
	 * Get total option price
	 * @param AbstractOrderEntryModel
	 *           the entry
	 * @return the total option price from entry
	 */
	private double getTotalOptionPrice(final AbstractOrderEntryModel entry){
		if(CollectionUtils.isNotEmpty(entry.getOptions())){
			final BlOptionsModel blOptionsModel = entry.getOptions().iterator().next();
			if(blOptionsModel.getUnitCost() != null){
				return blOptionsModel.getUnitPrice().doubleValue() * entry.getQuantity().intValue();
			}
		}

		return Double.valueOf(0.0d);
	}

	/**
	 * Calculate totals.
	 *
	 * @param order
	 *           the order
	 * @param recalculate
	 *           the recalculate
	 * @param taxValueMap
	 *           the tax value map
	 * @throws CalculationException
	 *            the calculation exception
	 */
	@Override
	protected void calculateTotals(final AbstractOrderModel order, final boolean recalculate,
								   final Map<TaxValue, Map<Set<TaxValue>, Double>> taxValueMap) throws CalculationException
	{
		if(BooleanUtils.isTrue(BlReplaceMentOrderUtils.isReplaceMentOrder()) && null != getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST)) {
			BlReplaceMentOrderUtils.updateCartForReplacementOrder(order);
		}
		else {
			if (recalculate || getDefaultOrderRequiresCalculationStrategy().requiresCalculation(order)) {
				double totalOptionCost = 0.0;
				double totalDamageWaiverCost = 0.0;
				final CurrencyModel curr = order.getCurrency();
				final int digits = curr.getDigits().intValue();
				// subtotal
				final double subtotal = order.getSubtotal().doubleValue();
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total totalDamageWaiverCost : {}", totalDamageWaiverCost);
				LOG.info("calculateTotals->subtotal" + subtotal);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total subtotal : {}", subtotal);
				//totalDamageWaiverCost
				if (BooleanUtils.isTrue(order.getIsRentalOrder())) {
					totalDamageWaiverCost = Objects.nonNull(order.getTotalDamageWaiverCost())
							? order.getTotalDamageWaiverCost().doubleValue()
							: 0.0d;
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Damage Waiver Cost : {}",
							totalDamageWaiverCost);
					totalOptionCost = Objects.nonNull(order.getTotalOptionsCost())
							? order.getTotalOptionsCost().doubleValue()
							: 0.0d;
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Option Cost : {}", totalOptionCost);

				}
				LOG.info("calculateTotals->subtotal2" + subtotal);
				LOG.info("calculateTotals->totalOptionCost" + totalOptionCost);
				LOG.info("totalDamageWaiverCost->subtotal2" + totalDamageWaiverCost);
				calculateTotalsForCart(order , recalculate , digits , subtotal , totalDamageWaiverCost , totalOptionCost);
			}
		}

	}

	/**
	 * This method added to calculate the total for cart
	 */

	private void calculateTotalsForCart(final AbstractOrderModel order, final boolean recalculate , final int digits ,
										final double subtotal , final double totalDamageWaiverCost ,final double totalOptionCost){
		final double totalDiscounts = calculateDiscountValues(order, recalculate);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "totalDiscounts Price : {}", totalDiscounts);
		final double roundedTotalDiscounts = getDefaultCommonI18NService().roundCurrency(totalDiscounts, digits);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "roundedTotalDiscounts Price : {}", roundedTotalDiscounts);
		order.setTotalDiscounts(roundedTotalDiscounts);

		// Set Delivery Cost as 0 for Extend rental order based on flag -> isExtendedOrder
		if (BooleanUtils.isTrue(order.getIsExtendedOrder())) {
			order.setDeliveryCost(0.0);
			BlExtendOrderUtils.setCurrentExtendOrderToSession(order);
		}

		getDefaultBlExternalTaxesService().calculateExternalTaxes(order);
		final double totalRoundedTaxes = getDefaultCommonI18NService().roundCurrency(order.getTotalTax(), digits);
		order.setTotalTax(totalRoundedTaxes);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Tax Price : {}", totalRoundedTaxes);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "SubTotal Price before round : {}", subtotal);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "TotalOptionCost Price before round : {}", totalOptionCost);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "TotalDamageWaiverCost : {}", totalDamageWaiverCost);
		// set total
		final double total = subtotal + totalDamageWaiverCost + totalOptionCost + order.getPaymentCost() + order.getDeliveryCost()
				- roundedTotalDiscounts + order.getTotalTax();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Price before round : {}", total);
		final double totalRounded = getDefaultCommonI18NService().roundCurrency(total, digits);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Rounded Price : {}", totalRounded);

		if(order instanceof OrderModel) {
			this.orderTotalCalculation(order, totalRounded);
		} else {
			order.setTotalPrice(totalRounded);
		}

		setCalculatedStatus(order);
		saveOrder(order);
	}

	/**
	 * This method will calculate order and grand total after modification
	 * @param order order
	 * @param totalRounded amt
	 */
	private void orderTotalCalculation(final AbstractOrderModel order, final double totalRounded) {
		final double gcAmount = this.getGiftCardAmount(order);
		if (gcAmount > BlCancelRefundLoggingConstants.ZERO) {
			if(Double.compare(order.getOriginalOrderTotalAmount(), order.getGiftCardAmount()) == BlInventoryScanLoggingConstants.ZERO) {
				order.setTotalPrice(0.0D);
			} else {
				order.setTotalPrice(totalRounded - gcAmount);
			}
			order.setGrandTotal(totalRounded);
		} else {
			order.setTotalPrice(totalRounded);
			order.setGrandTotal(0.0D);
		}
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "orderTotalCalculation : {}", totalRounded);
	}

	/**
	 * this will return gift card amount
	 * @param order order
	 * @return amount
	 */
	private double getGiftCardAmount(final AbstractOrderModel order) {
		final double totalAmt = (order.getGiftCardAvailableAmount() == null || order.getGiftCardAvailableAmount() ==
				BlCancelRefundLoggingConstants.ZERO) ? order.getGiftCardAmount() : order.getGiftCardAvailableAmount();
		return BigDecimal.valueOf((totalAmt < BlInventoryScanLoggingConstants.ZERO) ? -totalAmt : totalAmt).setScale(
				BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue();
	}


	/**
	 * Gets the damage Waiver price from entry.
	 *
	 * @param cartEntry
	 *           the cart entry
	 * @return the damage Waiver price from entry
	 */
	private double getDamageWaiverPriceFromEntry(final AbstractOrderEntryModel cartEntry)
	{
		final ProductModel product = cartEntry.getProduct();
		if (PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product))
		{
			if (BooleanUtils.isTrue(cartEntry.getGearGuardWaiverSelected()))
			{
				return cartEntry.getGearGuardWaiverPrice().doubleValue() * cartEntry.getQuantity().doubleValue();
			}
			if (BooleanUtils.isTrue(cartEntry.getGearGuardProFullWaiverSelected()))
			{
				return cartEntry.getGearGuardProFullWaiverPrice().doubleValue() * cartEntry.getQuantity().doubleValue();
			}
			return 0.0d;
		}
		return 0.0d;
	}

	/**
	 * Gets the price for sku or serial.
	 *
	 * @param order
	 *           the order
	 * @param entry
	 *           the entry
	 * @param product
	 *           the product
	 * @return the price for sku or serial
	 * @throws CalculationException
	 *            the calculation exception
	 */
	private PriceValue getPriceForSkuOrSerial(final AbstractOrderModel order, final AbstractOrderEntryModel entry,
											  final ProductModel product) throws CalculationException
	{

		if (PredicateUtils.instanceofPredicate(BlSerialProductModel.class).evaluate(product))
		{
			final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) product;
			if (Objects.nonNull(blSerialProductModel.getIncentivizedPrice()))
			{
				return createNewPriceValue(order.getCurrency().getIsocode(),
						blSerialProductModel.getIncentivizedPrice().doubleValue(), BooleanUtils.toBoolean(order.getNet()));
			}
			else if (Objects.nonNull(blSerialProductModel.getFinalSalePrice()))
			{
				return createNewPriceValue(order.getCurrency().getIsocode(), blSerialProductModel.getFinalSalePrice().doubleValue(),
						BooleanUtils.toBoolean(order.getNet()));
			}
			throw new CalculationException(
					"No Price defined for Serial Product with Serial Id : ".concat(blSerialProductModel.getProductId()));
		}
		else if (PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product))
		{
			// Create new price value for gift card purchase
			if (ProductTypeEnum.GIFTCARD.equals(((BlProductModel) product).getProductType()))
			{
				return createNewPriceValue(order.getCurrency().getIsocode(), order.getGiftCardCost().doubleValue(),
						BooleanUtils.toBoolean(order.getNet()));
			}
			if (BooleanUtils.isTrue(((BlProductModel) product).getRetailGear()))
			{
				return createNewPriceValue(order.getCurrency().getIsocode(),((BlProductModel) product).getRetailGearPrice().doubleValue(),BooleanUtils.toBoolean(order.getNet()));
			}
			else if(BooleanUtils.isTrue(((BlProductModel) product).isBundleProduct())){
				 return commercePriceService.getDynamicPriceForBundle(product,order);
			}
			else{
				return findBasePrice(entry);
			}

		}

		throw new CalculationException("Product Type is not a type of SKU or Serial Product");
	}

	/**
	 * Gets the dynamic base price for rental SKU.
	 *
	 * @param basePrice
	 *           the base price
	 * @param product
	 *           the product
	 * @return the dynamic base price for rental SKU
	 */
	private PriceValue getDynamicBasePriceForRentalSKU(final PriceValue basePrice, final ProductModel product, final AbstractOrderModel order)
	{

		if (!PredicateUtils.instanceofPredicate(BlSerialProductModel.class).evaluate(product)
				&& PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product) && BooleanUtils.isFalse(
				ProductTypeEnum.GIFTCARD.equals(((BlProductModel) product).getProductType())) && BooleanUtils.isFalse(
				((BlProductModel) product).getRetailGear()))
		{
			final BlProductModel blProductModel = (BlProductModel) product;
			Long rentedDays = null;
			if(order instanceof OrderModel)
			{
				rentedDays = getRentedDays(order.getRentalStartDate(), order.getRentalEndDate());
			}
			final BigDecimal dynamicPriceDataForProduct = getCommercePriceService()
					.getDynamicPriceDataForProductForOrder(blProductModel.getConstrained(), Double.valueOf(basePrice.getValue()), rentedDays);
			return createNewPriceValue(basePrice.getCurrencyIso(), dynamicPriceDataForProduct.doubleValue(), basePrice.isNet());
		}
		return basePrice;


	}

	/**
	 * This method will return long days
	 * @param startDate date
	 * @param endDate date
	 * @return long days
	 */
	private Long getRentedDays(final Date startDate, final Date endDate)
	{
		final LocalDate arrDate = startDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		final LocalDate retDate = endDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();

		return ChronoUnit.DAYS.between(arrDate, retDate);
	}



	/**
	 * Sets the damage Waiver prices. Calculating on the basis of the type of SKU and percentage.
	 *
	 * @param cartEntry
	 *           the cart entry
	 * @param product
	 *           the product
	 * @throws CalculationException
	 *            the calculation exception
	 */
	private void setDamageWaiverPrices(final AbstractOrderEntryModel cartEntry, final ProductModel product)
			throws CalculationException
	{
		if (PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product))
		{
			final List<BlDamageWaiverPricingModel> lDamageWaiverPricing = getBlDamageWaiverGenericDao().find();
			if (CollectionUtils.isEmpty(lDamageWaiverPricing))
			{
				throw new CalculationException("No Damage Waiver Pricing Percentage found");
			}
			final Double gearGuardWaiverPrice = setGearGuardDamageWaiverPrice(cartEntry, product, lDamageWaiverPricing);

			setGearGuardProFullDamageWaiverPrice(cartEntry, lDamageWaiverPricing, gearGuardWaiverPrice);

			setNoDamageWaiverPrice(cartEntry);
		}
	}

	/**
	 * Sets the no damage waiver price.
	 *
	 * @param cartEntry
	 *           the new no damage waiver price
	 */
	private void setNoDamageWaiverPrice(final AbstractOrderEntryModel cartEntry)
	{
		cartEntry.setNoDamageWaiverSelected(getDamageWaiverFlag(cartEntry.getNoDamageWaiverSelected(), Boolean.FALSE));
	}

	/**
	 * Sets the gear guard pro full waiver price.
	 *
	 * @param cartEntry
	 *           the cart entry
	 * @param lDamageWaiverPricing
	 *           the l damage Waiver pricing
	 * @param gearGuardWaiverPrice
	 *           the gear Guard Waiver price
	 * @throws CalculationException
	 *            the calculation exception
	 */
	private void setGearGuardProFullDamageWaiverPrice(final AbstractOrderEntryModel cartEntry,
													  final List<BlDamageWaiverPricingModel> lDamageWaiverPricing, final Double gearGuardWaiverPrice)
			throws CalculationException
	{
		final BlDamageWaiverPricingModel damageWaiverProPricing = getDamageWaiverPricingModel(lDamageWaiverPricing,
				BlCoreConstants.GEAR_GUARD_PRO);
		final double gearGuardWaiverProPrice = calculateDamageWaiverPrice(BigDecimal.valueOf(gearGuardWaiverPrice),
				damageWaiverProPricing);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Gear Guard Waiver Pro Price - {}", gearGuardWaiverProPrice);
		cartEntry.setGearGuardProFullWaiverPrice(gearGuardWaiverProPrice);
		cartEntry
				.setGearGuardProFullWaiverSelected(getDamageWaiverFlag(cartEntry.getGearGuardProFullWaiverSelected(), Boolean.FALSE));
	}

	/**
	 * Sets the gear guard damage waiver price.
	 *
	 * @param cartEntry
	 *           the cart entry
	 * @param product
	 *           the product
	 * @param lDamageWaiverPricing
	 *           the l damage Waiver pricing
	 * @return the double
	 * @throws CalculationException
	 *            the calculation exception
	 */
	private Double setGearGuardDamageWaiverPrice(final AbstractOrderEntryModel cartEntry, final ProductModel product,
												 final List<BlDamageWaiverPricingModel> lDamageWaiverPricing) throws CalculationException
	{
		final String gearType = BooleanUtils.toBoolean(((BlProductModel) product).getIsVideo()) ? BlCoreConstants.VIDEO
				: BlCoreConstants.PHOTO;
		final BlDamageWaiverPricingModel damageWaiverPricing = getDamageWaiverPricingModel(lDamageWaiverPricing, gearType);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Damage Waiver Type - {} and Price Percent - {}", gearType,
				damageWaiverPricing.getWaiverPercentage().doubleValue());
		final Double gearGuardWaiverPrice = calculateDamageWaiverPrice(BigDecimal.valueOf(cartEntry.getBasePrice().doubleValue()),
				damageWaiverPricing);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Gear Guard Waiver Price - {}", gearGuardWaiverPrice);
		cartEntry.setGearGuardWaiverPrice(gearGuardWaiverPrice);
		cartEntry.setGearGuardWaiverSelected(getDamageWaiverFlag(cartEntry.getGearGuardWaiverSelected(), Boolean.TRUE));
		return gearGuardWaiverPrice;
	}

	/**
	 * Gets the damage waiver flag.
	 *
	 * @param damageWaiverFlag
	 *           the damage waiver flag
	 * @param defaultFlag
	 *           the default flag
	 * @return the damage waiver flag
	 */
	private Boolean getDamageWaiverFlag(final Boolean damageWaiverFlag, final Boolean defaultFlag)
	{
		return Objects.isNull(damageWaiverFlag) ? defaultFlag : damageWaiverFlag;
	}

	/**
	 * Gets the damage waiver pricing model based on the gear type.
	 *
	 * @param lDamageWaiverPricing
	 *           the list of damage waiver pricing
	 * @param gearType
	 *           the gear type
	 * @return the damage waiver pricing model
	 * @throws CalculationException
	 *            the calculation exception
	 */
	private BlDamageWaiverPricingModel getDamageWaiverPricingModel(final List<BlDamageWaiverPricingModel> lDamageWaiverPricing,
																   final String gearType) throws CalculationException
	{
		final BlDamageWaiverPricingModel damageWaiverPricingModel = lDamageWaiverPricing.stream()
				.filter(damageWaiverPricing -> gearType.equals(damageWaiverPricing.getDamageWaiverGearType().getCode())).findFirst()
				.orElse(null);
		if (Objects.nonNull(damageWaiverPricingModel))
		{
			return damageWaiverPricingModel;
		}

		throw new CalculationException("No Damage Waiver Pricing Percentage found for : ".concat(gearType));
	}

	/**
	 * Calculate damage waiver price.
	 *
	 * @param price
	 *           the price
	 * @param damageWaiverPricingModel
	 *           the damage waiver pricing model
	 * @return the double
	 * @throws CalculationException
	 *            the calculation exception
	 */
	private Double calculateDamageWaiverPrice(final BigDecimal price, final BlDamageWaiverPricingModel damageWaiverPricingModel)
	{
		final BigDecimal waiverPricingPercent = BigDecimal.valueOf(damageWaiverPricingModel.getWaiverPercentage().doubleValue())
				.divide(BigDecimal.valueOf(100)).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);

		final BigDecimal gearGuardWaiverPrice = price.multiply(waiverPricingPercent).setScale(BlCoreConstants.DECIMAL_PRECISION,
				BlCoreConstants.ROUNDING_MODE);
		return gearGuardWaiverPrice.doubleValue();
	}

	/**
	 * Creates the new price value object.
	 *
	 * @param currencyIso
	 *           the currency iso
	 * @param price
	 *           the price
	 * @param isNet
	 *           the is net
	 * @return the price value
	 */
	private PriceValue createNewPriceValue(final String currencyIso, final double price, final boolean isNet)
	{
		return new PriceValue(currencyIso, price, isNet);
	}

	/**
	 * this method Created for rental Extend Order to do calculate bases on extend rental selection
	 */
	@Override
	public void recalculateForExtendOrder(final AbstractOrderModel order , final int defaultAddedTimeForExtendRental) throws CalculationException
	{
		try
		{
			saveOrderEntryUnneeded.set(Boolean.TRUE);
			calculateEntriesForExtendOrder(order, true , defaultAddedTimeForExtendRental);
			resetAllValues(order);
			calculateTotals(order, true, calculateSubtotal(order , true));
		}
		finally
		{
			saveOrderEntryUnneeded.remove();
		}
	}

	/**
	 * This method created to calculate entries for extend order
	 */
	@Override
	public void calculateEntriesForExtendOrder(final AbstractOrderModel order, final boolean forceRecalculate , final int defaultAddedTimeForExtendRental)
			throws CalculationException
	{
		double subtotal = 0.0;
		double totalDamageWaiverCost = 0.0;
		final List<AbstractOrderEntryModel> orderEntryList = order.getEntries().stream().filter(entry ->!entry.isBundleEntry()).collect(
				Collectors.toList());
		for (final AbstractOrderEntryModel entryModel : orderEntryList)
		{
			resetAllValuesForExtendOrder(entryModel , defaultAddedTimeForExtendRental);
			super.calculateTotals(entryModel , true);
			subtotal += entryModel.getTotalPrice().doubleValue();
			totalDamageWaiverCost += getDamageWaiverPriceFromEntry(entryModel);
		}
		final Double finaltotalDamageWaiverCost = Double.valueOf(totalDamageWaiverCost);
		order.setTotalDamageWaiverCost(finaltotalDamageWaiverCost);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlCoreConstants.DAMAGE_WAIVER_ERROR, finaltotalDamageWaiverCost);
		final Double totalPriceWithDamageWaiverCost = Double.valueOf(subtotal + totalDamageWaiverCost);
		order.setTotalPrice(totalPriceWithDamageWaiverCost);
		order.setDeliveryCost(0.0);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total Price : {}", totalPriceWithDamageWaiverCost);
		getDefaultBlExternalTaxesService().calculateExternalTaxes(order);
	}

	/**
	 * This method created to reset entries for extend rental order
	 */
	@Override
	public void resetAllValuesForExtendOrder(final AbstractOrderEntryModel entry , final int defaultAddedTimeForExtendRental) throws CalculationException
	{
		final ProductModel product = entry.getProduct();
		final Collection<TaxValue> entryTaxes = findTaxValues(entry);
		entry.setTaxValues(entryTaxes);
		final AbstractOrderModel order = entry.getOrder();
		final PriceValue pv;
		if(entry.isBundleMainEntry()){
			pv = commercePriceService.getDynamicBasePriceForBundle(product,defaultAddedTimeForExtendRental);
		}else {
			pv = getPriceForSkuOrSerial(order, entry, product);
		}
		final PriceValue basePrice = convertPriceIfNecessary(pv, order.getNet().booleanValue(), order.getCurrency(), entryTaxes);
		final PriceValue dynamicBasePrice = entry.isBundleMainEntry()? basePrice:getDynamicBasePriceForRentalExtendOrderSku(basePrice, product , defaultAddedTimeForExtendRental);
		entry.setBasePrice(Double.valueOf(dynamicBasePrice.getValue()));
		final List<DiscountValue> entryDiscounts = findDiscountValues(entry);
		entry.setDiscountValues(entryDiscounts);
		setDamageWaiverPrices(entry, product);
		getModelService().save(entry);
	}

	/**
	 * This method created to get dynamic price for extend rental based on extend rental days
	 */
	@Override
	public PriceValue getDynamicBasePriceForRentalExtendOrderSku(final PriceValue basePrice, final ProductModel product , final int defaultAddedTimeForExtendRental)
	{
		if (!PredicateUtils.instanceofPredicate(BlSerialProductModel.class).evaluate(product)
				&& PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product))
		{
			final PriceInformation dynamicPriceDataForProduct = getCommercePriceService()
					.getWebPriceForExtendProduct(product, Long.valueOf(defaultAddedTimeForExtendRental));
			return createNewPriceValue(basePrice.getCurrencyIso(), dynamicPriceDataForProduct.getPriceValue().getValue(), basePrice.isNet());
		}
		return basePrice;
	}



	/**
	 * this method Created for rental Extend Order to do calculate bases on extend rental selection
	 */
	@Override
	public void recalculateOrderForTax(final AbstractOrderModel order) throws CalculationException
	{
		try
		{
			saveOrderEntryUnneeded.set(Boolean.TRUE);
			calculateEntriesForTax(order, true);
			resetAllValues(order);
			calculateTotals(order, true, calculateSubtotal(order , true));
		}
		finally
		{
			saveOrderEntryUnneeded.remove();
		}
	}



	/**
	 * This method created to calculate entries for extend order
	 */
	@Override
	public void calculateEntriesForTax(final AbstractOrderModel order, final boolean forceRecalculate)
			throws CalculationException
	{
		double subtotal = 0.0;
		double totalDamageWaiverCost = 0.0;

		for (final AbstractOrderEntryModel entryModel : order.getEntries().stream().filter(entry -> !entry.isBundleEntry()).collect(
				Collectors.toList()))
		{
			resetAllValuesForTax(entryModel);
			super.calculateTotals(entryModel , true);
			subtotal += entryModel.getTotalPrice().doubleValue();
			totalDamageWaiverCost += getDamageWaiverPriceFromEntry(entryModel);
		}
		final Double finaltotalDamageWaiverCost = Double.valueOf(totalDamageWaiverCost);
		order.setTotalDamageWaiverCost(finaltotalDamageWaiverCost);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlCoreConstants.DAMAGE_WAIVER_ERROR, finaltotalDamageWaiverCost);
		final Double totalPriceWithDamageWaiverCost = Double.valueOf(subtotal + totalDamageWaiverCost);
		order.setTotalPrice(totalPriceWithDamageWaiverCost);
		order.setDeliveryCost(0.0);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlCoreConstants.TOTAL_PRICE, totalPriceWithDamageWaiverCost);
		getDefaultBlExternalTaxesService().calculateExternalTaxes(order);
	}

	/**
	 * This method created to reset entries for extend rental order
	 */
	@Override
	public void resetAllValuesForTax(final AbstractOrderEntryModel entry) throws CalculationException
	{
		final ProductModel product = entry.getProduct();
		final Collection<TaxValue> entryTaxes = findTaxValues(entry);
		entry.setTaxValues(entryTaxes);
		final AbstractOrderModel order = entry.getOrder();
		final PriceValue pv = getPriceForSkuOrSerial(order, entry, product);
		final PriceValue basePrice = convertPriceIfNecessary(pv, order.getNet().booleanValue(), order.getCurrency(), entryTaxes);
		final PriceValue dynamicBasePrice = ((BlProductModel)product).isBundleProduct() ? basePrice : getDynamicBasePriceForTax(basePrice, product , entry.getOrder());
		entry.setBasePrice(Double.valueOf(dynamicBasePrice.getValue()));
		final List<DiscountValue> entryDiscounts = findDiscountValues(entry);
		entry.setDiscountValues(entryDiscounts);
		setDamageWaiverPrices(entry, product);
	}

	/**
	 * This method created to get dynamic price for extend rental based on extend rental days
	 */
	@Override
	public PriceValue getDynamicBasePriceForTax(final PriceValue basePrice, final ProductModel product , final
	AbstractOrderModel abstractOrder)
	{
		if (!PredicateUtils.instanceofPredicate(BlSerialProductModel.class).evaluate(product)
				&& PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product))
		{
			final PriceInformation dynamicPriceDataForProduct = getCommercePriceService()
					.getWebPriceForTax(product , abstractOrder);
			return createNewPriceValue(basePrice.getCurrencyIso(), dynamicPriceDataForProduct.getPriceValue().getValue(), basePrice.isNet());
		}
		return basePrice;
	}

	/**
	 * This method is overridden to ignore calculation for created bundle entry.
	 *  @param order
	 *   @param recalculate
	 */
	@Override
	public Map<TaxValue, Map<Set<TaxValue>, Double>> calculateSubtotal(final AbstractOrderModel order,
																	   final boolean recalculate)
	{
		if (recalculate || orderRequiresCalculationStrategy.requiresCalculation(order))
		{
			double subtotal = 0.0;
			// entry grouping via map { tax code -> Double }  // NOSONAR
			final List<AbstractOrderEntryModel> entries = order.getEntries().stream().filter(entry-> !entry.isBundleEntry()).collect(
					Collectors.toList());
			final Map<TaxValue, Map<Set<TaxValue>, Double>> taxValueMap = new LinkedHashMap<TaxValue, Map<Set<TaxValue>, Double>>(
					entries.size() * 2);

			for (final AbstractOrderEntryModel entry : entries)
			{
				calculateTotals(entry, recalculate);
				final double entryTotal = entry.getTotalPrice().doubleValue();
				LOG.info("calculateSubtotal->entryTotal" + entryTotal);
				subtotal += entryTotal;
				// use un-applied version of tax values!!!
				final Collection<TaxValue> allTaxValues = entry.getTaxValues();
				final Set<TaxValue> relativeTaxGroupKey = getUnappliedRelativeTaxValues(allTaxValues);
				for (final TaxValue taxValue : allTaxValues)
				{
					addEntryTaxValue(taxValueMap, entry, entryTotal, relativeTaxGroupKey, taxValue);
				}
			}
			// store subtotal
			LOG.info("calculateSubtotal->subtotal->Before" + subtotal);
			subtotal = commonI18NService.roundCurrency(subtotal, order.getCurrency().getDigits().intValue());
			LOG.info("calculateSubtotal->subtotal->After" + subtotal);
			order.setSubtotal(Double.valueOf(subtotal));
			return taxValueMap;
		}
		return Collections.emptyMap();
	}

	private void addEntryTaxValue(final Map<TaxValue, Map<Set<TaxValue>, Double>> taxValueMap,
								  final AbstractOrderEntryModel entry,
								  final double entryTotal, final Set<TaxValue> relativeTaxGroupKey, final TaxValue taxValue)
	{
		if (taxValue.isAbsolute())
		{
			addAbsoluteEntryTaxValue(entry.getQuantity().longValue(), taxValue.unapply(), taxValueMap);
		}
		else
		{
			addRelativeEntryTaxValue(entryTotal, taxValue.unapply(), relativeTaxGroupKey, taxValueMap);
		}
	}

	/**
	 * @return the commercePriceService
	 */
	public BlCommercePriceService getCommercePriceService()
	{
		return commercePriceService;
	}

	/**
	 * @param commercePriceService
	 *           the commercePriceService to set
	 */
	public void setCommercePriceService(final BlCommercePriceService commercePriceService)
	{
		this.commercePriceService = commercePriceService;
	}

	/**
	 * @return the blDamageWaiverGenericDao
	 */
	public GenericDao<BlDamageWaiverPricingModel> getBlDamageWaiverGenericDao()
	{
		return blDamageWaiverGenericDao;
	}

	/**
	 * @param blDamageWaiverGenericDao
	 *           the blDamageWaiverGenericDao to set
	 */
	public void setBlDamageWaiverGenericDao(final GenericDao<BlDamageWaiverPricingModel> blDamageWaiverGenericDao)
	{
		this.blDamageWaiverGenericDao = blDamageWaiverGenericDao;
	}

	/**
	 * @return the defaultOrderRequiresCalculationStrategy
	 */
	public OrderRequiresCalculationStrategy getDefaultOrderRequiresCalculationStrategy()
	{
		return defaultOrderRequiresCalculationStrategy;
	}

	/**
	 * @param defaultOrderRequiresCalculationStrategy
	 *           the defaultOrderRequiresCalculationStrategy to set
	 */
	public void setDefaultOrderRequiresCalculationStrategy(
			final OrderRequiresCalculationStrategy defaultOrderRequiresCalculationStrategy)
	{
		this.defaultOrderRequiresCalculationStrategy = defaultOrderRequiresCalculationStrategy;
	}

	/**
	 * @return the defaultCommonI18NService
	 */
	public CommonI18NService getDefaultCommonI18NService()
	{
		return defaultCommonI18NService;
	}

	/**
	 * @param defaultCommonI18NService
	 *           the defaultCommonI18NService to set
	 */
	public void setDefaultCommonI18NService(final CommonI18NService defaultCommonI18NService)
	{
		this.defaultCommonI18NService = defaultCommonI18NService;
	}

	public DefaultBlExternalTaxesService getDefaultBlExternalTaxesService() {
		return defaultBlExternalTaxesService;
	}

	public void setDefaultBlExternalTaxesService(
			final DefaultBlExternalTaxesService defaultBlExternalTaxesService)
	{
		this.defaultBlExternalTaxesService = defaultBlExternalTaxesService;
	}

}
