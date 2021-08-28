package com.bl.core.order.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ProductTypeEnum;
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
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
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
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

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
		final PriceValue dynamicBasePrice = getDynamicBasePriceForRentalSKU(basePrice, product);
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
			for (final AbstractOrderEntryModel e : order.getEntries()) {
				recalculateOrderEntryIfNeeded(e, forceRecalculate);
				subtotal += e.getTotalPrice().doubleValue();
				if(!BlCoreConstants.AQUATECH_BRAND_ID.equals(e.getProduct().getManufacturerAID())) {
					totalDamageWaiverCost += getDamageWaiverPriceFromEntry(e);
					totalOptionCost += getTotalOptionPrice(e);
				}
			}
			if(BooleanUtils.isFalse(order.isGiftCardOrder()) && BooleanUtils.isFalse(order.getIsNewGearOrder())){
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
				//totalDamageWaiverCost
				if (BooleanUtils.isTrue(order.getIsRentalCart())) {
					totalDamageWaiverCost = Objects.nonNull(order.getTotalDamageWaiverCost())
							? order.getTotalDamageWaiverCost().doubleValue()
							: 0.0d;
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total Damage Waiver Cost : {}",
							totalDamageWaiverCost);
					totalOptionCost = Objects.nonNull(order.getTotalOptionsCost())
							? order.getTotalOptionsCost().doubleValue()
							: 0.0d;
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total Option Cost : {}", totalOptionCost);

				}
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
		final double roundedTotalDiscounts = getDefaultCommonI18NService()
				.roundCurrency(totalDiscounts, digits);
		order.setTotalDiscounts(Double.valueOf(roundedTotalDiscounts));

		// Set Delivery Cost as 0 for Extend rental order based on flag -> isExtendedOrder
		if (BooleanUtils.isTrue(order.getIsExtendedOrder())) {
			order.setDeliveryCost(0.0);
		}

		getDefaultBlExternalTaxesService().calculateExternalTaxes(order);
		// set total
		final double total = subtotal + totalDamageWaiverCost + totalOptionCost+ order.getPaymentCost().doubleValue()
				+ order.getDeliveryCost().doubleValue() - roundedTotalDiscounts + order.getTotalTax();
		final double totalRounded = getDefaultCommonI18NService().roundCurrency(total, digits);
		order.setTotalPrice(Double.valueOf(totalRounded));
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total Rounded Price : {}", totalRounded);
		final double totalRoundedTaxes = getDefaultCommonI18NService()
				.roundCurrency(order.getTotalTax(), digits);
		order.setTotalTax(Double.valueOf(totalRoundedTaxes));
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total Tax Price : {}", totalRoundedTaxes);
		setCalculatedStatus(order);
		saveOrder(order);

		// To set the current extend order in session
		if (BooleanUtils.isTrue(order.getIsExtendedOrder())) {
			BlExtendOrderUtils.setCurrentExtendOrderToSession(order);
		}
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
	private PriceValue getDynamicBasePriceForRentalSKU(final PriceValue basePrice, final ProductModel product)
	{
		
		if (!PredicateUtils.instanceofPredicate(BlSerialProductModel.class).evaluate(product) 
				&& PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product) && BooleanUtils.isFalse(
						ProductTypeEnum.GIFTCARD.equals(((BlProductModel) product).getProductType())) && BooleanUtils.isFalse(
				((BlProductModel) product).getRetailGear()))
		{
			final BlProductModel blProductModel = (BlProductModel) product;
			final BigDecimal dynamicPriceDataForProduct = getCommercePriceService()
					.getDynamicPriceDataForProduct(blProductModel.getConstrained(), Double.valueOf(basePrice.getValue()));
			return createNewPriceValue(basePrice.getCurrencyIso(), dynamicPriceDataForProduct.doubleValue(), basePrice.isNet());
		}
		return basePrice;
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
		for (final AbstractOrderEntryModel entryModel : order.getEntries())
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
		final PriceValue pv = getPriceForSkuOrSerial(order, entry, product);
		final PriceValue basePrice = convertPriceIfNecessary(pv, order.getNet().booleanValue(), order.getCurrency(), entryTaxes);
		final PriceValue dynamicBasePrice = getDynamicBasePriceForRentalExtendOrderSku(basePrice, product , defaultAddedTimeForExtendRental);
		entry.setBasePrice(Double.valueOf(dynamicBasePrice.getValue()));
		final List<DiscountValue> entryDiscounts = findDiscountValues(entry);
		entry.setDiscountValues(entryDiscounts);
		setDamageWaiverPrices(entry, product);
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
		for (final AbstractOrderEntryModel entryModel : order.getEntries())
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
		final PriceValue dynamicBasePrice = getDynamicBasePriceForTax(basePrice, product , entry.getOrder());
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
			DefaultBlExternalTaxesService defaultBlExternalTaxesService) {
		this.defaultBlExternalTaxesService = defaultBlExternalTaxesService;
	}

}
