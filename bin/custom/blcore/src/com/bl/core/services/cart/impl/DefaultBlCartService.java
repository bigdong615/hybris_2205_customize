package com.bl.core.services.cart.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commerceservices.order.CommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.impl.DefaultCartService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Default implementation of the {@link BlCartService}.
 *
 * @author Neeraj Singh
 */
public class DefaultBlCartService extends DefaultCartService implements BlCartService {

    private static final Logger LOGGER = Logger.getLogger(DefaultBlCartService.class);

    private CommerceCartService commerceCartService;
    private CommerceCartCalculationStrategy blCheckoutCartCalculationStrategy;
    private BlCommerceStockService blCommerceStockService;
    private BaseStoreService baseStoreService;
    private BlDatePickerService blDatePickerService;
    /**
     * {@inheritDoc}
     */
    @Override
    public void clearCartEntries() {

        final CartModel cartModel = getSessionCart();

        if (CollectionUtils.isNotEmpty(cartModel.getEntries())) {

            if (BooleanUtils.isFalse(cartModel.getIsRentalCart())) {
                setUsedGearSerialProductStatus(cartModel, null);
            }

            final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
            commerceCartParameter.setEnableHooks(true);
            commerceCartParameter.setCart(cartModel);
            getCommerceCartService().removeAllEntries(commerceCartParameter);


            BlLogger.logFormattedMessage(LOGGER, Level.DEBUG, BlCoreConstants.EMPTY_STRING,
                    "All entries removed from cart with code : {}", cartModel.getCode());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void resetCartCalculationFlag() {
        final CartModel cartModel = getSessionCart();
        if (CollectionUtils.isNotEmpty(cartModel.getEntries())) {
            cartModel.getEntries().forEach(entry -> {
                entry.setCalculated(Boolean.FALSE);
                getModelService().save(entry);
            });
        }
        cartModel.setCalculated(Boolean.FALSE);
        getModelService().save(cartModel);
        getModelService().refresh(cartModel);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void recalculateCartIfRequired() {
        final CartModel cartModel = getSessionCart();
        if (BooleanUtils.isFalse(cartModel.getCalculated())) {
            final CommerceCartParameter parameter = getCommerceCartParameter(cartModel);
            getBlCheckoutCartCalculationStrategy().calculateCart(parameter);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateCartEntryDamageWaiver(final long entryNumber, final String damageWaiverType) {
        final CartModel cartModel = getSessionCart();
        final Integer cartEntryNumber = Integer.valueOf((int) entryNumber);
        if (CollectionUtils.isNotEmpty(cartModel.getEntries())) {
            final AbstractOrderEntryModel cartEntryModel = cartModel.getEntries().stream()
                    .filter(cartEntry -> cartEntryNumber.equals(cartEntry.getEntryNumber())).findFirst().orElse(null);
            checkAndSetFlagForSelectedDamageWaiver(cartEntryModel, damageWaiverType);
            cartModel.setCalculated(Boolean.FALSE);
            getModelService().save(cartEntryModel);
            getModelService().save(cartModel);
            final CommerceCartParameter parameter = getCommerceCartParameter(cartModel);
            getBlCheckoutCartCalculationStrategy().recalculateCart(parameter);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setRentalDatesOnCart(final Date rentalStartDate, final Date rentalEndDate) {
        final CartModel cartModel = getSessionCart();
        final String cartCode = cartModel.getCode();
        cartModel.setRentalStartDate(rentalStartDate);
        cartModel.setRentalEndDate(rentalEndDate);
        try {
            getModelService().save(cartModel);
            BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Setting Rental Start Date: {} and End Date: {} on Cart: {}",
                    rentalStartDate, rentalEndDate, cartCode);
        } catch (final Exception exception) {
            BlLogger.logFormattedMessage(LOGGER, Level.ERROR, StringUtils.EMPTY, exception,
                    "Error while saving rental Start Date: {} and End Date: {} on cart - {}", rentalStartDate, rentalEndDate,
                    cartCode);
        }
    }

    /**
     * Gets the commerce cart parameter.
     *
     * @param cartModel the cart model
     * @return the commerce cart parameter
     */
    private CommerceCartParameter getCommerceCartParameter(final CartModel cartModel) {
        final CommerceCartParameter parameter = new CommerceCartParameter();
        parameter.setCart(cartModel);
        parameter.setEnableHooks(Boolean.TRUE);
        parameter.setRecalculate(true);
        return parameter;
    }

    /**
     * Check and set flag for selected damage Waiver.
     *
     * @param cartEntryModel   the cart entry model
     * @param damageWaiverType the damage Waiver type
     */
    private void checkAndSetFlagForSelectedDamageWaiver(final AbstractOrderEntryModel cartEntryModel,
                                                        final String damageWaiverType) {
        switch (damageWaiverType) {
            case BlCoreConstants.GEAR_GUARD_PRO_FULL:
                setFlags(cartEntryModel, Boolean.TRUE, Boolean.FALSE, Boolean.FALSE);
                break;
            case BlCoreConstants.GEAR_GUARD:
                setFlags(cartEntryModel, Boolean.FALSE, Boolean.TRUE, Boolean.FALSE);
                break;
            case BlCoreConstants.NO_GEAR_GUARD:
                setFlags(cartEntryModel, Boolean.FALSE, Boolean.FALSE, Boolean.TRUE);
                break;
            default:
                break;
        }
    }

    /**
     * Sets the flags for Damage Waiver.
     *
     * @param cartEntryModel                 the cart entry model
     * @param gearGuardProFullWaiverSelected the gear Guard pro full waiver selected
     * @param gearGuardWaiverSelected        the gear Guard waiver selected
     * @param noGearGuardWaiverSelected      the no gear Guard waiver selected
     */
    private void setFlags(final AbstractOrderEntryModel cartEntryModel, final Boolean gearGuardProFullWaiverSelected,
                          final Boolean gearGuardWaiverSelected, final Boolean noGearGuardWaiverSelected) {
        cartEntryModel.setGearGuardProFullWaiverSelected(gearGuardProFullWaiverSelected);
        cartEntryModel.setGearGuardWaiverSelected(gearGuardWaiverSelected);
        cartEntryModel.setNoDamageWaiverSelected(noGearGuardWaiverSelected);
        cartEntryModel.setCalculated(Boolean.FALSE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Long> getAvailabilityForRentalCart(final CartData cartData, final List<WarehouseModel> warehouses,
                                                          final RentalDateDto rentalDatesFromSession) {
        final List<String> lProductCodes = cartData.getEntries().stream().map(cartEntry -> cartEntry.getProduct().getCode())
                .collect(Collectors.toList());
        final Date lastDateToCheck = BlDateTimeUtils.getFormattedStartDay(BlDateTimeUtils.getNextYearsSameDay()).getTime();
        final List<Date> blackOutDates = getBlDatePickerService().getListOfBlackOutDates();
        final Date startDate = BlDateTimeUtils.subtractDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,
                rentalDatesFromSession.getSelectedFromDate(), blackOutDates);
        final Date endDate = BlDateTimeUtils.getRentalEndDate(blackOutDates, rentalDatesFromSession, lastDateToCheck);
        return getBlCommerceStockService().groupByProductsAvailability(startDate, endDate, lProductCodes, warehouses);
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void setUsedGearSerialProductStatus(final CartModel cartModel, final AbstractOrderEntryModel cartEntry) {
   	 if(Objects.nonNull(cartEntry) && cartEntry.getProduct() instanceof BlSerialProductModel){
   		 doChangeSerialProductStatus(cartEntry);
   	 }
   	 else if(Objects.nonNull(cartModel) && CollectionUtils.isNotEmpty(cartModel.getEntries())){
   		 for (final AbstractOrderEntryModel entry : cartModel.getEntries()) {
             if (entry.getProduct() instanceof BlSerialProductModel) {
                 doChangeSerialProductStatus(entry);
             }
   		 }
   	 }
    }

	/**
	 * Changes Serial Product Status from ADDED_TO_CART to ACTIVE status
	 * 
	 * @param entry
	 */
	private void doChangeSerialProductStatus(final AbstractOrderEntryModel entry) {
		final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) entry.getProduct();
		  if (SerialStatusEnum.ADDED_TO_CART.equals(blSerialProductModel.getSerialStatus())) {
		      blSerialProductModel.setSerialStatus(SerialStatusEnum.ACTIVE);
		      getModelService().save(blSerialProductModel);
		  }
	}
    
    


    public CommerceCartService getCommerceCartService() {
        return commerceCartService;
    }

    public void setCommerceCartService(final CommerceCartService commerceCartService) {
        this.commerceCartService = commerceCartService;
    }

    /**
     * @return blCheckoutCartCalculationStrategy
     */
    public CommerceCartCalculationStrategy getBlCheckoutCartCalculationStrategy() {
        return blCheckoutCartCalculationStrategy;
    }

    /**
     * @param blCheckoutCartCalculationStrategy
     */
    public void setBlCheckoutCartCalculationStrategy(final CommerceCartCalculationStrategy blCheckoutCartCalculationStrategy) {
        this.blCheckoutCartCalculationStrategy = blCheckoutCartCalculationStrategy;
    }

    /**
     * @return the blCommerceStockService
     */
    public BlCommerceStockService getBlCommerceStockService() {
        return blCommerceStockService;
    }

    /**
     * @param blCommerceStockService the blCommerceStockService to set
     */
    public void setBlCommerceStockService(final BlCommerceStockService blCommerceStockService) {
        this.blCommerceStockService = blCommerceStockService;
    }

    /**
     * @return the baseStoreService
     */
    public BaseStoreService getBaseStoreService() {
        return baseStoreService;
    }

    /**
     * @param baseStoreService the baseStoreService to set
     */
    public void setBaseStoreService(final BaseStoreService baseStoreService) {
        this.baseStoreService = baseStoreService;
    }

    /**
     * @return the blDatePickerService
     */
    public BlDatePickerService getBlDatePickerService() {
        return blDatePickerService;
    }

    /**
     * @param blDatePickerService the blDatePickerService to set
     */
    public void setBlDatePickerService(final BlDatePickerService blDatePickerService) {
        this.blDatePickerService = blDatePickerService;
    }

}
