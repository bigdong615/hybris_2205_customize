package com.bl.core.services.cart.impl;

import com.bl.core.blackout.date.dao.BlBlackoutDatesDao;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateShippingMethodEnum;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.enums.OrderTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlBlackoutDateModel;
import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import de.hybris.platform.catalog.daos.CatalogVersionDao;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commerceservices.order.CommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.order.impl.DefaultCartService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.product.daos.ProductDao;
import de.hybris.platform.promotionengineservices.dao.PromotionDao;
import de.hybris.platform.promotionengineservices.model.PromotionSourceRuleModel;
import de.hybris.platform.promotions.PromotionsService;
import de.hybris.platform.promotions.model.PromotionGroupModel;
import de.hybris.platform.promotions.result.PromotionOrderResults;
import de.hybris.platform.ruleengineservices.enums.RuleStatus;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Default implementation of the {@link BlCartService}.
 *
 * @author Neeraj Singh
 */
public class DefaultBlCartService extends DefaultCartService implements BlCartService {

	private static final Logger LOGGER = Logger.getLogger(DefaultBlCartService.class);
	
	private static final String DATE_IS_BLACKOUT_DATE = "Date : {} isBlackoutDate : {}";

    private CommerceCartService commerceCartService;
    private CommerceCartCalculationStrategy blCheckoutCartCalculationStrategy;
    private BlCommerceStockService blCommerceStockService;
    private BaseStoreService baseStoreService;
    private BlDatePickerService blDatePickerService;
    private CatalogVersionDao catalogVersionDao;
    private ProductDao productDao;
    private SearchRestrictionService searchRestrictionService;
    private BlBlackoutDatesDao blBlackoutDatesDao;
    private BlProductService productService;
    private PromotionDao promotionDao;
    private PromotionsService promotionsService;

    /**
     * {@inheritDoc}
     */
    @Override
    public void clearCartEntries (CartModel cartModel) {

        if(null == cartModel) {
            cartModel = getSessionCart();
        }
        if (CollectionUtils.isNotEmpty(cartModel.getEntries())) {

            if (BooleanUtils.isFalse(cartModel.getIsRentalCart())) {
                for(AbstractOrderEntryModel cartEntry : cartModel.getEntries()) {
                    setUsedGearSerialProductStatus(cartModel, cartEntry);
                }
            }
            cartModel.setIsNewGearOrder(false);
            cartModel.setIsRentalCart(false);
            cartModel.setRentalStartDate(null);
            cartModel.setRentalEndDate(null);
            cartModel.setGiftCardCost(0.0);
            cartModel.setGiftCardOrder(Boolean.FALSE);
            getModelService().save(cartModel);
            getModelService().refresh(cartModel);
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
     * Update cart entry with the selected option
     *
     * @param entryNumber the entry number
     * @param optionCode the optionCode
     */
    @Override
    public void updateCartEntrySelectedOption(final long entryNumber, final String optionCode){
        {
            final CartModel cartModel = getSessionCart();
            final Integer cartEntryNumber = Integer.valueOf((int) entryNumber);
            if (CollectionUtils.isNotEmpty(cartModel.getEntries())) {
                final AbstractOrderEntryModel cartEntryModel = cartModel.getEntries().stream()
                    .filter(cartEntry -> cartEntryNumber.equals(cartEntry.getEntryNumber())).findFirst().orElse(null);
                if(Objects.nonNull(cartEntryModel)){
                    setOptionOnCartEntry(cartEntryModel,optionCode);
                    cartModel.setCalculated(Boolean.FALSE);
                    getModelService().save(cartEntryModel);
                    getModelService().save(cartModel);
                    final CommerceCartParameter parameter = getCommerceCartParameter(cartModel);
                    getBlCheckoutCartCalculationStrategy().recalculateCart(parameter);
                }

            }
        }
    }
    /**
     * set Option On CartEntry
     *
     * @param AbstractOrderEntryModel the cartEntryModel
     * @param String the selectedOptionCode
     */
    private void setOptionOnCartEntry(final AbstractOrderEntryModel cartEntryModel,
    final String selectedOptionCode){
    ProductModel product = cartEntryModel.getProduct();
    if(product instanceof BlProductModel){
        BlProductModel blProductModel = (BlProductModel) product;
        List<BlOptionsModel> options = blProductModel.getOptions();
        if(CollectionUtils.isNotEmpty(options)){
            BlOptionsModel option = options.iterator().next();
            if(CollectionUtils.isNotEmpty(option.getSubOptions())){
                final Optional<BlOptionsModel> selectedSubOption = option.getSubOptions().stream()
                    .filter(subOption -> selectedOptionCode.equals(subOption.getCode())).findFirst();
                if(selectedSubOption.isPresent()){
                    final Integer quantity = Integer.parseInt(cartEntryModel.getQuantity().toString());
                    List<BlOptionsModel> selectOptionList = new ArrayList<BlOptionsModel>(quantity);
                    for(int i = 0 ; i < quantity ; i++){
                        selectOptionList.add(selectedSubOption.get());
                    }
                    cartEntryModel.setOptions(selectOptionList);
                }
            }

        }
    }
}
    /**
     * {@inheritDoc}
     */
    @Override
    public void setRentalDatesOnCart(final Date rentalStartDate, final Date rentalEndDate) {
        final CartModel cartModel = getSessionCart();
        final String cartCode = cartModel.getCode();
        if(BooleanUtils.isTrue(cartModel.getIsRentalCart())) {
            cartModel.setRentalStartDate(rentalStartDate);
            cartModel.setRentalEndDate(rentalEndDate);
            try {
                getModelService().save(cartModel);
                BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Setting Rental Start Date: {} and End Date: {} on Cart: {}",
                    rentalStartDate, rentalEndDate, cartCode);
            } catch (final Exception exception) {
                BlLogger.logFormattedMessage(LOGGER, Level.ERROR, StringUtils.EMPTY, exception, "Error while saving rental Start Date: {} and End Date: {} on cart - {}",
                    rentalStartDate, rentalEndDate,
                    cartCode);
            }
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
   	 BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, "DefaultBlCartService : getAvailabilityForRentalCart : Checking Availability for cart code : {} ", cartData.getCode());
        final List<String> lProductCodes =  cartData.getEntries().stream().filter(cartEntry -> !cartEntry.getProduct().isIsBundle())
            .map(cartEntry -> cartEntry.getProduct().getCode())
            .collect(Collectors.toList());
        BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, "Checking Cart Availability for products : {} ", lProductCodes);
        final List<ProductData> bundleProductList = cartData.getEntries().stream().filter(cartEntry -> cartEntry.getProduct().isIsBundle())
            .map(OrderEntryData::getProduct)
            .collect(Collectors.toList());
        final Date lastDateToCheck = BlDateTimeUtils.getFormattedStartDay(BlDateTimeUtils.getNextYearsSameDay()).getTime();
        BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, "last date to check : {} ", lastDateToCheck);
        final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
        BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, "blackout dates : {} ", blackOutDates);
        final Date startDate = BlDateTimeUtils.subtractDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,
                rentalDatesFromSession.getSelectedFromDate(), blackOutDates);
        BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, "Start date : {} ", startDate);
        final Date endDate = BlDateTimeUtils.getRentalEndDate(blackOutDates, rentalDatesFromSession, lastDateToCheck);
        BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, "End date : {} ", endDate);
        final Map<String, Long> stockLevelProductWise =
            CollectionUtils.isNotEmpty(lProductCodes) ? getBlCommerceStockService()
                .groupByProductsAvailability(startDate, endDate, lProductCodes, warehouses)
                : new HashMap<>();
        if(CollectionUtils.isNotEmpty(bundleProductList)){
            bundleProductList.forEach(productData -> {
                stockLevelProductWise.put(productData.getCode(),productData.getStock().getStockLevel());
            });
        }
    return stockLevelProductWise;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void setUsedGearSerialProductStatus(final CartModel cartModel, final AbstractOrderEntryModel cartEntry) {
   	 if(Objects.nonNull(cartEntry) && cartEntry.getProduct() instanceof BlSerialProductModel){
   		 doChangeSerialProductStatus(cartEntry);
   	 }
		else if (Objects.nonNull(cartModel) && CollectionUtils.isNotEmpty(cartModel.getEntries())){
			for (final AbstractOrderEntryModel entry : cartModel.getEntries())
			{
				if (entry.getProduct() instanceof BlSerialProductModel)
				{
					doChangeSerialProductStatus(entry);
				}
			}
		}
    }
    /**
     * Change gift card purchase status when remove from cart
     *
     * @param cartModel
     */
    @Override
    public void updateGiftCardPurchaseStatus(final CartModel cartModel){
      if(CollectionUtils.isEmpty(cartModel.getEntries())){
          cartModel.setGiftCardOrder(Boolean.FALSE);
          getModelService().save(cartModel);
          getModelService().refresh(cartModel);
      }
    }

    /**
     * Change new gear purchase status when remove from cart
     */
    @Override
    public void updateNewGearPurchaseStatus(final CartModel cartModel){
        if(CollectionUtils.isEmpty(cartModel.getEntries())){
            cartModel.setIsNewGearOrder(Boolean.FALSE);
            getModelService().save(cartModel);
            getModelService().refresh(cartModel);
        }
    }
    /**
     *{@inheritDoc}
     */
    @Override
    public void savePoPaymentDetails(final String poNumber, final String poNotes) {
        final CartModel cartModel = getSessionCart();
        if(cartModel != null){
            cartModel.setPoNumber(poNumber.trim());
            cartModel.setPoNotes(poNotes);
            if(cartModel.getPaymentInfo() != null){
                cartModel.setPaymentInfo(null);
            }
            getModelService().save(cartModel);
            getModelService().refresh(cartModel);
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

	/**
	   * This method created to store the PO number to order
	   */
	  @Override
	  public boolean savePoPaymentDetailsForPayBill(final String poNumber , final String poNotes , final OrderModel orderModel){

          if (null != orderModel) {
              orderModel.setPoNumber(poNumber.trim());
              orderModel.setPoNotes(poNotes);
              if (orderModel.getPaymentInfo() != null) {
                  orderModel.setPaymentInfo(null);
              }
              getModelService().save(orderModel);
              getModelService().refresh(orderModel);
              return true;
          }
          return false;
      }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateOrderTypes() {
        final CartModel cartModel = getSessionCart();
        try {
            if (isCartEligibleForSettingOrderType(cartModel)) {

                if (BooleanUtils.isFalse(cartModel.isGiftCardOrder())) {
                    if (isFrontDeskOrder(cartModel)) {

                        cartModel.setOrderType(OrderTypeEnum.FD);
                        BlLogger.logMessage(LOGGER, Level.DEBUG,
                            "Setting order type to FD for cart code {}", cartModel.getCode());
                    } else {

                        cartModel.setOrderType(OrderTypeEnum.SHIPPING);
                        BlLogger.logMessage(LOGGER, Level.DEBUG,
                            "Setting order type to SHIPPING for cart code {}", cartModel.getCode());
                    }
                }

                cartModel.setIsVipOrder(isVipOrder(cartModel));
                BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG,
                    "Setting order type VIP : {} for cart code {}",
                    cartModel.getIsVipOrder(), cartModel.getCode());
                setVerificationLevel(cartModel);
                getModelService().save(cartModel);
                getModelService().refresh(cartModel);
            }
        } catch (final Exception exception) {
            BlLogger.logMessage(LOGGER, Level.ERROR,
                "Error occurred while updating order types for cart {}", cartModel.getCode(),
                exception);
        }
    }

    /**
     * It checks, is cart eligible to set order types or vip order and verification level.
     * @param cartModel the CartModel
     * @return true false
     */
    private boolean isCartEligibleForSettingOrderType(final CartModel cartModel) {
        boolean flag = false;
        if(Objects.nonNull(cartModel)) {
            flag =  (Objects.nonNull(cartModel.getDeliveryMode())
                && Objects.nonNull(cartModel.getStore())) || (cartModel.isGiftCardOrder());
        }
        return flag;
    }

    /**
     * This method returns true if this is VIP order.
     *
     * @param cartModel
     */
    private boolean isVipOrder(final CartModel cartModel) {

        return (null != cartModel.getStore().getVipOrderThreshold()
            && cartModel.getTotalPrice() > cartModel.getStore().getVipOrderThreshold());
    }

    /**
     * This method set verification level value based on cart total and verification level range value.
     *
     * @param cartModel the CartModel
     */
    private void setVerificationLevel(final CartModel cartModel) {
        try {
            final Integer verificationLevelStartRange = cartModel.getStore()
                .getVerificationLevelStartRange();
            final Integer verificationLevelEndRange = cartModel.getStore()
                .getVerificationLevelEndRange();
            final Double cartTotalPrice = cartModel.getTotalPrice();
            if (null != verificationLevelStartRange && null != verificationLevelEndRange) {
                if (isQualifyForLevelOne(verificationLevelStartRange, verificationLevelEndRange,
                    cartTotalPrice)) {
                    cartModel.setVerificationLevel(BlCoreConstants.VERIFICATION_LEVEL_ONE);
                } else if (cartTotalPrice >= verificationLevelEndRange) {
                    cartModel.setVerificationLevel(BlCoreConstants.VERIFICATION_LEVEL_TWO);
                } else if (cartTotalPrice < verificationLevelStartRange) {
                    cartModel.setVerificationLevel(BlCoreConstants.VERIFICATION_LEVEL_ZERO);
                }
            }
        } catch (final Exception exception) {
            BlLogger.logMessage(LOGGER, Level.ERROR,
                "Error occurred while setting up verification level value on cart {}",
                cartModel.getCode(),
                exception);
        }
    }

    /**
     * It checks, cart total is eligible for verification level value 1 or not.
     * @param verificationLevelStartRange
     * @param verificationLevelEndRange
     * @param cartTotalPrice
     * @return true/false
     */
    private boolean isQualifyForLevelOne(final Integer verificationLevelStartRange,
        final Integer verificationLevelEndRange, final Double cartTotalPrice) {
        return cartTotalPrice >= verificationLevelStartRange
            && cartTotalPrice < verificationLevelEndRange;
    }

    /**
     * This method returns true if this is Front desk order.
     *
     * @param cartModel
     */
    public boolean isFrontDeskOrder(final CartModel cartModel) {

        final DeliveryModeModel deliveryModeModel = cartModel.getDeliveryMode();

        return (deliveryModeModel instanceof BlPickUpZoneDeliveryModeModel && Arrays
            .asList(BlCoreConstants.BL_SAN_CARLOS, BlCoreConstants.BL_WALTHAM)
            .contains(deliveryModeModel.getCode()));
    }

    /**
 	 * {@inheritDoc}
 	 */
 	@Override
 	public boolean isSelectedDateIsBlackoutDate(final Date dateToCheck, final BlackoutDateTypeEnum blackoutDateType)
 	{
 		BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Date to check for Blackout: {} ", dateToCheck);
 		final List<BlBlackoutDateModel> allBlackoutDatesForGivenType = getBlBlackoutDatesDao()
				.getAllBlackoutDatesForGivenType(blackoutDateType);
 		final boolean isRentalStartDate = Objects.nonNull(blackoutDateType) && BlackoutDateTypeEnum.RENTAL_START_DATE.equals(blackoutDateType);
 		if (CollectionUtils.isEmpty(allBlackoutDatesForGivenType))
 		{
 			BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, DATE_IS_BLACKOUT_DATE, dateToCheck, Boolean.FALSE);
 			return Boolean.FALSE;
 		}
 		if(isRentalStartDate)
 		{
 			allBlackoutDatesForGivenType.removeIf(blackoutDate -> !BlackoutDateShippingMethodEnum.ALL.equals(blackoutDate.getBlockedShippingMethod()));
 		}
 		final List<BlBlackoutDateModel> allHolidaysForGivenType = getBlBlackoutDatesDao().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
 		if(CollectionUtils.isNotEmpty(allHolidaysForGivenType))
 		{
 			allBlackoutDatesForGivenType.addAll(allHolidaysForGivenType);
 		}
 		for(final BlBlackoutDateModel blackoutDate : allBlackoutDatesForGivenType)
 		{
 			if (DateUtils.isSameDay(dateToCheck, blackoutDate.getBlackoutDate()))
 			{
 				BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, DATE_IS_BLACKOUT_DATE, dateToCheck, Boolean.TRUE);
 				return Boolean.TRUE;
 			}
 		}
 		BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, DATE_IS_BLACKOUT_DATE, dateToCheck, Boolean.FALSE);
 		return Boolean.FALSE;
 	}
 	
 	/**
    * @inheritDoc
    */
   @Override
 	public boolean isRentalCartOnly()
 	{
 		final CartModel cartModel = getSessionCart();
 		if(Objects.nonNull(cartModel))
 		{
 			return BooleanUtils.isTrue(cartModel.getIsRentalCart()) && BooleanUtils.isFalse(cartModel.isGiftCardOrder()) && BooleanUtils.isFalse(cartModel.getIsNewGearOrder());
 		}
 		return Boolean.FALSE;
 	}

    /**
     * @inheritDoc
     */
    @Override
    public boolean isAquatechProductsPresentInCart(final ProductModel productModel) {

        final AtomicBoolean foundAquatech = new AtomicBoolean(false);
        final CartModel cartModel = getSessionCart();
        cartModel.getEntries().forEach(entry -> {

            if (null != entry.getProduct() && productService.isAquatechProduct(productModel)
                && entry.getProduct().getCode().equalsIgnoreCase(productModel.getCode())) {

                foundAquatech.set(true);
            }
        });

        return foundAquatech.get();
    }

    /**
     * @inheritDoc
     */
    @Override
    public boolean isAquatechProductsPresentInCart() {

        final AtomicBoolean foundAquatech = new AtomicBoolean(false);
        final CartModel cartModel = getSessionCart();
        cartModel.getEntries().forEach(entry -> {

            if (null != entry.getProduct() && productService.isAquatechProduct(entry.getProduct())) {

                foundAquatech.set(true);
                return;
            }
        });

        return foundAquatech.get();
    }

    /**
     * Update promotional End date for the promotion with extended rental days
     *
     * @param updatedRentalToDate
     */
    @Override
    public void updatePromotionalEndDate(final Date updatedRentalToDate) {
        final CartModel cartModel = getSessionCart();
        if (updatedRentalToDate != null && cartModel != null) {
            final String cartCode = cartModel.getCode();
            cartModel.setRentalEndDate(updatedRentalToDate);
            BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG,"Setting Rental Cart End Date: {} on Cart: {}", cartModel.getRentalEndDate(), cartCode);
            try {
                getModelService().save(cartModel);
                getModelService().refresh(cartModel);
                BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG,
                    "Setting Rental Promotional End Date: {} on Cart: {}", updatedRentalToDate,
                    cartCode);
            } catch (final Exception exception) {
                BlLogger.logFormattedMessage(LOGGER, Level.ERROR, StringUtils.EMPTY, exception,
                    "Error while saving Rental Promotional End Date: {}  on cart - {}",
                    updatedRentalToDate,
                    cartCode);
            }
        }
    }


    /**
     * Check if the Promotion with extended days is applied to cart
     *
     * @return
     */
    @Override
    public boolean isFreeRentalDayPromoApplied() {
        final PromotionGroupModel blPromoGroup = getPromotionDao().findPromotionGroupByCode(BlCoreConstants.BL_PROMO_GROUP);
        final PromotionOrderResults promotionResults = getPromotionsService().getPromotionResults(getSessionCart());
          if (blPromoGroup != null && CollectionUtils.isNotEmpty(blPromoGroup.getPromotionSourceRules())) {
            final Date currentDate = new Date();
            final Optional<PromotionSourceRuleModel> extendedRentalDayPromotion = blPromoGroup.getPromotionSourceRules().stream().filter(promotionSourceRuleModel -> promotionSourceRuleModel.getCode().contains(BlCoreConstants.BL_EXTENDED_RENTAL_DAYS_PROMOCODE) && RuleStatus.PUBLISHED.equals(promotionSourceRuleModel.getStatus())).findAny();
            final Optional<PromotionSourceRuleModel> couponExtendedRentalDayPromotion = blPromoGroup.getPromotionSourceRules().stream().filter(promotionSourceRuleModel -> promotionSourceRuleModel.getCode().contains(BlCoreConstants.BL_EXTENDED_RENTAL_DAYS_PROMOCODE) && RuleStatus.PUBLISHED.equals(promotionSourceRuleModel.getStatus()) && promotionSourceRuleModel.getConditions().contains(BlCoreConstants.QUALIFYING_COUPONS)).findAny();
            final boolean isPromotionActiveInBackend = extendedRentalDayPromotion.isPresent()  && extendedRentalDayPromotion.get().getStartDate() != null
                && extendedRentalDayPromotion.get().getEndDate() != null && currentDate.getTime() >= extendedRentalDayPromotion.get().getStartDate().getTime() && currentDate.getTime() <= extendedRentalDayPromotion.get().getEndDate().getTime();
            final boolean isCouponPromotionActiveInBackend = couponExtendedRentalDayPromotion.isPresent()  && couponExtendedRentalDayPromotion.get().getStartDate() != null
                && couponExtendedRentalDayPromotion.get().getEndDate() != null && currentDate.getTime() >= couponExtendedRentalDayPromotion.get().getStartDate().getTime() && currentDate.getTime() <= couponExtendedRentalDayPromotion.get().getEndDate().getTime();
            final boolean orderResult = Objects.nonNull(promotionResults) ? promotionResults.getFiredOrderPromotions().stream().anyMatch( promotionResult -> promotionResult.getPromotion().getCode().equals(extendedRentalDayPromotion.get().getCode()) || promotionResult.getPromotion().getCode().equals(couponExtendedRentalDayPromotion.get().getCode())) : Boolean.FALSE;
            return  (isPromotionActiveInBackend || isCouponPromotionActiveInBackend) && orderResult;
        }
      return false;
    }


    /**
     * @inheritDoc
     */
    @Override
    public void removeEmptyCart(final CartModel cartModel){
        getModelService().remove(cartModel);
        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Removing empty cart with code:{} ", cartModel.getCode());
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

    public CatalogVersionDao getCatalogVersionDao() {
       return catalogVersionDao;
   }

   public void setCatalogVersionDao(CatalogVersionDao catalogVersionDao) {
       this.catalogVersionDao = catalogVersionDao;
   }

   /**
    * @return the productDao
    */
   public ProductDao getProductDao() {
       return productDao;
   }

   /**
    * @param productDao the productDao to set
    */
   public void setProductDao(ProductDao productDao) {
       this.productDao = productDao;
   }


   public SearchRestrictionService getSearchRestrictionService() {
       return searchRestrictionService;
   }

   public void setSearchRestrictionService(
       SearchRestrictionService searchRestrictionService) {
       this.searchRestrictionService = searchRestrictionService;
   }

	/**
	 * @return the blBlackoutDatesDao
	 */
	public BlBlackoutDatesDao getBlBlackoutDatesDao()
	{
		return blBlackoutDatesDao;
	}

	/**
	 * @param blBlackoutDatesDao the blBlackoutDatesDao to set
	 */
	public void setBlBlackoutDatesDao(BlBlackoutDatesDao blBlackoutDatesDao)
	{
		this.blBlackoutDatesDao = blBlackoutDatesDao;
	}

    public BlProductService getProductService() {
        return productService;
    }

    public void setProductService(BlProductService productService) {
        this.productService = productService;
    }

    public PromotionDao getPromotionDao() {
        return promotionDao;
    }

    public void setPromotionDao(PromotionDao promotionDao) {
        this.promotionDao = promotionDao;
    }

    public PromotionsService getPromotionsService() {
        return promotionsService;
    }

    public void setPromotionsService(PromotionsService promotionsService) {
        this.promotionsService = promotionsService;
    }
}
