package com.bl.facades.shipping.impl;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.ShippingTypeEnum;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.PartnerPickUpStoreModel;
import com.bl.core.model.ShippingGroupModel;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.giftcard.BlGiftCardFacade;
import com.bl.facades.locator.data.UPSLocatorRequestData;
import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.facades.shipping.data.BlPartnerPickUpStoreData;
import com.bl.facades.shipping.data.BlPickUpZoneDeliveryModeData;
import com.bl.facades.shipping.data.BlRushDeliveryModeData;
import com.bl.facades.shipping.data.BlShippingGroupData;
import com.bl.facades.ups.address.data.AVSResposeData;
import com.bl.integration.services.BlUPSAddressValidatorService;
import com.bl.integration.services.BlUPSLocatorService;
import com.bl.logging.BlLogger;
import com.bl.storefront.forms.BlPickUpByForm;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import de.hybris.platform.acceleratorfacades.order.impl.DefaultAcceleratorCheckoutFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import de.hybris.platform.commercefacades.order.data.ZoneDeliveryModeData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commerceservices.order.CommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * {javadoc}
 *
 * @auther Namrata Lohar
 */
public class DefaultBlCheckoutFacade extends DefaultAcceleratorCheckoutFacade implements BlCheckoutFacade {

    private static final Logger LOG = Logger.getLogger(DefaultBlCheckoutFacade.class);

    @Resource(name = "blDeliveryModeService")
    private BlDeliveryModeService blDeliveryModeService;

    @Resource(name = "blUPSLocatorService")
    private BlUPSLocatorService blUPSLocatorService;

    @Resource(name = "upsAddressValidatorService")
    BlUPSAddressValidatorService blUPSAddressValidatorService;

    private BlGiftCardFacade blGiftCardFacade;
    private BrainTreeCheckoutFacade brainTreeCheckoutFacade;
    private BlCheckoutFacade checkoutFacade;
    private CommerceCartCalculationStrategy blCheckoutCartCalculationStrategy;

    private BlDatePickerService blDatePickerService;
    private Converter<ZoneDeliveryModeModel, ZoneDeliveryModeData> blZoneDeliveryModeConverter;
    private Converter<BlPickUpZoneDeliveryModeModel, BlPickUpZoneDeliveryModeData> blPickUpZoneDeliveryModeConverter;
    private Converter<BlRushDeliveryModeModel, BlRushDeliveryModeData> blRushDeliveryModeConverter;
    private Converter<ShippingGroupModel, BlShippingGroupData> blShippingGroupConverter;
    private Converter<PartnerPickUpStoreModel, BlPartnerPickUpStoreData> blPartnerPickUpStoreConverter;

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlShippingGroupData> getAllShippingGroups() {
        return getBlShippingGroupConverter().convertAll(sortModelsOnShippingGroupType(getBlZoneDeliveryModeService().getAllShippingGroups()));
    }

    private Collection<ShippingGroupModel> sortModelsOnShippingGroupType(final Collection<ShippingGroupModel> shippingGroupModels) {
        Collection<ShippingGroupModel> allGroupModels = new ArrayList<>();

        final Collection<ShippingGroupModel> fastGroupModels = shippingGroupModels.stream().filter(model ->
                ShippingTypeEnum.FAST.getCode().equals(model.getShippingType().getCode())).collect(Collectors.toList());
        final Collection<ShippingGroupModel> fasterGroupModels = shippingGroupModels.stream().filter(model ->
                ShippingTypeEnum.FASTER.getCode().equals(model.getShippingType().getCode())).collect(Collectors.toList());
        final Collection<ShippingGroupModel> fastestGroupModels = shippingGroupModels.stream().filter(model ->
                ShippingTypeEnum.FASTEST.getCode().equals(model.getShippingType().getCode())).collect(Collectors.toList());

        allGroupModels.addAll(CollectionUtils.isNotEmpty(fastGroupModels) ? fastGroupModels.stream().sorted(
                Comparator.comparing(ShippingGroupModel::isDefaultShippingGroup).reversed()).collect(Collectors.toList())
                : Collections.emptyList());
        allGroupModels.addAll(CollectionUtils.isNotEmpty(fasterGroupModels) ? fasterGroupModels.stream().sorted(
                Comparator.comparing(ShippingGroupModel::isDefaultShippingGroup).reversed()).collect(Collectors.toList())
                : Collections.emptyList());
        allGroupModels.addAll(CollectionUtils.isNotEmpty(fastestGroupModels) ? fastestGroupModels.stream().sorted(
                Comparator.comparing(ShippingGroupModel::isDefaultShippingGroup).reversed()).collect(Collectors.toList())
                : Collections.emptyList());

        return allGroupModels;
    }

    @Override
    protected AddressModel createDeliveryAddressModel(final AddressData addressData, final CartModel cartModel)
    {
        final AddressModel addressModel = getModelService().create(AddressModel.class);
        getAddressReversePopulator().populate(addressData, addressModel);
        addressModel.setOwner(cartModel);
        getModelService().save(addressModel);
        getModelService().refresh(addressModel);
        return addressModel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<? extends DeliveryModeData> getSupportedDeliveryModes(final String shippingGroup, final String partnerZone,
                                                                            final boolean payByCustomer) {
        final CartModel cartModel = getCart();
        if (cartModel != null && shippingGroup != null) {
            if (getRentalStartDate() != null && getRentalEndDate() != null) {
                return getDeliveryModeData(shippingGroup, partnerZone, getRentalStartDate(), getRentalEndDate(), payByCustomer);
            } else {
                return Collections.emptyList();
            }
        }
        return Collections.emptyList();
    }

    /**
     * This method will call appropriate method according to shipping group
     *
     * @param shippingGroup name
     * @param partnerZone   if bl-partner-pickup
     * @param rentalStart   date
     * @param rentalEnd     date
     * @return Collection of delivery mode data
     */
    private Collection<? extends DeliveryModeData> getDeliveryModeData(final String shippingGroup, final String partnerZone,
                                                                       final String rentalStart, final String rentalEnd,
                                                                       final boolean payByCustomer) {
        if (BlDeliveryModeLoggingConstants.SHIP_HOME_HOTEL_BUSINESS.equals(shippingGroup)) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.SHIP_HOME_HOTEL_BUSINESS_MSG);
            return getAllShipToHomeDeliveryModes(rentalStart, rentalEnd, payByCustomer);
        } else if (BlDeliveryModeLoggingConstants.BL_PARTNER_PICKUP.equals(shippingGroup) && null != partnerZone) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.BL_PARTNER_PICKUP_MSG);
            return getPartnerZoneDeliveryModes(partnerZone, rentalStart, rentalEnd, payByCustomer);
        } else if (BlDeliveryModeLoggingConstants.SHIP_HOLD_UPS_OFFICE.equals(shippingGroup)) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.SHIP_HOLD_UPS_OFFICE_MSG);
            return getAllUSPStoreDeliveryModes(rentalStart, rentalEnd, payByCustomer);
        } else if (BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY.equals(shippingGroup)) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY_MSG);
            return getBlZoneDeliveryModeService().checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(
                    BlDeliveryModeLoggingConstants.ZONE_PST, new Date()), rentalStart) == BlInventoryScanLoggingConstants.ZERO
                    ? getBlRushDeliveryModes(BlDeliveryModeLoggingConstants.SF, BlDateTimeUtils.getCurrentTimeUsingCalendar(
                    BlDeliveryModeLoggingConstants.ZONE_PST), payByCustomer) : getBlRushDeliveryModes(BlDeliveryModeLoggingConstants.SF,
                    null, payByCustomer);
        } else if (BlDeliveryModeLoggingConstants.NEXT_DAY_RUSH_DELIVERY.equals(shippingGroup)) {
            return getBlRushDeliveryModeData(rentalStart, payByCustomer);
        } else {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.DEFAULT_DELIVERY_MSG);
            return getAllShipToHomeDeliveryModes(rentalStart, rentalEnd, payByCustomer);
        }
    }

    /**
     *
     * @param rentalStart date
     * @param payByCustomer flag
     * @return collection of RushDeliveryData
     */
    private Collection<BlRushDeliveryModeData> getBlRushDeliveryModeData(String rentalStart, boolean payByCustomer) {
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.NEXT_DAY_RUSH_DELIVERY_MSG);
        final int result = getBlZoneDeliveryModeService().checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(
                BlDeliveryModeLoggingConstants.ZONE_EST, new Date()), rentalStart);
        if(result == BlInventoryScanLoggingConstants.ONE) {
            final Collection<BlRushDeliveryModeData> blRushDeliveryModeData = getBlRushDeliveryModes(BlDeliveryModeLoggingConstants.NYC,
                    BlDateTimeUtils.getCurrentTimeUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_EST), payByCustomer);
            return CollectionUtils.isNotEmpty(blRushDeliveryModeData) ? blRushDeliveryModeData.stream().filter(mode ->
                    !mode.getCode().equals(BlDeliveryModeLoggingConstants.RUSH_NYC_NEXT_DAY_9_To_12)).collect(Collectors.toList())
                    : Collections.emptyList();
        } else if(result >= BlInventoryScanLoggingConstants.TWO) {
            return getBlRushDeliveryModes(BlDeliveryModeLoggingConstants.NYC,null, payByCustomer);
        } else {
            return Collections.emptyList();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ZoneDeliveryModeData> getAllShipToHomeDeliveryModes(final String rentalStart, final String rentalEnd,
                                                                          final boolean payByCustomer) {
        final Collection<ZoneDeliveryModeModel> deliveryModeModels = getBlZoneDeliveryModeService()
                .getAllShipToHomeDeliveryModesWithRentalDates(rentalStart, rentalEnd, payByCustomer);
        if (CollectionUtils.isNotEmpty(deliveryModeModels)) {
            final Collection<ZoneDeliveryModeData> resultDeliveryData = new ArrayList<>();
            for (ZoneDeliveryModeModel zoneDeliveryModeModel : deliveryModeModels) {
                final ZoneDeliveryModeData zoneDeliveryModeData = getZoneDeliveryModeConverter().convert(zoneDeliveryModeModel);
                if (null != zoneDeliveryModeData) {
                    zoneDeliveryModeData.setDeliveryCost(getPriceDataFactory().create(PriceDataType.BUY, BigDecimal.valueOf(
                            getBlZoneDeliveryModeService().getAmountForAppropriateZoneModel((AbstractOrderModel) getCart(), zoneDeliveryModeModel)),
                            getCart().getCurrency().getIsocode()));
                    resultDeliveryData.add(zoneDeliveryModeData);
                }
            }
            return resultDeliveryData;
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     *
     * @return
     */
    @Override
    public Collection<BlPartnerPickUpStoreData> getAllPartnerPickUpStore() {
        return getBlPartnerPickUpStoreConverter().convertAll(getBlZoneDeliveryModeService().getAllPartnerPickUpStore());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeData> getAllUSPStoreDeliveryModes(final String rentalStart, final String rentalEnd,
                                                                                final boolean payByCustomer) {
        final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeService()
                .getAllPartnerPickUpDeliveryModesWithRentalDatesForUPSStore(rentalStart, rentalEnd, payByCustomer);
        if (CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels)) {
            final Collection<BlPickUpZoneDeliveryModeData> resultDeliveryData = new ArrayList<>();
            for (BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel : blPickUpZoneDeliveryModeModels) {
                getResultantPartnerDeliveryMethods(resultDeliveryData, blPickUpZoneDeliveryModeModel);
            }
            return resultDeliveryData;
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeData> getPartnerZoneDeliveryModes(final String partnerZone, final String rentalStart,
                                                                                final String rentalEnd, final boolean payByCustomer) {
        final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels;
        try {
            blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeService().getPartnerZoneDeliveryModes(
                    partnerZone, rentalStart, rentalEnd, payByCustomer);
            if (CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels)) {
                final Collection<BlPickUpZoneDeliveryModeData> resultDeliveryData = new ArrayList<>();
                for (BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel : blPickUpZoneDeliveryModeModels) {
                    getResultantPartnerDeliveryMethods(resultDeliveryData, blPickUpZoneDeliveryModeModel);
                }
                return resultDeliveryData;
            }
        } catch (ParseException e) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Exception while parsing dates ", e);
        }
        return Collections.emptyList();
    }

    /**
     * This method will get the partner delivery methods
     *
     * @param resultDeliveryData            resultant list
     * @param blPickUpZoneDeliveryModeModel model
     */
    private void getResultantPartnerDeliveryMethods(final Collection<BlPickUpZoneDeliveryModeData> resultDeliveryData,
                                                    final BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel) {
        final BlPickUpZoneDeliveryModeData zoneDeliveryModeData = getBlPickUpZoneDeliveryModeConverter().convert(blPickUpZoneDeliveryModeModel);
        if (null != zoneDeliveryModeData) {
            zoneDeliveryModeData.setDeliveryCost(getPriceDataFactory().create(PriceDataType.BUY, BigDecimal.valueOf(
                    getBlZoneDeliveryModeService().getAmountForAppropriateZoneModel((AbstractOrderModel) getCart(), blPickUpZoneDeliveryModeModel)),
                    getCart().getCurrency().getIsocode()));
            resultDeliveryData.add(zoneDeliveryModeData);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlRushDeliveryModeData> getBlRushDeliveryModes(final String deliveryMode, final String pstCutOffTime,
                                                                     final boolean payByCustomer) {

        final Collection<BlRushDeliveryModeModel> blRushDeliveryModeModels = getBlZoneDeliveryModeService().getBlRushDeliveryModes(
                deliveryMode, pstCutOffTime, getRentalStartDate(), getRentalEndDate(), payByCustomer);
        if (CollectionUtils.isNotEmpty(blRushDeliveryModeModels)) {
            final Collection<BlRushDeliveryModeData> resultDeliveryData = new ArrayList<>();
            for (BlRushDeliveryModeModel blRushDeliveryModeModel : blRushDeliveryModeModels) {
                final BlRushDeliveryModeData blRushDeliveryModeData = getBlRushDeliveryModeConverter().convert(blRushDeliveryModeModel);
                if (null != blRushDeliveryModeData) {
                    blRushDeliveryModeData.setDeliveryCost(getPriceDataFactory().create(PriceDataType.BUY, BigDecimal.valueOf(
                            getBlZoneDeliveryModeService().getAmountForAppropriateZoneModel((AbstractOrderModel) getCart(), blRushDeliveryModeModel)),
                            getCart().getCurrency().getIsocode()));
                    resultDeliveryData.add(blRushDeliveryModeData);
                }
            }
            return resultDeliveryData;
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean setDeliveryMode(final String deliveryMethod, final boolean internalStoreAddress) {
        final CartModel cartModel = getCart();
        if (cartModel != null) {
            final DeliveryModeModel deliveryModeModel = getDeliveryService().getDeliveryModeForCode(deliveryMethod);
            if (deliveryModeModel != null) {
                setPartnerInternalAddressAsDeliveryAddress(internalStoreAddress, cartModel, deliveryModeModel);
                final CommerceCheckoutParameter parameter = createCommerceCheckoutParameter(cartModel, true);
                parameter.setDeliveryMode(deliveryModeModel);
                return getCommerceCheckoutService().setDeliveryMode(parameter);
            }
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean checkSFOrNYCPinCodeValidity(final String pinCode, final String deliveryType) {
        return getBlZoneDeliveryModeService().checkSFOrNYCPinCodeValidity(pinCode, deliveryType);
    }

    /**
     *
     * @param internalStoreAddress address
     * @param cartModel cart
     * @param deliveryModeModel delivery method
     */
    private void setPartnerInternalAddressAsDeliveryAddress(final boolean internalStoreAddress, final CartModel cartModel,
                                                            final DeliveryModeModel deliveryModeModel) {
        if (internalStoreAddress && deliveryModeModel instanceof BlPickUpZoneDeliveryModeModel) {
            final BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) deliveryModeModel;
            final AddressModel addressModel = blPickUpZoneDeliveryModeModel.getInternalStoreAddress();
            if (addressModel != null) {
                addressModel.setPickStoreAddress(Boolean.TRUE);
                if(cartModel.isPickUpByMe()) {
                    cartModel.setPickUpPersonPhone(addressModel.getPhone1());
                    cartModel.setPickUpPersonEmail(addressModel.getEmail());
                    cartModel.setPickUpPersonFirstName(addressModel.getFirstname());
                    cartModel.setPickUpPersonLastName(addressModel.getLastname());
                }
                getModelService().save(addressModel);
                getModelService().refresh(addressModel);
                setUPSAddressOnCart(addressModel);
            }
        }
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void setUPSAddressOnCart(final AddressModel addressModel) {
        final CartModel cartModel = getCart();
        if (cartModel != null) {
            cartModel.setDeliveryAddress(addressModel);
            getModelService().save(cartModel);
            getModelService().refresh(cartModel);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setUPSAddressOnCartForIam(final AddressData address) {
        final CartModel cartModel = getCart();
        try {
            if (cartModel != null && address != null && cartModel.isPickUpByMe()) {
                cartModel.setPickUpPersonFirstName(address.getFirstName());
                cartModel.setPickUpPersonLastName(address.getLastName());
                cartModel.setPickUpPersonEmail(address.getEmail());
                cartModel.setPickUpPersonPhone(address.getPhone());
                getModelService().save(cartModel);
                getModelService().refresh(cartModel);
            }
        } catch (Exception e) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Exception while saving UPS pickUpByMe details", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String savePickUpInfoOnCart(final BlPickUpByForm blPickUpByForm) {
        final CartModel cartModel = getCart();
        try {
            if (cartModel != null && blPickUpByForm != null) {
                cartModel.setPickUpPersonFirstName(blPickUpByForm.getFirstName());
                cartModel.setPickUpPersonLastName(blPickUpByForm.getLastName());
                cartModel.setPickUpPersonEmail(blPickUpByForm.getEmail());
                cartModel.setPickUpPersonPhone(blPickUpByForm.getPhone());
                cartModel.setPickUpByMe(blPickUpByForm.getFirstName() != null ? Boolean.FALSE : Boolean.TRUE);
                getModelService().save(cartModel);
                getModelService().refresh(cartModel);
            }
            return BlFacadesConstants.RESULT_SUCCESS;
        } catch (Exception e) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Exception while saving pickUpBySomeone details", e);
            return "FAILURE";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String setDeliveryDetails(final String deliveryNote, final boolean statusUpdate) {
        final CartModel cartModel = getCart();
        try {
            if (cartModel != null) {
                cartModel.setDeliveryNotes(deliveryNote);
                cartModel.setStatusUpdate(statusUpdate);
                getModelService().save(cartModel);
                getModelService().refresh(cartModel);
            }
            return BlFacadesConstants.RESULT_SUCCESS;
        } catch (Exception e) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Exception while saving pickUpBySomeone details", e);
            return "FAILURE";
        }
    }

    @Override
    public boolean hasNoDeliveryAddress()
    {
        final CartData cartData = getCheckoutCart();
        return hasShippingItems() && (cartData == null || cartData.getDeliveryAddress() == null);
    }

    @Override
    public CartData getCheckoutCart()
    {
        final CartData cartData = getCartFacade().getSessionCart();
        if (cartData != null)
        {
            cartData.setDeliveryMode(getDeliveryMode());
            cartData.setPaymentInfo(getPaymentDetails());
        }
        return cartData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String removeDeliveryDetails() {
        final CartModel cartModel = getCart();
        if (cartModel != null) {
            cartModel.setDeliveryAddress(null);
            cartModel.setDeliveryCost(null);
            cartModel.setDeliveryMode(null);
            cartModel.setPickUpByMe(Boolean.TRUE);
            cartModel.setPickUpPersonFirstName(null);
            cartModel.setPickUpPersonLastName(null);
            cartModel.setPickUpPersonEmail(null);
            cartModel.setPickUpPersonPhone(null);
            cartModel.setDeliveryNotes(null);
            cartModel.setStatusUpdate(Boolean.FALSE);
            getModelService().save(cartModel);
            getModelService().refresh(cartModel);
            getModelService().save(cartModel);
            getModelService().refresh(cartModel);
            return BlFacadesConstants.RESULT_SUCCESS;
        }
        return "ERROR";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AVSResposeData getAVSResponse(final AddressData addressData) {
        return getBlUPSAddressValidatorService().getVerifiedAddress(addressData);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public UpsLocatorResposeData checkPartnerPickCodeValidity(final String pinCode) {
        final UPSLocatorRequestData upsLocatorRequestData = new UPSLocatorRequestData();
        upsLocatorRequestData.setZipcode(pinCode);
        return getBlUPSLocatorService().provideUPSLocation(upsLocatorRequestData);
    }

    /**
     * This method will return rental start date from session
     *
     * @return rental start date
     */
    public String getRentalStartDate() {
        return getBlDatePickerService().getRentalDatesFromSession() != null ? getBlDatePickerService().getRentalDatesFromSession()
                .getSelectedFromDate() : null;
    }

    /**
     * This method will return rental end date from session
     *
     * @return rental end date
     */
    public String getRentalEndDate() {
        return getBlDatePickerService().getRentalDatesFromSession() != null ? getBlDatePickerService().getRentalDatesFromSession()
                .getSelectedToDate() : null;
    }

    
 	/**
 	 * {@inheritDoc}
 	 */

 	@Override
 	public Collection<ZoneDeliveryModeModel> getAllBlDeliveryModes()
 	{
 		return getBlZoneDeliveryModeService().getAllBlDeliveryModes();
 	}
 	
 	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean checkAvailabilityForDeliveryMode(final String deliveryModeCode)
	{
		final DeliveryModeModel deliveryModeModel = getDeliveryService().getDeliveryModeForCode(deliveryModeCode);
		return Objects.nonNull(deliveryModeModel) && deliveryModeModel instanceof ZoneDeliveryModeModel
				? getBlZoneDeliveryModeService().checkCartEntriesAvailability(getRentalStartDate(), getRentalEndDate(),
						(ZoneDeliveryModeModel) deliveryModeModel)
				: Boolean.FALSE;
	}

	/**
   * {@inheritDoc}
   */
	 public List<String> recalculateCartForGiftCard() {
        if (getBrainTreeCheckoutFacade().getCartService() != null) {
            final CartModel cartModel = getBrainTreeCheckoutFacade().getCartService()
                .getSessionCart();
            List<GiftCardModel> giftCardModelList = cartModel.getGiftCard();
            if (CollectionUtils.isNotEmpty(giftCardModelList)) {
                List<String> giftCardCodeList = new ArrayList<>();
                final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
                commerceCartParameter.setCart(cartModel);
                commerceCartParameter.setBaseSite(cartModel.getSite());
                commerceCartParameter.setEnableHooks(true);
                commerceCartParameter.setRecalculate(true);
                getBlCheckoutCartCalculationStrategy().calculateCart(commerceCartParameter);
                return getRemovedGiftCardCodes(cartModel, giftCardModelList, giftCardCodeList);
            }
        }
        return Collections.emptyList();
	 }

    /**
     * It removes applied gift card and returns all the gift card codes, which has been removed.
     *
     * @param cartModel
     * @param giftCardModelList
     * @param giftCardCodeList
     * @return list of gift cards.
     */
    private List<String> getRemovedGiftCardCodes(final CartModel cartModel,
        final List<GiftCardModel> giftCardModelList, final List<String> giftCardCodeList) {
        List<GiftCardMovementModel> giftCardMovementModelList;
        for (GiftCardModel giftCardModel : giftCardModelList) {
            giftCardMovementModelList = giftCardModel.getMovements();
            if (getCheckoutFacade().isCommittedMovement(giftCardMovementModelList)
                && giftCardModel.getBalance() == 0) {
                String removedGiftCardCode = getCheckoutFacade()
                    .removeGiftCardFromCart(giftCardModel.getCode(), cartModel);
                if (StringUtils.isNotEmpty(removedGiftCardCode)) {
                    giftCardCodeList.add(removedGiftCardCode);
                }
            }
        }
        return giftCardCodeList;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isCommittedMovement(
        final List<GiftCardMovementModel> giftCardMovementModelList) {
        boolean committedMovement = true;
        if (CollectionUtils.isNotEmpty(giftCardMovementModelList)) {
            for (GiftCardMovementModel giftCardMovementModel : giftCardMovementModelList) {
                if (Boolean.FALSE.equals(giftCardMovementModel.getCommitted())) {
                    committedMovement = false;
                }
            }
        }
        return committedMovement;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String removeGiftCardFromCart(final String giftCardCode, final CartModel cartModel) {
        try {
            getBlGiftCardFacade()
                .removeGiftCard(giftCardCode, cartModel);
            return giftCardCode;
        } catch (final Exception exception) {
            BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
                "Error while removing applied gift card code: {} from cart: {} for the customer: {}",
                giftCardCode, cartModel.getCode(), cartModel.getUser().getUid(), exception);
            return null;
        }
    }

    public BlDeliveryModeService getBlZoneDeliveryModeService() {
        return blDeliveryModeService;
    }

    public void setBlZoneDeliveryModeService(BlDeliveryModeService blDeliveryModeService) {
        this.blDeliveryModeService = blDeliveryModeService;
    }

    public Converter<ZoneDeliveryModeModel, ZoneDeliveryModeData> getBlZoneDeliveryModeConverter() {
        return blZoneDeliveryModeConverter;
    }

    public void setBlZoneDeliveryModeConverter(Converter<ZoneDeliveryModeModel, ZoneDeliveryModeData> blZoneDeliveryModeConverter) {
        this.blZoneDeliveryModeConverter = blZoneDeliveryModeConverter;
    }

    public Converter<BlPickUpZoneDeliveryModeModel, BlPickUpZoneDeliveryModeData> getBlPickUpZoneDeliveryModeConverter() {
        return blPickUpZoneDeliveryModeConverter;
    }

    public void setBlPickUpZoneDeliveryModeConverter(Converter<BlPickUpZoneDeliveryModeModel, BlPickUpZoneDeliveryModeData> blPickUpZoneDeliveryModeConverter) {
        this.blPickUpZoneDeliveryModeConverter = blPickUpZoneDeliveryModeConverter;
    }

    public Converter<BlRushDeliveryModeModel, BlRushDeliveryModeData> getBlRushDeliveryModeConverter() {
        return blRushDeliveryModeConverter;
    }

    public void setBlRushDeliveryModeConverter(Converter<BlRushDeliveryModeModel, BlRushDeliveryModeData> blRushDeliveryModeConverter) {
        this.blRushDeliveryModeConverter = blRushDeliveryModeConverter;
    }

    public Converter<ShippingGroupModel, BlShippingGroupData> getBlShippingGroupConverter() {
        return blShippingGroupConverter;
    }

    public void setBlShippingGroupConverter(Converter<ShippingGroupModel, BlShippingGroupData> blShippingGroupConverter) {
        this.blShippingGroupConverter = blShippingGroupConverter;
    }

    public Converter<PartnerPickUpStoreModel, BlPartnerPickUpStoreData> getBlPartnerPickUpStoreConverter() {
        return blPartnerPickUpStoreConverter;
    }

    public void setBlPartnerPickUpStoreConverter(Converter<PartnerPickUpStoreModel, BlPartnerPickUpStoreData> blPartnerPickUpStoreConverter) {
        this.blPartnerPickUpStoreConverter = blPartnerPickUpStoreConverter;
    }

    public BlDatePickerService getBlDatePickerService() {
        return blDatePickerService;
    }

    public void setBlDatePickerService(BlDatePickerService blDatePickerService) {
        this.blDatePickerService = blDatePickerService;
    }

    public BlUPSLocatorService getBlUPSLocatorService() {
        return blUPSLocatorService;
    }

    public void setBlUPSLocatorService(BlUPSLocatorService blUPSLocatorService) {
        this.blUPSLocatorService = blUPSLocatorService;
    }

    public BlUPSAddressValidatorService getBlUPSAddressValidatorService() {
        return blUPSAddressValidatorService;
    }

    public void setBlUPSAddressValidatorService(BlUPSAddressValidatorService blUPSAddressValidatorService) {
        this.blUPSAddressValidatorService = blUPSAddressValidatorService;
    }

    public BlGiftCardFacade getBlGiftCardFacade() {
        return blGiftCardFacade;
    }

    public void setBlGiftCardFacade(BlGiftCardFacade blGiftCardFacade) {
        this.blGiftCardFacade = blGiftCardFacade;
    }

    public BrainTreeCheckoutFacade getBrainTreeCheckoutFacade() {
        return brainTreeCheckoutFacade;
    }

    public void setBrainTreeCheckoutFacade(
        BrainTreeCheckoutFacade brainTreeCheckoutFacade) {
        this.brainTreeCheckoutFacade = brainTreeCheckoutFacade;
    }

    public BlCheckoutFacade getCheckoutFacade() {
        return checkoutFacade;
    }

    public void setCheckoutFacade(BlCheckoutFacade checkoutFacade) {
        this.checkoutFacade = checkoutFacade;
    }

    public CommerceCartCalculationStrategy getBlCheckoutCartCalculationStrategy() {
        return blCheckoutCartCalculationStrategy;
    }

    public void setBlCheckoutCartCalculationStrategy(
        CommerceCartCalculationStrategy blCheckoutCartCalculationStrategy) {
        this.blCheckoutCartCalculationStrategy = blCheckoutCartCalculationStrategy;
    }
}
