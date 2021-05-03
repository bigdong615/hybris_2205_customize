package com.bl.facades.shipping.impl;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.*;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.facades.shipping.data.BlPartnerPickUpStoreData;
import com.bl.facades.shipping.data.BlPickUpZoneDeliveryModeData;
import com.bl.facades.shipping.data.BlRushDeliveryModeData;
import com.bl.facades.shipping.data.BlShippingGroupData;
import com.bl.logging.BlLogger;
import com.bl.storefront.forms.BlPickUpByForm;
import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import de.hybris.platform.commercefacades.order.data.ZoneDeliveryModeData;
import de.hybris.platform.commercefacades.order.impl.DefaultCheckoutFacade;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;

/**
 * {javadoc}
 *
 * @auther Namrata Lohar
 */
public class DefaultBlCheckoutFacade extends DefaultCheckoutFacade implements BlCheckoutFacade {

    private static final Logger LOG = Logger.getLogger(DefaultBlCheckoutFacade.class);

    @Resource(name = "blDeliveryModeService")
    private BlDeliveryModeService blDeliveryModeService;

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
        return getBlShippingGroupConverter().convertAll(getBlZoneDeliveryModeService().getAllShippingGroups());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<? extends DeliveryModeData> getSupportedDeliveryModes(final String shippingGroup, final String partnerZone,
                                                                            final String pinCode) {
        final CartModel cartModel = getCart();
        if (cartModel != null && shippingGroup != null) {
            if(getRentalStartDate() != null && getRentalEndDate() != null) {
                return getDeliveryModeData(shippingGroup, partnerZone, pinCode, getRentalStartDate(), getRentalEndDate());
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
     * @param partnerZone if bl-partner-pickup
     * @param pinCode for rush delivery
     * @param rentalStart date
     * @param rentalEnd date
     * @return Collection of delivery mode data
     */
    private Collection<? extends DeliveryModeData> getDeliveryModeData(final String shippingGroup, final String partnerZone,
                                                                       final String pinCode, final String rentalStart,
                                                                       final String rentalEnd) {
        if (BlDeliveryModeLoggingConstants.SHIP_HOME_HOTEL_BUSINESS.equals(shippingGroup)) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.SHIP_HOME_HOTEL_BUSINESS_MSG);
            return getAllShipToHomeDeliveryModes(rentalStart, rentalEnd);
        } else if (BlDeliveryModeLoggingConstants.BL_PARTNER_PICKUP.equals(shippingGroup) && null != partnerZone) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.BL_PARTNER_PICKUP_MSG);
            return getPartnerZoneDeliveryModes(partnerZone, rentalStart, rentalEnd);
        } else if (BlDeliveryModeLoggingConstants.SHIP_HOLD_UPS_OFFICE.equals(shippingGroup)) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.SHIP_HOLD_UPS_OFFICE_MSG);
            return getAllUSPStoreDeliveryModes(rentalStart, rentalEnd);
        } else if (BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY.equals(shippingGroup) && null != pinCode) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY_MSG);
            return getBlRushDeliveryModes(pinCode, BlDeliveryModeLoggingConstants.SF, BlDateTimeUtils.getCurrentTimeUsingCalendar(
                    BlDeliveryModeLoggingConstants.ZONE_PST));
        } else if (BlDeliveryModeLoggingConstants.NEXT_DAY_RUSH_DELIVERY.equals(shippingGroup) && null != pinCode) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.NEXT_DAY_RUSH_DELIVERY_MSG);
            return getBlRushDeliveryModes(pinCode, BlDeliveryModeLoggingConstants.NYC, BlDateTimeUtils.getCurrentTimeUsingCalendar(
                    BlDeliveryModeLoggingConstants.ZONE_EST));
        } else {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlDeliveryModeLoggingConstants.DEFAULT_DELIVERY_MSG);
            return getAllShipToHomeDeliveryModes(rentalStart, rentalEnd);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ZoneDeliveryModeData> getAllShipToHomeDeliveryModes(final String rentalStart,
                                                                                final String rentalEnd) {
        final Collection<ZoneDeliveryModeModel> deliveryModeModels = getBlZoneDeliveryModeService()
                .getAllShipToHomeDeliveryModesWithRentalDates(rentalStart, rentalEnd);
        if(CollectionUtils.isNotEmpty(deliveryModeModels)) {
            final Collection<ZoneDeliveryModeData> resultDeliveryData = new ArrayList<>();
            for (ZoneDeliveryModeModel zoneDeliveryModeModel : deliveryModeModels) {
                final ZoneDeliveryModeData zoneDeliveryModeData = getZoneDeliveryModeConverter().convert(zoneDeliveryModeModel);
                if(null != zoneDeliveryModeData) {
                    zoneDeliveryModeData.setDeliveryCost(getPriceDataFactory().create(PriceDataType.BUY, BigDecimal.valueOf(
                            getBlZoneDeliveryModeService().getAmountForAppropriateZoneModel(null, getCart(), zoneDeliveryModeModel)),
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
    public Collection<BlPickUpZoneDeliveryModeData> getAllUSPStoreDeliveryModes(final String rentalStart, final String rentalEnd) {
        final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeService()
                .getAllPartnerPickUpDeliveryModesWithRentalDatesForUPSStore(rentalStart, rentalEnd);
        if(CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels)) {
            final Collection<BlPickUpZoneDeliveryModeData> resultDeliveryData = new ArrayList<>();
            for(BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel : blPickUpZoneDeliveryModeModels) {
                final BlPickUpZoneDeliveryModeData zoneDeliveryModeData = getBlPickUpZoneDeliveryModeConverter().convert(blPickUpZoneDeliveryModeModel);
                if(null != zoneDeliveryModeData) {
                    zoneDeliveryModeData.setDeliveryCost(getPriceDataFactory().create(PriceDataType.BUY, BigDecimal.valueOf(
                            getBlZoneDeliveryModeService().getAmountForAppropriateZoneModel(null, getCart(), blPickUpZoneDeliveryModeModel)),
                            getCart().getCurrency().getIsocode()));
                    resultDeliveryData.add(zoneDeliveryModeData);
                }
            }
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeData> getPartnerZoneDeliveryModes(final String partnerZone, final String rentalStart,
                                                                              final String rentalEnd) {
        final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeService().getPartnerZoneDeliveryModes(
                partnerZone, rentalStart, rentalEnd);
        if(CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels)) {
            final Collection<BlPickUpZoneDeliveryModeData> resultDeliveryData = new ArrayList<>();
            for(BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel : blPickUpZoneDeliveryModeModels) {
                final BlPickUpZoneDeliveryModeData zoneDeliveryModeData = getBlPickUpZoneDeliveryModeConverter().convert(blPickUpZoneDeliveryModeModel);
                if(null != zoneDeliveryModeData) {
                    zoneDeliveryModeData.setDeliveryCost(getPriceDataFactory().create(PriceDataType.BUY, BigDecimal.valueOf(
                            getBlZoneDeliveryModeService().getAmountForAppropriateZoneModel(null, getCart(), blPickUpZoneDeliveryModeModel)),
                            getCart().getCurrency().getIsocode()));
                    resultDeliveryData.add(zoneDeliveryModeData);
                }
            }
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlRushDeliveryModeData> getBlRushDeliveryModes(final String pinCode, final String deliveryMode,
                                                                         final String pstCutOffTime) {

        final Collection<BlRushDeliveryModeModel> blRushDeliveryModeModels = getBlZoneDeliveryModeService().getBlRushDeliveryModes(
                pinCode, deliveryMode, pstCutOffTime);
        if(CollectionUtils.isNotEmpty(blRushDeliveryModeModels)) {
            final Collection<BlRushDeliveryModeData> resultDeliveryData = new ArrayList<>();
            for(BlRushDeliveryModeModel blRushDeliveryModeModel : blRushDeliveryModeModels) {
                final BlRushDeliveryModeData blRushDeliveryModeData = getBlRushDeliveryModeConverter().convert(blRushDeliveryModeModel);
                if(null != blRushDeliveryModeData) {
                    blRushDeliveryModeData.setDeliveryCost(getPriceDataFactory().create(PriceDataType.BUY, BigDecimal.valueOf(
                            getBlZoneDeliveryModeService().getAmountForAppropriateZoneModel(null, getCart(), blRushDeliveryModeModel)),
                            getCart().getCurrency().getIsocode()));
                    resultDeliveryData.add(blRushDeliveryModeData);
                }
            }
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean setDeliveryMethod(final String deliveryMethod) {
        final CartModel cartModel = getCart();
        if (cartModel != null)
        {
            final DeliveryModeModel deliveryModeModel = getDeliveryService().getDeliveryModeForCode(deliveryMethod);
            if (deliveryModeModel != null)
            {
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
    public void savePickUpInfoOnCart(final BlPickUpByForm blPickUpByForm) {
        final CartModel cartModel = getCart();
        if (cartModel != null)
        {
            cartModel.setFirstName(blPickUpByForm.getFirstName());
            cartModel.setLastName(blPickUpByForm.getLastName());
            cartModel.setEmail(blPickUpByForm.getEmail());
            cartModel.setPhone(blPickUpByForm.getEmail());
            getModelService().save(cartModel);
            getModelService().refresh(cartModel);
        }
    }

    @Override
    public boolean setDeliveryModeIfAvailable()
    {
        final CartModel cartModel = getCart();
        if (cartModel != null)
        {
            // validate delivery mode if already exists
            //getCommerceCheckoutService().validateDeliveryMode(createCommerceCheckoutParameter(cartModel, true));

            if (cartModel.getDeliveryMode() == null)
            {
                return false;
                /*
                final List<? extends DeliveryModeData> availableDeliveryModes = getSupportedDeliveryModes();
                if (!availableDeliveryModes.isEmpty())
                {
                    return setDeliveryMode(availableDeliveryModes.get(0).getCode());
                }*/
            }
            return true;
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String removeDeliveryDetails() {
        final CartModel cartModel = getCart();
        if (cartModel != null)
        {
            cartModel.setDeliveryAddress(null);
            cartModel.setDeliveryCost(null);
            cartModel.setDeliveryMode(null);
            cartModel.setFirstName(null);
            cartModel.setLastName(null);
            cartModel.setEmail(null);
            cartModel.setPhone(null);
            getModelService().save(cartModel);
            getModelService().refresh(cartModel);
            return "SUCCESS";
        }
        return "ERROR";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean checkPartnerPickCodeValidity(final String pinCode) {
        return true;
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
}
