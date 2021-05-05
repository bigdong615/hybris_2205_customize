package com.bl.core.shipping.service.impl;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.CarrierEnum;
import com.bl.core.enums.DeliveryTypeEnum;
import com.bl.core.model.*;
import com.bl.core.shipping.dao.BlDeliveryModeDao;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeValueModel;
import de.hybris.platform.order.impl.DefaultZoneDeliveryModeService;
import de.hybris.platform.util.Config;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.time.DayOfWeek;
import java.util.*;
import java.util.stream.Collectors;

/**
 * {javadoc}
 *
 * @auther Namrata Lohar
 */
public class DefaultBlDeliveryModeService extends DefaultZoneDeliveryModeService implements BlDeliveryModeService {

    private static final Logger LOG = Logger.getLogger(DefaultBlDeliveryModeService.class);

    @Resource(name = "blDeliveryModeDao")
    BlDeliveryModeDao blDeliveryModeDao;

    /**
     * {@inheritDoc}
     *
     * @return
     */
    @Override
    public Collection<ShippingGroupModel> getAllShippingGroups() {
        return getBlZoneDeliveryModeDao().getAllShippingGroups();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModes(final String carrier, final String mode,
                                                                        final String pstCutOffTime) {
        return getBlZoneDeliveryModeDao().getShipToHomeDeliveryModes(carrier, mode, pstCutOffTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesNotLike(final String carrier, final String mode,
                                                                               final String pstCutOffTime) {
        return getBlZoneDeliveryModeDao().getShipToHomeDeliveryModesNotLike(carrier, mode, pstCutOffTime);
    }

    /**
     * {@inheritDoc}
     * @return
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getAllShipToHomeDeliveryModesWithRentalDates(final String rentalStart,
                                                                                          final String rentalEnd) {

        Collection<ZoneDeliveryModeModel> allDeliveryModes = new ArrayList<>();
        Collection<ZoneDeliveryModeModel> upsZoneDeliveryModes = getShipToHomeDeliveryModesWithRentalDates(rentalStart,
                rentalEnd, String.valueOf(CarrierEnum.UPS)).stream().filter(this::checkDaysToSkipForDeliveryMode).
                collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(upsZoneDeliveryModes)) {
            allDeliveryModes.addAll(upsZoneDeliveryModes);
        }
        Collection<ZoneDeliveryModeModel> fedexZoneDeliveryModes = getShipToHomeDeliveryModesWithRentalDates(rentalStart,
                rentalEnd, String.valueOf(CarrierEnum.FEDEX)).stream().filter(this::checkDaysToSkipForDeliveryMode)
                .collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(fedexZoneDeliveryModes)) {
            allDeliveryModes.addAll(fedexZoneDeliveryModes);
        }
        return allDeliveryModes;
    }

    /**
     * {@inheritDoc}
     *
     * @return
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesWithRentalDates(final String rentalStart,
                                                                                       final String rentalEnd,
                                                                                       final String carrier) {
        final boolean available = checkCartEntriesAvailability();
        final String pstCutOffTime = BlDateTimeUtils.getCurrentTimeUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST);
        final int result = checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST),
                rentalStart);
        if(available) {
            if (result >= BlInventoryScanLoggingConstants.TWO) {
                return getShipToHomeDeliveryModes(carrier, BlDeliveryModeLoggingConstants.DELIVERY_TYPE_STANDARD,
                        null);
            } else if (result == BlInventoryScanLoggingConstants.ONE) {
                final DayOfWeek currentDayOfWeek = BlDateTimeUtils.getDayOfWeek(BlDeliveryModeLoggingConstants.ZONE_PST);
                if(currentDayOfWeek.equals(DayOfWeek.SUNDAY) || currentDayOfWeek.equals(DayOfWeek.SATURDAY)) {
                    return getShipToHomeDeliveryModes(carrier, BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT,
                            null);
                } else {
                    return getShipToHomeDeliveryModes(carrier, BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT,
                            pstCutOffTime);
                }
            }
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModes(final String mode, final String pstCutOffTime) {
        return getBlZoneDeliveryModeDao().getPartnerZoneUPSStoreDeliveryModes(mode, pstCutOffTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModesNotLike(final String mode,
                                                                                                final String pstCutOffTime) {
        return getBlZoneDeliveryModeDao().getPartnerZoneUPSStoreDeliveryModesNotLike(mode, pstCutOffTime);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerPickUpDeliveryModesWithRentalDates(final String rentalStart,
                                                                                                  final String rentalEnd) {
        final boolean available = checkCartEntriesAvailability();
        final String pstCutOffTime = BlDateTimeUtils.getCurrentTimeUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST);
        final int result = checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST),
                rentalStart);
        if (result >= BlInventoryScanLoggingConstants.TWO) {
            return available ? getPartnerZoneUPSStoreDeliveryModes(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_STANDARD, null)
                    : getPartnerZoneUPSStoreDeliveryModesNotLike(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_STANDARD, null);
        } else if (result == BlInventoryScanLoggingConstants.ONE) {
            return available ? getPartnerZoneUPSStoreDeliveryModes(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT, pstCutOffTime)
                    : getPartnerZoneUPSStoreDeliveryModesNotLike(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT, pstCutOffTime);
        } else {
            return Collections.emptyList();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getAllPartnerPickUpDeliveryModesWithRentalDatesForUPSStore(final String rentalStart,
                                                                                                             final String rentalEnd) {
        return getPartnerPickUpDeliveryModesWithRentalDates(
                rentalStart, rentalEnd).stream().filter(this::checkDaysToSkipForDeliveryMode).collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     *
     * @return
     */
    @Override
    public Collection<PartnerPickUpStoreModel> getAllPartnerPickUpStore() {
        return getBlZoneDeliveryModeDao().getPartnerPickUpStore();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModes(final String partnerZone,
                                                                              final String rentalStart,
                                                                              final String rentalEnd) {
        if (checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST),
                rentalStart) > BlInventoryScanLoggingConstants.TWO) {
            Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels = null;
            final PartnerPickUpStoreModel partnerPickUpStoreModel = getPartnerPickUpStoreFromPartnerZone(partnerZone);
            if (partnerPickUpStoreModel != null && partnerPickUpStoreModel.isWarehousePickUp()) {
                blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeDao()
                        .getPartnerZoneDeliveryModes(partnerZone, BlDateTimeUtils.getCurrentTimeUsingCalendar(
                                BlDeliveryModeLoggingConstants.ZONE_PST));

            } else {
                if (checkCartEntriesAvailability()) {
                    blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeDao().getPartnerZoneDeliveryModes(partnerZone,
                            null);
                }
            }
            return CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels) ? blPickUpZoneDeliveryModeModels.stream()
                    .filter(this::checkDaysToSkipForDeliveryMode).collect(Collectors.toList()) : Collections.emptyList();
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PartnerPickUpStoreModel getPartnerPickUpStoreFromPartnerZone(final String partnerZone) {
        return getBlZoneDeliveryModeDao().getPartnerPickUpStoreFromPartnerZone(partnerZone);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlRushDeliveryModeModel> getBlRushDeliveryModes(final String pinCode, final String deliveryMode,
                                                                         final String pstCutOffTime) {
        if (checkSFOrNYCPinCodeValidity(pinCode, deliveryMode)) {
            final Collection<BlRushDeliveryModeModel> blRushDeliveryModeModels = getBlZoneDeliveryModeDao().getBlRushDeliveryModes(
                    deliveryMode, pstCutOffTime).stream().filter(this::checkDaysToSkipForDeliveryMode).collect(Collectors.toList());
            return CollectionUtils.isNotEmpty(blRushDeliveryModeModels) ? blRushDeliveryModeModels : Collections.emptyList();
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ShippingCostModel getShippingCostForCalculatedDeliveryCost(final String calculatedCost, final String deliveryMethod) {
        return getBlZoneDeliveryModeDao().getShippingCostForCalculatedDeliveryCost(calculatedCost, deliveryMethod);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getShippingCostAmount(final AbstractOrderModel order, final DeliveryModeModel deliveryMode) {
        if(deliveryMode instanceof BlPickUpZoneDeliveryModeModel) {
            final BlPickUpZoneDeliveryModeModel zoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) deliveryMode;
            return getAmountForAppropriateZoneModel(order, zoneDeliveryModeModel);
        } else if(deliveryMode instanceof BlRushDeliveryModeModel) {
            final BlRushDeliveryModeModel zoneDeliveryModeModel = (BlRushDeliveryModeModel) deliveryMode;
            return getAmountForAppropriateZoneModel(order, zoneDeliveryModeModel);
        } else if (deliveryMode instanceof ZoneDeliveryModeModel) {
            final ZoneDeliveryModeModel zoneDeliveryModeModel = (ZoneDeliveryModeModel) deliveryMode;
            return getAmountForAppropriateZoneModel(order, zoneDeliveryModeModel);
        }
        return BlInventoryScanLoggingConstants.ZERO;
    }

    /**
     * {@inheritDoc}
     * @return
     */
    @Override
    public double getAmountForAppropriateZoneModel(final AbstractOrderModel order, final ZoneDeliveryModeModel zoneDeliveryModeModel) {
        for(ZoneDeliveryModeValueModel valueModel : zoneDeliveryModeModel.getValues()) {
            if(!valueModel.isFixedAmount()) {
                final Map<String, Double> calculatedValueMap;   
                try {
                    calculatedValueMap = getCalculatedWeightForDelivery(order);
                    final Double shippingCostModel = getShippingAmount(order, zoneDeliveryModeModel, calculatedValueMap);
                    return shippingCostModel != null ? shippingCostModel : BlInventoryScanLoggingConstants.ZERO;
                } catch(Exception e) {
                    BlLogger.logMessage(LOG, Level.ERROR,"Exception while calculating delivery cost");
                }
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"ShippingCostModel is null, Shipping amount: {} ",
                        BlInventoryScanLoggingConstants.ZERO);
                return BlInventoryScanLoggingConstants.ZERO;
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Shipping fixed amount: {} ", valueModel.getValue());
                return valueModel.getValue();
            }
        }
        return BlInventoryScanLoggingConstants.ZERO;
    }

    private Double getShippingAmount(AbstractOrderModel order, ZoneDeliveryModeModel zoneDeliveryModeModel, Map<String, Double> calculatedValueMap) {
        if(!(order instanceof CartModel)) {
            order.setTotalWeight(calculatedValueMap.get(BlDeliveryModeLoggingConstants.TOTAL_WEIGHT));
            order.setDimensionalWeight(calculatedValueMap.get(BlDeliveryModeLoggingConstants.DIMENSIONAL_WEIGHT));
        }
        final ShippingCostModel shippingCostModel = getShippingCostForCalculatedDeliveryCost(String.valueOf(Math.max(
                calculatedValueMap.get(BlDeliveryModeLoggingConstants.TOTAL_WEIGHT), calculatedValueMap.get(
                        BlDeliveryModeLoggingConstants.DIMENSIONAL_WEIGHT))), zoneDeliveryModeModel.getCode());
        if(shippingCostModel != null) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Shipping calculated amount: {} ",
                    shippingCostModel.getAmount());
            return shippingCostModel.getAmount();
        }
        return null;
    }

    /**
     * This method will calculate total weight and dimensional weight for order from order product entries
     *
     * @param order model
     * @return Map of total weight and dimensional weight for order entries
     */
    protected Map<String, Double> getCalculatedWeightForDelivery(final AbstractOrderModel order) {
        final Map<String, Double> valueMap = new HashMap<>();
        Collection<AbstractOrderEntryModel> abstractOrderEntryModels = new ArrayList<>();
        BigDecimal totalWeight = new BigDecimal(BlInventoryScanLoggingConstants.ZERO);
        int maxLength = BlInventoryScanLoggingConstants.ZERO;
        int maxHeight = BlInventoryScanLoggingConstants.ZERO;
        int sumWidth = BlInventoryScanLoggingConstants.ZERO;
        try {
            if(order instanceof CartModel) {
                final CartModel cart = (CartModel) order;
                abstractOrderEntryModels = cart.getEntries();
            } else {
                abstractOrderEntryModels = order.getEntries();
            }
            for(AbstractOrderEntryModel entry : abstractOrderEntryModels) {
                final BlProductModel blSerialProduct = (BlProductModel) entry.getProduct();
                if(blSerialProduct != null) {
                    totalWeight = getBigDecimal(totalWeight, blSerialProduct);
                    sumWidth = getSumWidth(sumWidth, blSerialProduct.getWidth());
                    maxHeight = getMaxHeight(maxHeight, blSerialProduct.getHeight());
                    maxLength = getMaxLength(maxLength, blSerialProduct.getLength());
                }
            }

            final double dimensionalWeight = (maxHeight * sumWidth * maxLength) /
                    Config.getInt(BlDeliveryModeLoggingConstants.DIMENSIONAL_FACTOR_KEY, BlDeliveryModeLoggingConstants.DIMENSIONAL_FACTOR);
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Total weight: {} ", totalWeight.doubleValue());
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Dimensional weight: {} ", dimensionalWeight);

            valueMap.put(BlDeliveryModeLoggingConstants.TOTAL_WEIGHT, totalWeight.doubleValue());
            valueMap.put(BlDeliveryModeLoggingConstants.DIMENSIONAL_WEIGHT, dimensionalWeight);
        } catch(Exception e) {
            BlLogger.logMessage(LOG, Level.ERROR,"Exception while calculating delivery cost");
        }
        return valueMap;
    }

    /**
     * This method will calulate total weight
     * @param totalWeight weight
     * @param blSerialProduct product
     * @return big decimal
     */
    private BigDecimal getBigDecimal(final BigDecimal totalWeight, final BlProductModel blSerialProduct) {
        return blSerialProduct.getWeight() != null ? (totalWeight.add(blSerialProduct.getWeight())) :
                (totalWeight.add(BigDecimal.valueOf(BlInventoryScanLoggingConstants.ZERO_FIVE)));
    }

    /**
     * This method will calculate sum of all products width
     *
     * @param sumWidth width
     * @param width width
     * @return value
     */
    private int getSumWidth(final int sumWidth, final Integer width) {
        return width != null ? (sumWidth + width) : (sumWidth + BlInventoryScanLoggingConstants.FIVE);
    }

    /**
     * This method will calculate max height
     * @param maxHeight height
     * @param height height
     * @return height value
     */
    private int getMaxHeight(final int maxHeight, final Integer height) {
        return height != null ? Math.max(height, maxHeight) : Math.max(BlInventoryScanLoggingConstants.FIVE, maxHeight);
    }

    /**
     * This method will calculate max length
     * @param maxLength length
     * @param length length
     * @return length value
     */
    private int getMaxLength(final int maxLength, final Integer length) {
        return length != null ? Math.max(length, maxLength) : Math.max(BlInventoryScanLoggingConstants.FIVE, maxLength);
    }

    /**
     * This method will check delivery mode's model instance and send appropriate model further
     * @param deliveryMode model
     * @return boolean for current delivery mode to disable or not
     */
    public boolean checkDaysToSkipForDeliveryMode(final DeliveryModeModel deliveryMode) {
        final DayOfWeek currentDayOfWeek = BlDateTimeUtils.getDayOfWeek(BlDeliveryModeLoggingConstants.ZONE_PST);
        if(deliveryMode instanceof BlPickUpZoneDeliveryModeModel) {
            final BlPickUpZoneDeliveryModeModel zoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) deliveryMode;
            return getResultForDayToSkip(currentDayOfWeek, zoneDeliveryModeModel);
        } else if(deliveryMode instanceof BlRushDeliveryModeModel) {
            final BlRushDeliveryModeModel zoneDeliveryModeModel = (BlRushDeliveryModeModel) deliveryMode;
            return getResultForDayToSkip(currentDayOfWeek, zoneDeliveryModeModel);
        } else if (deliveryMode instanceof ZoneDeliveryModeModel) {
            final ZoneDeliveryModeModel zoneDeliveryModeModel = (ZoneDeliveryModeModel) deliveryMode;
            return getResultForDayToSkip(currentDayOfWeek, zoneDeliveryModeModel);
        } else {
            return false;
        }
    }

    /**
     * This mwthod will check current day against days to skip for delivery mode and if current day matches then that
     * delivery mode will be disabled for the day
     * @param currentDayOfWeek today
     * @param zoneDeliveryModeModel delivery mode
     * @return boolean, where delivery mode will be disabled or not
     */
    private boolean getResultForDayToSkip(final DayOfWeek currentDayOfWeek, final ZoneDeliveryModeModel zoneDeliveryModeModel) {
        final Collection<de.hybris.platform.cronjob.enums.DayOfWeek> dayOfWeeks = zoneDeliveryModeModel.getDaysToSkip();
        for(de.hybris.platform.cronjob.enums.DayOfWeek day : dayOfWeeks) {
            if(day.getCode().equals(currentDayOfWeek.toString())) {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Current day is present in days to skip: {} ", currentDayOfWeek);
                return false;
            }
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean checkSFOrNYCPinCodeValidity(final String pinCode, final String deliveryType) {
        if (deliveryType.equals(DeliveryTypeEnum.SF.toString())) {
            //check pinCode in ~50miles
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Checking same day delivery pinCode validity");
        } else {
            //check pinCode in ~25miles
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Checking next day delivery pinCode validity");
        }
        return true;
    }

    /**
     * This method will check cart entries for gear availability
     *
     * @return boolean true or false
     */
    public boolean checkCartEntriesAvailability() {
        return true;
    }

    /**
     * This method will check conditions on rentalDate to return appropriate delivery modes
     *
     * @param currentDay  means current Day
     * @param rentalStart means start rental date
     * @return long i.e., difference in days
     */
    private int checkDateForRental(final String currentDay, final String rentalStart) {
        final int days = BlDateTimeUtils.getDaysBetweenBusinessDays(currentDay, rentalStart);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Business days difference: {} ", days);
        return days;
    }

    public BlDeliveryModeDao getBlZoneDeliveryModeDao() {
        return blDeliveryModeDao;
    }

    public void setBlZoneDeliveryModeDao(final BlDeliveryModeDao blDeliveryModeDao) {
        this.blDeliveryModeDao = blDeliveryModeDao;
    }
}
