package com.bl.core.shipping.strategy.impl;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.AddressTypeEnum;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.enums.CarrierEnum;
import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.core.enums.OptimizedShippingTypeEnum;
import com.bl.core.model.OptimizedShippingMethodModel;
import com.bl.core.model.ShippingOptimizationModel;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.shipping.strategy.BlShippingOptimizationStrategy;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.internal.service.AbstractBusinessService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class DefaultBlShippingOptimizationStrategy extends AbstractBusinessService implements BlShippingOptimizationStrategy {

    private static final Logger LOG = Logger.getLogger(DefaultBlShippingOptimizationStrategy.class);

    private BlDeliveryModeService zoneDeliveryModeService;
    private BlDatePickerService blDatePickerService;
    private BlCommerceStockService blCommerceStockService;

    /**
     * {@inheritDoc}
     */
    @Override
    public SourcingLocation getProductAvailabilityForThreeDayGround(final SourcingContext context, final SourcingLocation sourcingLocation) {
        if (context != null && CollectionUtils.isNotEmpty(context.getOrderEntries())) {
            final Optional<AbstractOrderEntryModel> orderEntryModel = context.getOrderEntries().stream().findFirst();
            if (orderEntryModel.isPresent()) {
                final AbstractOrderModel order = orderEntryModel.get().getOrder();
                return order != null ? getUpdatedSourcingLocation(sourcingLocation, order, BlDateTimeUtils.subtractDaysInRentalDates(
                        BlCoreConstants.SKIP_THREE_DAYS, BlDateTimeUtils.getDateInStringFormat(order.getRentalStartDate()),
                        getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY))) : setFalseSourcingLocation(sourcingLocation);
            }
        }
        return setFalseSourcingLocation(sourcingLocation);
    }

    /**
     * This method will fetch Stock for entry of order
     *
     * @param sourcingLocation consignment details
     * @param order            order details
     * @param rentalStart      date
     * @return Sourcing Location
     */
    private SourcingLocation getUpdatedSourcingLocation(final SourcingLocation sourcingLocation, final AbstractOrderModel order,
                                                        final Date rentalStart) {
        final int result = BlDateTimeUtils.getBusinessDaysDifferenceWithCutOffTime(BlDateTimeUtils.convertStringDateToDate(
                BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
                BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN), rentalStart, sourcingLocation.getWarehouse().getCutOffTime());

        if (result >= BlInventoryScanLoggingConstants.THREE) {
            final Map<String, Long> allocatedMap = sourcingLocation.getAllocatedMap();
            if (allocatedMap != null) {
                final Set<String> productCodes = allocatedMap.keySet();
                final Collection<StockLevelModel> threeStockLevelModels = getStockLevelModels(sourcingLocation, order, productCodes);
                if (CollectionUtils.isNotEmpty(productCodes) && CollectionUtils.isNotEmpty(threeStockLevelModels)) {
                    return getSourcingLocationForStock(sourcingLocation, allocatedMap, threeStockLevelModels.stream().collect(
                            Collectors.groupingBy(StockLevelModel::getProductCode)), order);
                }
            }
        }
        return setFalseSourcingLocation(sourcingLocation);
    }

    /**
     * This method will validate total stock against required stock and set in map
     *
     * @param sourcingLocation       consignment details
     * @param allocatedMap           map
     * @param stockLevelsProductWise map
     * @return SourcingLocation
     */
    private SourcingLocation getSourcingLocationForStock(final SourcingLocation sourcingLocation, final Map<String, Long> allocatedMap,
                                                         final Map<String, List<StockLevelModel>> stockLevelsProductWise, final AbstractOrderModel order) {
        if (stockLevelsProductWise != null) {
            Map<String, List<StockLevelModel>> availabilityMap = new HashMap<>();
            int i = BlInventoryScanLoggingConstants.ZERO;
            for (Map.Entry<String, Long> entry : allocatedMap.entrySet()) {
                final String key = entry.getKey().substring(0, entry.getKey().lastIndexOf('_'));
                if (stockLevelsProductWise.get(key) != null && stockLevelsProductWise.get(key).size() >= entry.getValue()) {
                    availabilityMap.put(key, stockLevelsProductWise.get(key));
                } else {
                    i = BlInventoryScanLoggingConstants.ONE;
                    break;
                }
            }
            return getSourcingLocationForGroundStock(sourcingLocation, i, availabilityMap, order);
        }
        return setFalseSourcingLocation(sourcingLocation);
    }

    /**
     * This method will decide result of stock checking and put appropriate details in AvailabilityMap.
     *
     * @param sourcingLocation consignment details
     * @param i                value
     * @param availabilityMap  map
     * @return SourcingLocation
     */
    private SourcingLocation getSourcingLocationForGroundStock(final SourcingLocation sourcingLocation, final int i,
                                                               final Map<String, List<StockLevelModel>> availabilityMap,
                                                               final AbstractOrderModel order) {
        if (i == BlInventoryScanLoggingConstants.ZERO) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Returning three day ground availability stock!!");
            sourcingLocation.setGroundAvailability(Boolean.TRUE);
            sourcingLocation.setGroundAvailabilityCode(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode());
            sourcingLocation.setAvailabilityMap(availabilityMap);
            sourcingLocation.getContext().getOrderEntries().iterator().next().getOrder().setActualRentalStartDate(
                    BlDateTimeUtils.subtractDaysInRentalDates(BlCoreConstants.SKIP_THREE_DAYS, BlDateTimeUtils.getDateInStringFormat(
                            order.getRentalStartDate()),getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY)));
            sourcingLocation.getContext().getOrderEntries().iterator().next().getOrder().setActualRentalEndDate(
                    BlDateTimeUtils.addDaysInRentalDates(BlCoreConstants.SKIP_THREE_DAYS, BlDateTimeUtils.getDateInStringFormat(
                            order.getRentalEndDate()), getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY)));
            return sourcingLocation;
        } else {
            return setFalseSourcingLocation(sourcingLocation);
        }
    }

    /**
     * This method will fetch stock for 3 day ground
     *
     * @param sourcingLocation consignment details
     * @param order            details
     * @param productCodes     set of strings
     * @return Collection<StockLevelModel>
     */
    private Collection<StockLevelModel> getStockLevelModels(final SourcingLocation sourcingLocation, final AbstractOrderModel order,
                                                            final Set<String> productCodes) {
        final Set<String> newProductCodes = new HashSet<>();
        for(final String product : productCodes) {
            newProductCodes.add(product.substring(0, product.lastIndexOf('_')));
        }
        return getBlCommerceStockService().getStockForProductCodesAndDate(newProductCodes, sourcingLocation.getWarehouse(),
                BlDateTimeUtils.subtractDaysInRentalDates(BlCoreConstants.SKIP_THREE_DAYS, BlDateTimeUtils.getDateInStringFormat(order.getRentalStartDate()),
                        getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY)), BlDateTimeUtils.addDaysInRentalDates(BlCoreConstants.SKIP_THREE_DAYS,
                        BlDateTimeUtils.getDateInStringFormat(order.getRentalEndDate()), getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY)));
    }

    /**
     * Common method to set failure result for 3 day ground
     *
     * @param sourcingLocation details
     * @return Updated SourcingLocation
     */
    private SourcingLocation setFalseSourcingLocation(final SourcingLocation sourcingLocation) {
        sourcingLocation.setGroundAvailability(Boolean.FALSE);
        sourcingLocation.setGroundAvailabilityCode(OptimizedShippingMethodEnum.DEFAULT.getCode());
        return sourcingLocation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean generateShipmentLabelForConsignment(final ConsignmentModel consignmentModel) {
        if (!BlDateTimeUtils.compareTimeWithCutOff(consignmentModel.getWarehouse().getCutOffTime())) {
            final Date rentalStartDate = consignmentModel.getOrder().getRentalStartDate();
            final String rentalStringStartDate = BlDateTimeUtils.convertDateToStringDate(consignmentModel.getOrder()
                    .getRentalStartDate(), BlCoreConstants.DATE_FORMAT);
            final String rentalEndDate = BlDateTimeUtils.convertDateToStringDate(consignmentModel.getOrder().getRentalEndDate(),
                    BlCoreConstants.DATE_FORMAT);
            final int result = BlDateTimeUtils.getBusinessDaysDifferenceWithCutOffTime(consignmentModel.getOptimizedShippingStartDate(),
                    rentalStartDate, consignmentModel.getWarehouse().getCutOffTime());
            if(OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE.getCode().equals(consignmentModel.getOptimizedShippingMethodType().getCode())) {
                if (result >= BlInventoryScanLoggingConstants.TWO) {
                    return checkTwoDayAir(consignmentModel, rentalStringStartDate, rentalEndDate);
                } else {
                    return checkNextDayAir(consignmentModel, rentalStringStartDate, rentalEndDate);
                }
            } else {
                return getOptimizedShippingMethod(consignmentModel, rentalStringStartDate, rentalEndDate, result, consignmentModel.getOptimizedShippingType(),
                        getCarrierId((ZoneDeliveryModeModel) consignmentModel.getDeliveryMode()), getWarehouseCode(consignmentModel.getWarehouse()));
            }
        }
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Consignment :" + consignmentModel.getCode() + " can't be optimized as warehouse " +
                "cutOff time is not passed yet");
        return false;
    }

    /**
     * This method will check existing assigned optimized shipping method and assign another optimization
     *
     * @param consignmentModel            details
     * @param rentalStartDate             date
     * @param rentalEndDate               date
     * @param result                      difference
     * @param optimizedShippingMethodEnum code
     * @param carrierId                   UPS/FEDEX
     * @param warehouseCode               CA/MA
     * @return true if success
     */
    private boolean getOptimizedShippingMethod(final ConsignmentModel consignmentModel, final String rentalStartDate,
                                               final String rentalEndDate, final int result,
                                               final OptimizedShippingMethodModel optimizedShippingMethodEnum, final int carrierId,
                                               final int warehouseCode) {
   	 final String addressZip = getAddressZip(consignmentModel.getShippingAddress());
        if (optimizedShippingMethodEnum.getCode().equals(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode())) {
            return checkTwoDayGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else if (optimizedShippingMethodEnum.getCode().equals(OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode())) {
            return checkOvernightGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else if (optimizedShippingMethodEnum.getCode().equals(OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode())) {
            if (result == BlInventoryScanLoggingConstants.ZERO) {
                setOptimizedDetailsOnConsignment(consignmentModel, result, consignmentModel.getOrder().getRentalStartDate(),
                        consignmentModel.getOrder().getRentalEndDate(), OptimizedShippingMethodEnum.DEFAULT);
                return true;
            } else {
                return result == BlInventoryScanLoggingConstants.ONE ? checkNextDayAir(consignmentModel, rentalStartDate,
                        rentalEndDate) : checkTwoDayAir(consignmentModel, rentalStartDate, rentalEndDate);
            }
        } else if (optimizedShippingMethodEnum.getCode().equals(OptimizedShippingMethodEnum.TWO_DAY_AIR.getCode()) ||
                optimizedShippingMethodEnum.getCode().equals(OptimizedShippingMethodEnum.TWO_DAY_AIR_AM.getCode())) {
            return checkNextDayAir(consignmentModel, rentalStartDate, rentalEndDate);
        } else {
            setOptimizedDetailsOnConsignment(consignmentModel, result, consignmentModel.getOrder().getRentalStartDate(),
                    consignmentModel.getOrder().getRentalEndDate(), OptimizedShippingMethodEnum.DEFAULT);
            return true;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getOptimizedShippingMethodForOrder(final ConsignmentModel consignmentModel) {
        final String rentalStartDate = BlDateTimeUtils.getDateInStringFormat(consignmentModel.getOrder().getRentalStartDate());
        final String rentalEndDate = BlDateTimeUtils.getDateInStringFormat(consignmentModel.getOrder().getRentalEndDate());
        final int result = BlDateTimeUtils.getBusinessDaysDifferenceWithCutOffTime(consignmentModel.getOrder().getActualRentalStartDate(),
                consignmentModel.getOrder().getRentalStartDate(), consignmentModel.getWarehouse().getCutOffTime());
        final int carrierId = getCarrierId((ZoneDeliveryModeModel) consignmentModel.getDeliveryMode());
        final int warehouseCode = getWarehouseCode(consignmentModel.getWarehouse());
        final String addressZip = getAddressZip(consignmentModel.getShippingAddress());

        if (consignmentModel.isThreeDayGroundAvailability() && result >= BlInventoryScanLoggingConstants.THREE) {
            return checkThreeDayGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else if (result == BlInventoryScanLoggingConstants.TWO) {
            return checkTwoDayGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else if (result == BlInventoryScanLoggingConstants.ONE) {
            return checkOvernightGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else {

            setOptimizedDetailsOnConsignment(consignmentModel, result, consignmentModel.getOrder().getRentalStartDate(),
                consignmentModel.getOrder().getRentalEndDate(), OptimizedShippingMethodEnum.DEFAULT);

            //return checkTwoDayAir(consignmentModel, rentalStartDate, rentalEndDate);
            return false;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean checkCurrentDayInBlackOutDays() {
        return BlDateTimeUtils.isDateFallsOnBlackOutDate(Objects.requireNonNull(BlDateTimeUtils.convertStringDateToDate(
                BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
                BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN)),
                getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY));
    }

    /**
     * This method will check three day ground
     *
     * @param result           day difference
     * @param carrierId        UPS/FedEx
     * @param warehouseCode    CA/MA
     * @param customerZip      code
     * @param consignmentModel order
     * @param rentalStart      date
     * @param rentalEnd        date
     * @return true if success
     */
    private boolean checkThreeDayGround(final int result, final int carrierId, final int warehouseCode, final String customerZip,
                                        final ConsignmentModel consignmentModel, final String rentalStart, final String rentalEnd) {
        final ShippingOptimizationModel shippingOptimizationModel = getZoneDeliveryModeService().getOptimizedShippingRecord(
                carrierId, warehouseCode, customerZip, BlInventoryScanLoggingConstants.THREE, BlInventoryScanLoggingConstants.ONE);
        if (shippingOptimizationModel != null) {
            final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SAVING + OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode()
                    + BlInventoryScanLoggingConstants.SPACE + consignmentModel.getCode());
            setOptimizedDetailsOnConsignment(consignmentModel, result, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_THREE_DAYS, rentalStart, blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_THREE_DAYS, rentalEnd, blackOutDates), OptimizedShippingMethodEnum.THREE_DAY_GROUND);
            return true;
        } else {
            return checkTwoDayGround(BlInventoryScanLoggingConstants.TWO, carrierId, warehouseCode, customerZip, consignmentModel,
                    rentalStart, rentalEnd);
        }
    }

    /**
     * This method will check two day ground
     *
     * @param result           day difference
     * @param carrierId        UPS/FedEx
     * @param warehouseCode    CA/MA
     * @param customerZip      code
     * @param consignmentModel order
     * @param rentalStart      date
     * @param rentalEnd        date
     * @return true if success
     */
    private boolean checkTwoDayGround(final int result, final int carrierId, final int warehouseCode, final String customerZip,
                                      final ConsignmentModel consignmentModel, final String rentalStart, final String rentalEnd) {
        final ShippingOptimizationModel shippingOptimizationModel = getZoneDeliveryModeService().getOptimizedShippingRecord(
                carrierId, warehouseCode, customerZip, result, BlInventoryScanLoggingConstants.ONE);
        if (shippingOptimizationModel != null) {
            final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SAVING +
                    OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode() + BlInventoryScanLoggingConstants.SPACE + consignmentModel.getCode());
            setOptimizedDetailsOnConsignment(consignmentModel, result, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalStart, blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalEnd, blackOutDates), OptimizedShippingMethodEnum.TWO_DAY_GROUND);
            return true;
        } else {
            return checkOvernightGround(result, carrierId, warehouseCode, customerZip, consignmentModel,
                    rentalStart, rentalEnd);
        }
    }

    /**
     * This method will check one day ground
     *
     * @param result           day difference
     * @param carrierId        UPS/FedEx
     * @param warehouseCode    CA/MA
     * @param customerZip      code
     * @param consignmentModel order
     * @param rentalStart      date
     * @param rentalEnd        date
     * @return true if success
     */
    private boolean checkOvernightGround(final int result, final int carrierId, final int warehouseCode, final String customerZip,
                                         final ConsignmentModel consignmentModel, final String rentalStart, final String rentalEnd) {
        final ShippingOptimizationModel shippingOptimizationModel = getZoneDeliveryModeService().getOptimizedShippingRecord(
                carrierId, warehouseCode, customerZip, result, BlInventoryScanLoggingConstants.ONE);
        if (shippingOptimizationModel != null) {
            final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SAVING +
                    OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode() + BlInventoryScanLoggingConstants.SPACE + consignmentModel.getCode());
            setOptimizedDetailsOnConsignment(consignmentModel, result, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalStart, blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalEnd, blackOutDates), OptimizedShippingMethodEnum.ONE_DAY_GROUND);
            return true;
        } else {
            return result >= BlInventoryScanLoggingConstants.TWO ? checkTwoDayAir(consignmentModel, rentalStart, rentalEnd) :
                    checkNextDayAir(consignmentModel, rentalStart, rentalEnd);
        }
    }

    /**
     * This method will check two day air ot AM
     *
     * @param consignmentModel order details
     * @param rentalStart      date
     * @param rentalEnd        date
     * @return true if success
     */
    private boolean checkTwoDayAir(final ConsignmentModel consignmentModel, final String rentalStart, final String rentalEnd) {
        final AddressModel shippingAddress = consignmentModel.getOrder().getDeliveryAddress();
        final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
        if (shippingAddress != null && shippingAddress.getAddressType() != null 
      		  && AddressTypeEnum.BUSINESS.getCode().equals(shippingAddress.getAddressType().getCode())) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SAVING + OptimizedShippingMethodEnum.TWO_DAY_AIR_AM.getCode()
                    + BlInventoryScanLoggingConstants.SPACE + consignmentModel.getCode());
            setOptimizedDetailsOnConsignment(consignmentModel, BlInventoryScanLoggingConstants.TWO, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalStart, blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalEnd, blackOutDates), OptimizedShippingMethodEnum.TWO_DAY_AIR_AM);
        } else {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SAVING + OptimizedShippingMethodEnum.TWO_DAY_AIR.getCode()
                    + BlInventoryScanLoggingConstants.SPACE + consignmentModel.getCode());
            setOptimizedDetailsOnConsignment(consignmentModel, BlInventoryScanLoggingConstants.TWO, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalStart, blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalEnd, blackOutDates), OptimizedShippingMethodEnum.TWO_DAY_AIR);
        }
        return true;
    }

    /**
     * This method will check next day air ot AM
     *
     * @param consignmentModel order details
     * @param rentalStart      date
     * @param rentalEnd        date
     * @return true if success
     */
    private boolean checkNextDayAir(final ConsignmentModel consignmentModel, final String rentalStart, final String rentalEnd) {
        final AddressModel shippingAddress = consignmentModel.getOrder().getDeliveryAddress();
        final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
        if (shippingAddress != null &&  shippingAddress.getAddressType() != null 
      		  && AddressTypeEnum.BUSINESS.getCode().equals(shippingAddress.getAddressType().getCode())) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SAVING + OptimizedShippingMethodEnum.NEXT_DAY_AIR_AM.getCode()
                    + BlInventoryScanLoggingConstants.SPACE + consignmentModel.getCode());
            setOptimizedDetailsOnConsignment(consignmentModel, BlInventoryScanLoggingConstants.TWO, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalStart, blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalEnd, blackOutDates), OptimizedShippingMethodEnum.NEXT_DAY_AIR_AM);
        } else {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SAVING + OptimizedShippingMethodEnum.NEXT_DAY_AIR.getCode()
                    + BlInventoryScanLoggingConstants.SPACE + consignmentModel.getCode());
            setOptimizedDetailsOnConsignment(consignmentModel, BlInventoryScanLoggingConstants.TWO, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalStart, blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalEnd, blackOutDates), OptimizedShippingMethodEnum.NEXT_DAY_AIR);
        }
        return true;
    }

    /**
     * This method with set details and save model
     *
     * @param consignmentModel        order
     * @param result                  of days
     * @param optimizedDate           date
     * @param optimizedShippingMethod methode
     */
    private void setOptimizedDetailsOnConsignment(final ConsignmentModel consignmentModel, final int result, final Date optimizedDate,
                                                  final Date optimizedEndDate, final OptimizedShippingMethodEnum optimizedShippingMethod) {
        consignmentModel.setOptimizedShippingStartDate(optimizedDate);
        consignmentModel.setOptimizedShippingEndDate(optimizedEndDate);
        if (result != BlInventoryScanLoggingConstants.ZERO) {
            final OptimizedShippingTypeEnum optimizedShippingType = checkConsignmentShippingType(consignmentModel, result);
            consignmentModel.setOptimizedShippingMethodType(optimizedShippingType);
            if(optimizedShippingType != null) {
                if (OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE.equals(optimizedShippingType)) {
                    consignmentModel.setOptimizedShippingType(getZoneDeliveryModeService().getOptimizedShippingMethod(
                            OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode()));
                } else {
                    consignmentModel.setOptimizedShippingType(getZoneDeliveryModeService().getOptimizedShippingMethod(
                            optimizedShippingMethod.getCode()));
                }
            }
        } else {
            consignmentModel.setOptimizedShippingType(null);
            consignmentModel.setOptimizedShippingMethodType(null);
        }
        getModelService().save(consignmentModel);
        getModelService().refresh(consignmentModel);
    }

    /**
     * This method will
     *
     * @param consignmentModel order
     * @param result           number of days
     * @return OptimizedShippingTypeEnum
     */
    private OptimizedShippingTypeEnum checkConsignmentShippingType(final ConsignmentModel consignmentModel, final int result) {
        final ZoneDeliveryModeModel zoneDeliveryModeModel = (ZoneDeliveryModeModel) consignmentModel.getDeliveryMode();
        if (zoneDeliveryModeModel != null) {
            final String shippingGroup = zoneDeliveryModeModel.getShippingGroup().getCode();
            if (BlDeliveryModeLoggingConstants.SHIP_HOME_HOTEL_BUSINESS.equals(shippingGroup)) {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlDeliveryModeLoggingConstants.OPTIMIZED_SHIPPING_TYPE +
                        OptimizedShippingTypeEnum.WAREHOUSE2CUSTOMER.getCode());
                return OptimizedShippingTypeEnum.WAREHOUSE2CUSTOMER;
            } else if (BlDeliveryModeLoggingConstants.SHIP_HOLD_UPS_OFFICE.equals(shippingGroup)) {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlDeliveryModeLoggingConstants.OPTIMIZED_SHIPPING_TYPE +
                        OptimizedShippingTypeEnum.WAREHOUSE2PICKUP.getCode());
                return OptimizedShippingTypeEnum.WAREHOUSE2PICKUP;
            } else if (BlDeliveryModeLoggingConstants.BL_PARTNER_PICKUP.equals(shippingGroup)) {
                if (zoneDeliveryModeModel.getCode().startsWith("BL_")) {
                    BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlDeliveryModeLoggingConstants.OPTIMIZED_SHIPPING_TYPE +
                            OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE.getCode());
                    return OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE;
                } else {
                    BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlDeliveryModeLoggingConstants.OPTIMIZED_SHIPPING_TYPE +
                            OptimizedShippingTypeEnum.WAREHOUSE2PICKUP.getCode());
                    return OptimizedShippingTypeEnum.WAREHOUSE2PICKUP;
                }
            } else if (BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY.equals(shippingGroup)) {
                return getSameAndNextDayOptimizedShippingType(consignmentModel, result, zoneDeliveryModeModel,
                        BlInventoryScanLoggingConstants.ONE);
            } else {
                return getSameAndNextDayOptimizedShippingType(consignmentModel, result, zoneDeliveryModeModel,
                        BlInventoryScanLoggingConstants.TWO);
            }
        }
        return null;
    }

    /**
     * This method is refactoration for returning OptimizedShippingTypeEnum
     *
     * @param consignmentModel      order
     * @param result                no. of days
     * @param zoneDeliveryModeModel delivery method
     * @param days                  days
     * @return OptimizedShippingTypeEnum
     */
    private OptimizedShippingTypeEnum getSameAndNextDayOptimizedShippingType(final ConsignmentModel consignmentModel, final int result,
                                                                             final ZoneDeliveryModeModel zoneDeliveryModeModel, final int days) {
        if (result >= days) {
            if (zoneDeliveryModeModel.getWarehouse().getCode().equals(consignmentModel.getWarehouse().getCode())) {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlDeliveryModeLoggingConstants.OPTIMIZED_SHIPPING_TYPE +
                        OptimizedShippingTypeEnum.WAREHOUSE2CUSTOMER.getCode());
                return OptimizedShippingTypeEnum.WAREHOUSE2CUSTOMER;
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlDeliveryModeLoggingConstants.OPTIMIZED_SHIPPING_TYPE +
                        OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE.getCode());
                return OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE;
            }
        } else {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlDeliveryModeLoggingConstants.OPTIMIZED_SHIPPING_TYPE +
                    OptimizedShippingTypeEnum.WAREHOUSE2CUSTOMER.getCode());
            return OptimizedShippingTypeEnum.WAREHOUSE2CUSTOMER;
        }
    }

    /**
     * This method will return addressZip
     *
     * @param addressModel delivery mode
     * @return result
     */
    private String getAddressZip(final AddressModel addressModel) {
        String newZip;
        if(addressModel != null && addressModel.getPostalcode() != null) {
            if(addressModel.getPostalcode().contains("-")) {
                newZip = addressModel.getPostalcode().split("-")[0];
            } else {
                newZip = addressModel.getPostalcode();
            }
        } else {
             newZip = StringUtils.EMPTY;
        }
        return newZip;
    }

    /**
     * This method will return warehouse code
     *
     * @param warehouseModel delivery mode
     * @return result
     */
    private int getWarehouseCode(final WarehouseModel warehouseModel) {
        if (warehouseModel != null) {
            return warehouseModel.getCode().contains("_ca") ? BlInventoryScanLoggingConstants.ONE : BlInventoryScanLoggingConstants.TWO;
        }
        return BlInventoryScanLoggingConstants.ZERO;
    }

    /**
     * This method will return carrier Id
     *
     * @param zoneDeliveryModeModel delivery mode
     * @return result
     */
    private int getCarrierId(final ZoneDeliveryModeModel zoneDeliveryModeModel) {
        if (zoneDeliveryModeModel != null) {
            if (zoneDeliveryModeModel.getCarrier() != null) {
                return CarrierEnum.UPS.getCode().equals(zoneDeliveryModeModel.getCarrier().getCode()) ? BlInventoryScanLoggingConstants.TWO :
                        BlInventoryScanLoggingConstants.ONE;
            } else {
                return BlInventoryScanLoggingConstants.TWO;
            }
        }
        return BlInventoryScanLoggingConstants.ZERO;
    }

    public BlDeliveryModeService getZoneDeliveryModeService() {
        return zoneDeliveryModeService;
    }

    public void setZoneDeliveryModeService(BlDeliveryModeService zoneDeliveryModeService) {
        this.zoneDeliveryModeService = zoneDeliveryModeService;
    }

    public BlDatePickerService getBlDatePickerService() {
        return blDatePickerService;
    }

    public void setBlDatePickerService(BlDatePickerService blDatePickerService) {
        this.blDatePickerService = blDatePickerService;
    }

    public BlCommerceStockService getBlCommerceStockService() {
        return blCommerceStockService;
    }

    public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
        this.blCommerceStockService = blCommerceStockService;
    }
}
