package com.bl.core.shipping.strategy.impl;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.AddressTypeEnum;
import com.bl.core.enums.CarrierEnum;
import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.core.enums.OptimizedShippingTypeEnum;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.shipping.strategy.BlShippingOptimizationStrategy;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import de.hybris.platform.core.model.ShippingOptimizationModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.internal.service.AbstractBusinessService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

public class DefaultBlShippingOptimizationStrategy extends AbstractBusinessService implements BlShippingOptimizationStrategy {

    private BlDeliveryModeService zoneDeliveryModeService;
    private BlDatePickerService blDatePickerService;
    private BlCommerceStockService blCommerceStockService;

    /**
     * {@inheritDoc}
     */
    @Override
    public SourcingLocation getProductAvailabilityForThreeDayGround(final SourcingContext context, final SourcingLocation sourcingLocation) {
        if(context != null && CollectionUtils.isNotEmpty(context.getOrderEntries())) {
            final Optional<AbstractOrderEntryModel> orderEntryModel = context.getOrderEntries().stream().findFirst();
            if(orderEntryModel.isPresent()) {
                final AbstractOrderModel order = orderEntryModel.get().getOrder();
                return order != null ? getUpdatedSourcingLocation(sourcingLocation, order,
                        BlDateTimeUtils.subtractDaysInRentalDates(BlCoreConstants.SKIP_THREE_DAYS,
                                order.getRentalStartDate().toString(), getBlDatePickerService().getListOfBlackOutDates()))
                        : setFalseSourcingLocation(sourcingLocation);
            }
        }
        return setFalseSourcingLocation(sourcingLocation);
    }

    /**
     * This method will fetch Stock for entry of order
     *
     * @param sourcingLocation consignment details
     * @param order order details
     * @param rentalStart date
     * @return Sourcing Location
     */
    private SourcingLocation getUpdatedSourcingLocation(final SourcingLocation sourcingLocation, final AbstractOrderModel order,
                                                        final Date rentalStart) {
        final int result = BlDateTimeUtils.getBusinessDaysDifferenceWithCutOffTime(BlDateTimeUtils.convertStringDateToDate(
                BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
                BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN), rentalStart, sourcingLocation.getWarehouse().getCutOffTime());

        if(result >= BlInventoryScanLoggingConstants.THREE) {
            final Map<ProductModel, Long> availability = sourcingLocation.getAvailability();
            if(availability != null) {
                final Set<String> productCodes = getProductCodes(availability);
                final Collection<StockLevelModel> threeStockLevelModels = getStockLevelModels(sourcingLocation, order, productCodes);
                if(CollectionUtils.isNotEmpty(productCodes) && CollectionUtils.isNotEmpty(threeStockLevelModels)) {
                    return getSourcingLocationForStock(sourcingLocation, availability, threeStockLevelModels.stream().collect(
                                Collectors.groupingBy(StockLevelModel::getProductCode)));
                }
            }
        }
        return setFalseSourcingLocation(sourcingLocation);
    }

    /**
     * This method will validate total stock against required stock and set in map
     *
     * @param sourcingLocation consignment details
     * @param availability map
     * @param stockLevelsProductWise map
     * @return SourcingLocation
     */
    private SourcingLocation getSourcingLocationForStock(final SourcingLocation sourcingLocation, final Map<ProductModel, Long> availability,
                                                         final Map<String, List<StockLevelModel>> stockLevelsProductWise) {
        if(stockLevelsProductWise != null) {
            Map<String, List<StockLevelModel>> availabilityMap = new HashMap<>();
            int i = BlInventoryScanLoggingConstants.ZERO;
            for (Map.Entry<ProductModel, Long> entry : availability.entrySet()) {
                if(stockLevelsProductWise.get(entry.getKey().getCode()).size() >= entry.getValue()) {
                    availabilityMap.put(entry.getKey().getCode(), stockLevelsProductWise.get(entry.getKey().getCode()));
                } else {
                    i = BlInventoryScanLoggingConstants.ONE;
                    break;
                }
            }
            return getSourcingLocationForGroundStock(sourcingLocation, i, availabilityMap);
        }
        return setFalseSourcingLocation(sourcingLocation);
    }

    /**
     * This method will decide result of stock checking and put appropriate details in AvailabilityMap.
     *
     * @param sourcingLocation consignment details
     * @param i value
     * @param availabilityMap map
     * @return SourcingLocation
     */
    private SourcingLocation getSourcingLocationForGroundStock(final SourcingLocation sourcingLocation, final int i,
                                                               final Map<String, List<StockLevelModel>> availabilityMap) {
        if(i == BlInventoryScanLoggingConstants.ZERO) {
            sourcingLocation.setGroundAvailability(Boolean.TRUE);
            sourcingLocation.setGroundAvailabilityCode(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode());
            sourcingLocation.setAvailabilityMap(availabilityMap);
            return sourcingLocation;
        } else {
            return setFalseSourcingLocation(sourcingLocation);
        }
    }

    /**
     * This method will fetch stock for 3 day ground
     *
     * @param sourcingLocation consignment details
     * @param order details
     * @param productCodes set of strings
     * @return Collection<StockLevelModel>
     */
    private Collection<StockLevelModel> getStockLevelModels(final SourcingLocation sourcingLocation, final AbstractOrderModel order,
                                                            final Set<String> productCodes) {
        return getBlCommerceStockService().getStockForProductCodesAndDate(productCodes, sourcingLocation.getWarehouse(),
                BlDateTimeUtils.subtractDaysInRentalDates(BlCoreConstants.SKIP_THREE_DAYS, order.getRentalStartDate().toString(),
                        getBlDatePickerService().getListOfBlackOutDates()), BlDateTimeUtils.addDaysInRentalDates(BlCoreConstants.SKIP_THREE_DAYS,
                        order.getRentalEndDate().toString(), getBlDatePickerService().getListOfBlackOutDates()));
    }

    /**
     * This method will take ProductModel and will return set of string of product code
     *
     * @param availability map
     * @return Set<String>
     */
    private Set<String> getProductCodes(final Map<ProductModel, Long> availability) {
        final Set<String> productCodes = new HashSet<>();
        for(ProductModel model : availability.keySet()) {
            productCodes.add(model.getCode());
        }
        return productCodes;
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
        if(BlDateTimeUtils.compareTimeWithCutOff(consignmentModel.getWarehouse().getCutOffTime())) {
            final Date rentalStartDate = consignmentModel.getOrder().getRentalStartDate();
            final int result = BlDateTimeUtils.getBusinessDaysDifferenceWithCutOffTime(consignmentModel.getOptimizedShippingStartDate(),
                    rentalStartDate, consignmentModel.getWarehouse().getCutOffTime());
            return getOptimizedShippingMethod(consignmentModel, getBlDatePickerService().getListOfBlackOutDates(), rentalStartDate,
                    consignmentModel.getOrder().getRentalEndDate(), result, consignmentModel.getOptimizedShippingMethod(),
                    getCarrierId((ZoneDeliveryModeModel) consignmentModel.getDeliveryMode()), getWarehouseCode(consignmentModel.getWarehouse()),
                    getAddressZip(consignmentModel.getShippingAddress()));
        }
        return false;
    }

    /**
     * This method will check existing assigned optimized shipping method and assign another optimization
     *
     * @param consignmentModel details
     * @param blackOutDates list
     * @param rentalStartDate date
     * @param rentalEndDate date
     * @param result difference
     * @param optimizedShippingMethodEnum code
     * @param carrierId UPS/FEDEX
     * @param warehouseCode CA/MA
     * @param addressZip code
     * @return true if success
     */
    private boolean getOptimizedShippingMethod(final ConsignmentModel consignmentModel, final List<Date> blackOutDates,
                                               final Date rentalStartDate, final Date rentalEndDate, final int result,
                                               final OptimizedShippingMethodEnum optimizedShippingMethodEnum, final int carrierId,
                                               final int warehouseCode, final String addressZip) {
        if(optimizedShippingMethodEnum.equals(OptimizedShippingMethodEnum.THREE_DAY_GROUND)) {
            return checkTwoDayGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else if(optimizedShippingMethodEnum.equals(OptimizedShippingMethodEnum.TWO_DAY_GROUND)){
            return checkOvernightGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else if(optimizedShippingMethodEnum.equals(OptimizedShippingMethodEnum.ONE_DAY_GROUND)){
            if(result == BlInventoryScanLoggingConstants.ZERO) {
                setOptimizedDetailsOnConsignment(consignmentModel, result, BlDateTimeUtils.subtractDaysInRentalDates(result,
                        rentalStartDate.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(result,
                        rentalEndDate.toString(), blackOutDates), OptimizedShippingMethodEnum.DEFAULT);
                return true;
            } else {
                return result == BlInventoryScanLoggingConstants.ONE ? checkNextDayAir(consignmentModel, rentalStartDate,
                        rentalEndDate) : checkTwoDayAir(consignmentModel, rentalStartDate, rentalEndDate);
            }
        } else if(optimizedShippingMethodEnum.equals(OptimizedShippingMethodEnum.TWO_DAY_AIR) ||
                optimizedShippingMethodEnum.equals(OptimizedShippingMethodEnum.TWO_DAY_AIR_AM)) {
            return checkNextDayAir(consignmentModel, rentalStartDate, rentalEndDate);
        } else {
            setOptimizedDetailsOnConsignment(consignmentModel, result, BlDateTimeUtils.subtractDaysInRentalDates(
                    result, rentalStartDate.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(result,
                    rentalEndDate.toString(), blackOutDates), OptimizedShippingMethodEnum.DEFAULT);
            return true;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getOptimizedShippingMethodForOrder(final ConsignmentModel consignmentModel) {
        final Date rentalStartDate = consignmentModel.getOrder().getRentalStartDate();
        final Date rentalEndDate = consignmentModel.getOrder().getRentalEndDate();
        final int result = BlDateTimeUtils.getBusinessDaysDifferenceWithCutOffTime(consignmentModel.getOptimizedShippingStartDate(),
                rentalStartDate, consignmentModel.getWarehouse().getCutOffTime());
        final int carrierId = getCarrierId((ZoneDeliveryModeModel) consignmentModel.getDeliveryMode());
        final int warehouseCode = getWarehouseCode(consignmentModel.getWarehouse());
        final String addressZip = getAddressZip(consignmentModel.getShippingAddress());

        if(result >= BlInventoryScanLoggingConstants.THREE) {
            return checkThreeDayGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else if(result == BlInventoryScanLoggingConstants.TWO) {
            return checkTwoDayGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else if(result == BlInventoryScanLoggingConstants.ONE) {
            return checkOvernightGround(result, carrierId, warehouseCode, addressZip, consignmentModel, rentalStartDate, rentalEndDate);
        } else {
            return checkTwoDayAir(consignmentModel, rentalStartDate, rentalEndDate);
        }
    }

    /**
     * This method will check three day ground
     *
     * @param result day difference
     * @param carrierId UPS/FedEx
     * @param warehouseCode CA/MA
     * @param customerZip code
     * @param consignmentModel order
     * @param rentalStart date
     * @param rentalEnd date
     * @return true if success
     */
    private boolean checkThreeDayGround(final int result, final int carrierId, final int warehouseCode, final String customerZip,
                                     final ConsignmentModel consignmentModel, final Date rentalStart, final Date rentalEnd) {
        final ShippingOptimizationModel shippingOptimizationModel = getZoneDeliveryModeService().getOptimizedShippingRecord(
                carrierId, warehouseCode, customerZip, result, BlInventoryScanLoggingConstants.ONE);
        if(shippingOptimizationModel != null) {
            final List<Date> blackOutDates = getBlDatePickerService().getListOfBlackOutDates();
            setOptimizedDetailsOnConsignment(consignmentModel, result, BlDateTimeUtils.subtractDaysInRentalDates(
                        BlCoreConstants.SKIP_THREE_DAYS, rentalStart.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                        BlCoreConstants.SKIP_THREE_DAYS, rentalEnd.toString(), blackOutDates), OptimizedShippingMethodEnum.THREE_DAY_GROUND);
            return true;
        } else {
            return checkTwoDayGround(BlInventoryScanLoggingConstants.TWO, carrierId, warehouseCode, customerZip, consignmentModel,
                    rentalStart, rentalEnd);
        }
    }

    /**
     * This method will check two day ground
     *
     * @param result day difference
     * @param carrierId UPS/FedEx
     * @param warehouseCode CA/MA
     * @param customerZip code
     * @param consignmentModel order
     * @param rentalStart date
     * @param rentalEnd date
     * @return true if success
     */
    private boolean checkTwoDayGround(final int result, final int carrierId, final int warehouseCode, final String customerZip,
                                   final ConsignmentModel consignmentModel, final Date rentalStart, final Date rentalEnd) {
        final ShippingOptimizationModel shippingOptimizationModel = getZoneDeliveryModeService().getOptimizedShippingRecord(
                carrierId, warehouseCode, customerZip, result, BlInventoryScanLoggingConstants.ONE);
        if(shippingOptimizationModel != null) {
            final List<Date> blackOutDates = getBlDatePickerService().getListOfBlackOutDates();
            setOptimizedDetailsOnConsignment(consignmentModel, result, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalStart.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalEnd.toString(), blackOutDates), OptimizedShippingMethodEnum.TWO_DAY_GROUND);
            return true;
        } else {
            return checkOvernightGround(BlInventoryScanLoggingConstants.ONE, carrierId, warehouseCode, customerZip, consignmentModel,
                    rentalStart, rentalEnd);
        }
    }

    /**
     * This method will check one day ground
     *
     * @param result day difference
     * @param carrierId UPS/FedEx
     * @param warehouseCode CA/MA
     * @param customerZip code
     * @param consignmentModel order
     * @param rentalStart date
     * @param rentalEnd date
     * @return true if success
     */
    private boolean checkOvernightGround(final int result, final int carrierId, final int warehouseCode, final String customerZip,
                                      final ConsignmentModel consignmentModel, final Date rentalStart, final Date rentalEnd) {
        final ShippingOptimizationModel shippingOptimizationModel = getZoneDeliveryModeService().getOptimizedShippingRecord(
                carrierId, warehouseCode, customerZip, result, BlInventoryScanLoggingConstants.ONE);
        if(shippingOptimizationModel != null) {
            final List<Date> blackOutDates = getBlDatePickerService().getListOfBlackOutDates();
            setOptimizedDetailsOnConsignment(consignmentModel, result, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalStart.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalEnd.toString(), blackOutDates), OptimizedShippingMethodEnum.ONE_DAY_GROUND);
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
     * @param rentalStart date
     * @param rentalEnd date
     * @return true if success
     */
    private boolean checkTwoDayAir(final ConsignmentModel consignmentModel, final Date rentalStart, final Date rentalEnd) {
        final AddressModel shippingAddress = consignmentModel.getOrder().getDeliveryAddress();
        final List<Date> blackOutDates = getBlDatePickerService().getListOfBlackOutDates();
        if (shippingAddress != null && AddressTypeEnum.BUSINESS.getCode().equals(shippingAddress.getAddressType().getCode())) {
            setOptimizedDetailsOnConsignment(consignmentModel, BlInventoryScanLoggingConstants.TWO, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalStart.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalEnd.toString(), blackOutDates), OptimizedShippingMethodEnum.TWO_DAY_AIR_AM);
        } else {
            setOptimizedDetailsOnConsignment(consignmentModel, BlInventoryScanLoggingConstants.TWO, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalStart.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_TWO_DAYS, rentalEnd.toString(), blackOutDates), OptimizedShippingMethodEnum.TWO_DAY_AIR);
        }
        return true;
    }

    /**
     * This method will check next day air ot AM
     *
     * @param consignmentModel order details
     * @param rentalStart date
     * @param rentalEnd date
     * @return true if success
     */
    private boolean checkNextDayAir(final ConsignmentModel consignmentModel, final Date rentalStart, final Date rentalEnd) {
        final AddressModel shippingAddress = consignmentModel.getOrder().getDeliveryAddress();
        final List<Date> blackOutDates = getBlDatePickerService().getListOfBlackOutDates();
        if (shippingAddress != null && AddressTypeEnum.BUSINESS.getCode().equals(shippingAddress.getAddressType().getCode())) {
            setOptimizedDetailsOnConsignment(consignmentModel, BlInventoryScanLoggingConstants.TWO, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalStart.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalEnd.toString(), blackOutDates), OptimizedShippingMethodEnum.NEXT_DAY_AIR_AM);
        } else {
            setOptimizedDetailsOnConsignment(consignmentModel, BlInventoryScanLoggingConstants.TWO, BlDateTimeUtils.subtractDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalStart.toString(), blackOutDates), BlDateTimeUtils.addDaysInRentalDates(
                    BlCoreConstants.SKIP_ONE_DAYS, rentalEnd.toString(), blackOutDates), OptimizedShippingMethodEnum.NEXT_DAY_AIR);
        }
        return true;
    }

    /**
     * This method with set details and save model
     *
     * @param consignmentModel order
     * @param result of days
     * @param optimizedDate date
     * @param optimizedShippingMethod methode
     */
    private void setOptimizedDetailsOnConsignment(final ConsignmentModel consignmentModel, final int result, final Date optimizedDate,
                                                  final Date optimizedEndDate, final OptimizedShippingMethodEnum optimizedShippingMethod) {
        consignmentModel.setOptimizedShippingStartDate(optimizedDate);
        consignmentModel.setOptimizedShippingEndDate(optimizedEndDate);
        if(result != BlInventoryScanLoggingConstants.ZERO) {
            final OptimizedShippingTypeEnum optimizedShippingType = checkConsignmentShippingType(consignmentModel, result);
            if (OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE.equals(optimizedShippingType)) {
                consignmentModel.setOptimizedShippingMethod(OptimizedShippingMethodEnum.ONE_DAY_GROUND);
            } else {
                consignmentModel.setOptimizedShippingMethod(optimizedShippingMethod);
            }
            consignmentModel.setOptimizedShippingType(optimizedShippingType);
        } else {
            consignmentModel.setOptimizedShippingType(null);
        }

        getModelService().save(consignmentModel);
        getModelService().refresh(consignmentModel);
    }

    /**
     * This method will
     *
     * @param consignmentModel order
     * @param result number of days
     * @return OptimizedShippingTypeEnum
     */
    private OptimizedShippingTypeEnum checkConsignmentShippingType(final ConsignmentModel consignmentModel, final int result) {
        final ZoneDeliveryModeModel zoneDeliveryModeModel = (ZoneDeliveryModeModel) consignmentModel.getDeliveryMode();
        if(zoneDeliveryModeModel != null) {
            final String shippingGroup = zoneDeliveryModeModel.getShippingGroup().getCode();
            if(BlDeliveryModeLoggingConstants.SHIP_HOME_HOTEL_BUSINESS.equals(shippingGroup)) {
                return OptimizedShippingTypeEnum.WAREHOUSE2CUSTOMER;
            } else if(BlDeliveryModeLoggingConstants.SHIP_HOLD_UPS_OFFICE.equals(shippingGroup)) {
                return OptimizedShippingTypeEnum.WAREHOUSE2PICKUP;
            } else if(BlDeliveryModeLoggingConstants.BL_PARTNER_PICKUP.equals(shippingGroup)){
                if(zoneDeliveryModeModel.getCode().startsWith("BL_")) {
                    return OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE;
                } else {
                    return OptimizedShippingTypeEnum.WAREHOUSE2PICKUP;
                }
            } else if(BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY.equals(shippingGroup)){
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
     * @param consignmentModel order
     * @param result no. of days
     * @param zoneDeliveryModeModel delivery method
     * @param days days
     * @return OptimizedShippingTypeEnum
     */
    private OptimizedShippingTypeEnum getSameAndNextDayOptimizedShippingType(final ConsignmentModel consignmentModel, final int result,
                                                                             final ZoneDeliveryModeModel zoneDeliveryModeModel, final int days) {
        if(result >= days) {
            if(zoneDeliveryModeModel.getWarehouse().getCode().equals(consignmentModel.getWarehouse().getCode())) {
                return OptimizedShippingTypeEnum.WAREHOUSE2CUSTOMER;
            } else {
                return OptimizedShippingTypeEnum.WAREHOUSE2WAREHOUSE;
            }
        } else {
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
        return addressModel != null && addressModel.getPostalcode() != null ? addressModel.getPostalcode() : StringUtils.EMPTY;
    }

    /**
     * This method will return warehouse code
     *
     * @param warehouseModel delivery mode
     * @return result
     */
    private int getWarehouseCode(final WarehouseModel warehouseModel) {
        if(warehouseModel != null) {
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
        if(zoneDeliveryModeModel != null) {
            if(zoneDeliveryModeModel.getCarrier() != null) {
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
