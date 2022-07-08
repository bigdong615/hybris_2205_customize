package com.bl.core.shipping.service.impl;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeValueModel;
import de.hybris.platform.order.impl.DefaultZoneDeliveryModeService;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.storelocator.model.PointOfServiceModel;

import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.blackout.date.dao.BlBlackoutDatesDao;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.data.StockResult;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.enums.CarrierEnum;
import com.bl.core.enums.ShippingCostEnum;
import com.bl.core.model.BlBlackoutDateModel;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.OptimizedShippingMethodModel;
import com.bl.core.model.PartnerPickUpStoreModel;
import com.bl.core.model.ShippingCostModel;
import com.bl.core.model.ShippingGroupModel;
import com.bl.core.model.ShippingOptimizationModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.shipping.dao.BlDeliveryModeDao;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.fexEx.data.SameDayCityReqData;
import com.bl.facades.fexEx.data.SameDayCityResData;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.integration.services.BlFedExSameDayService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;


/**
 * {javadoc}
 *
 * @auther Namrata Lohar
 */
public class DefaultBlDeliveryModeService extends DefaultZoneDeliveryModeService implements BlDeliveryModeService {

    private static final Logger LOG = Logger.getLogger(DefaultBlDeliveryModeService.class);

    @Resource(name = "blDeliveryModeDao")
    BlDeliveryModeDao blDeliveryModeDao;

    @Resource(name = "userService")
    transient UserService userService;

    @Resource(name = "fedExSameDayServiceImpl")
    BlFedExSameDayService blFedExSameDayService;

    private BlCommerceStockService blCommerceStockService;

    private BlCartService blCartService;

    private BaseStoreService baseStoreService;

    private BlBlackoutDatesDao blBlackoutDatesDao;

	 @Value("${shipping.sf.zip.code}")
    private String sf;

    @Value("${shipping.nyc.zip.code}")
    private String nyc;

    private BlDatePickerService blDatePickerService;

    private BlProductService productService;

	 private BlDeliveryModeService blDeliveryModeService;

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
                                                                        final String pstCutOffTime, final boolean payByCustomer) {
        return getBlZoneDeliveryModeDao().getShipToHomeDeliveryModes(carrier, mode, pstCutOffTime, payByCustomer);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesForUsedGear(final String carrier, final String mode,
                                                                                   final boolean payByCustomer) {
        return getBlZoneDeliveryModeDao().getShipToHomeDeliveryModesForUsedGear(carrier, mode, payByCustomer);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesNotLike(final String carrier, final String mode,
                                                                               final String pstCutOffTime, final boolean payByCustomer) {
        return getBlZoneDeliveryModeDao().getShipToHomeDeliveryModesNotLike(carrier, mode, pstCutOffTime, payByCustomer);
    }

    /**
     * {@inheritDoc}
     *
     * @return
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getAllShipToHomeDeliveryModesWithRentalDates(final String rentalStart,
                                                                                          final String rentalEnd,
                                                                                          final boolean payByCustomer) {

        final Collection<ZoneDeliveryModeModel> allDeliveryModes = new ArrayList<>();
        final Collection<ZoneDeliveryModeModel> upsZoneDeliveryModes = getShipToHomeDeliveryModesWithRentalDates(rentalStart,
                rentalEnd, String.valueOf(CarrierEnum.UPS), payByCustomer).stream().filter(model -> checkDaysToSkipForDeliveryMode(model, rentalStart, rentalEnd)).
                collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(upsZoneDeliveryModes)) {
            allDeliveryModes.addAll(upsZoneDeliveryModes);
        }
        final Collection<ZoneDeliveryModeModel> fedexZoneDeliveryModes = getShipToHomeDeliveryModesWithRentalDates(rentalStart,
                rentalEnd, String.valueOf(CarrierEnum.FEDEX), payByCustomer).stream().filter(model -> checkDaysToSkipForDeliveryMode(model, rentalStart, rentalEnd))
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
    public Collection<ZoneDeliveryModeModel> getAllShipToHomeDeliveryModesWithoutRentalDates(final boolean payByCustomer) {
        final Collection<ZoneDeliveryModeModel> allDeliveryModes = new ArrayList<>();
        final Collection<ZoneDeliveryModeModel> upsZoneDeliveryModes = getShipToHomeDeliveryModesWithoutRentalDates(
                String.valueOf(CarrierEnum.UPS), payByCustomer);
        if (CollectionUtils.isNotEmpty(upsZoneDeliveryModes)) {
            allDeliveryModes.addAll(upsZoneDeliveryModes);
        }
        final Collection<ZoneDeliveryModeModel> fedexZoneDeliveryModes = getShipToHomeDeliveryModesWithoutRentalDates(
                String.valueOf(CarrierEnum.FEDEX), payByCustomer);
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
                                                                                       final String carrier,
                                                                                       final boolean payByCustomer) {
        final String pstCutOffTime = BlDateTimeUtils.getCurrentTimeUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST);
		  int result = checkDateForRental(
				  BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
                rentalStart);
		  final DayOfWeek currentDayOfWeek = BlDateTimeUtils.getDayOfWeek(BlDeliveryModeLoggingConstants.ZONE_PST,
				  new Date().toString());

		  final ZoneDeliveryModeModel zoneDeliveryMode = blDeliveryModeService
				  .getZoneDeliveryMode(ShippingCostEnum.UPS_OVERNIGHT_ROUND_TRIP.getCode());
		  if (result == 2 && zoneDeliveryMode != null ? !BlDateTimeUtils.compareTimeWithCutOff(zoneDeliveryMode.getCutOffTime())
				  : Boolean.FALSE)
		  {
			  result = 1;
		  }

        if (result >= BlInventoryScanLoggingConstants.TWO) {
            return getShipToHomeDeliveryModes(carrier, BlDeliveryModeLoggingConstants.DELIVERY_TYPE_STANDARD,
                    null, payByCustomer);
        } else if (result == BlInventoryScanLoggingConstants.ONE) {
			  // final DayOfWeek currentDayOfWeek = BlDateTimeUtils.getDayOfWeek(BlDeliveryModeLoggingConstants.ZONE_PST, new Date().toString());
            if (currentDayOfWeek.equals(DayOfWeek.SUNDAY) || currentDayOfWeek.equals(DayOfWeek.SATURDAY)) {
                return getShipToHomeDeliveryModes(carrier, BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT,
                        null, payByCustomer);
            } else {
                return getShipToHomeDeliveryModes(carrier, BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT,
                        pstCutOffTime, payByCustomer);
            }
        }
        return Collections.emptyList();
    }


    /**
     * {@inheritDoc}
     *
     * @return
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesWithoutRentalDates(final String carrier,
                                                                                          final boolean payByCustomer) {
        return getShipToHomeDeliveryModesForUsedGear(carrier, BlDeliveryModeLoggingConstants.DELIVERY_TYPE_STANDARD, payByCustomer);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModes(final String mode, final String pstCutOffTime,
                                                                                         final boolean payByCustomer) {
        return getBlZoneDeliveryModeDao().getPartnerZoneUPSStoreDeliveryModes(mode, pstCutOffTime, payByCustomer);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModesForUsedGear(final String mode,
                                                                                                    final boolean payByCustomer) {

        return getBlZoneDeliveryModeDao().getPartnerZoneUPSStoreDeliveryModesForUsedGear(mode, payByCustomer);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModesNotLike(final String mode,
                                                                                                final String pstCutOffTime,
                                                                                                final boolean payByCustomer) {
        return getBlZoneDeliveryModeDao().getPartnerZoneUPSStoreDeliveryModesNotLike(mode, pstCutOffTime, payByCustomer);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerPickUpDeliveryModesWithRentalDates(final String rentalStart,
                                                                                                  final String rentalEnd,
                                                                                                  final boolean payByCustomer) {
        final String pstCutOffTime = BlDateTimeUtils.getCurrentTimeUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST);
        final int result = checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
                rentalStart);
        if (result >= BlInventoryScanLoggingConstants.TWO) {
            return getPartnerZoneUPSStoreDeliveryModes(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_STANDARD,
                    null, payByCustomer);
        } else if (result == BlInventoryScanLoggingConstants.ONE) {
            final DayOfWeek currentDayOfWeek = BlDateTimeUtils.getDayOfWeek(BlDeliveryModeLoggingConstants.ZONE_PST, new Date().toString());
            if (currentDayOfWeek.equals(DayOfWeek.SUNDAY) || currentDayOfWeek.equals(DayOfWeek.SATURDAY)) {
                return getPartnerZoneUPSStoreDeliveryModes(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT,
                        null, payByCustomer);
            } else {
                return getPartnerZoneUPSStoreDeliveryModes(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT,
                        pstCutOffTime, payByCustomer);
            }
        }
        return Collections.emptyList();
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerPickUpDeliveryModesWithoutRentalDates(final boolean payByCustomer) {
        return getPartnerZoneUPSStoreDeliveryModesForUsedGear(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_STANDARD, payByCustomer);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getAllPartnerPickUpDeliveryModesWithRentalDatesForUPSStore(final String rentalStart,
                                                                                                                final String rentalEnd,
                                                                                                                final boolean payByCustomer) {
   	 return getPartnerPickUpDeliveryModesWithRentalDates(rentalStart, rentalEnd, payByCustomer).stream()
                .filter(model -> checkDaysToSkipForDeliveryMode(model, rentalStart, rentalEnd)).collect(Collectors.toList());
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getAllPartnerPickUpDeliveryModesWithoutRentalDatesForUPSStore(
            final boolean payByCustomer) {
        return getPartnerPickUpDeliveryModesWithoutRentalDates(payByCustomer);
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
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModes(final String partnerZone, final String rentalStart,
                                                                                 final String rentalEnd, final boolean payByCustomer) {
        final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeDao().
                getPartnerZoneDeliveryModes(partnerZone, payByCustomer);
        if (CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels)) {
            final Collection<BlPickUpZoneDeliveryModeModel> newBlPickUpZoneDeliveryModeModels = new ArrayList<>(blPickUpZoneDeliveryModeModels);
            final int result = checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date())
                    , rentalStart);
            for (final BlPickUpZoneDeliveryModeModel pickUpZoneDeliveryModeModel : blPickUpZoneDeliveryModeModels) {
                checkDeliveryModeValidityOfTypePartner(newBlPickUpZoneDeliveryModeModels, result, pickUpZoneDeliveryModeModel);
            }
            return CollectionUtils.isNotEmpty(newBlPickUpZoneDeliveryModeModels) ? newBlPickUpZoneDeliveryModeModels.stream()
                    .filter(model -> checkDaysToSkipForDeliveryMode(model, rentalStart, rentalEnd)).collect(Collectors.toList()) : Collections.emptyList();
        }
        return Collections.emptyList();
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModesForUsedGear(final String partnerZone,
                                                                                            final boolean payByCustomer) {
        final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeDao()
                .getPartnerZoneDeliveryModes(partnerZone, payByCustomer);
        if (CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels)) {
            final Collection<BlPickUpZoneDeliveryModeModel> newBlPickUpZoneDeliveryModeModels = new ArrayList<>(
                    blPickUpZoneDeliveryModeModels);
            return CollectionUtils.isNotEmpty(newBlPickUpZoneDeliveryModeModels) ? newBlPickUpZoneDeliveryModeModels
                    : Collections.emptyList();

        }
        return Collections.emptyList();
    }

    /**
     * Get delivery Zone for new Gear product.
     * @param partnerZone
     * @param payByCustomer
     * @return
     */
    @Override
    public Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModesForNewGear(final String partnerZone,
        final boolean payByCustomer) {
        final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels = getBlZoneDeliveryModeDao()
            .getPartnerZoneDeliveryModes(partnerZone, payByCustomer);
        if (CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels)) {
            final Collection<BlPickUpZoneDeliveryModeModel> newBlPickUpZoneDeliveryModeModels = new ArrayList<>();
            blPickUpZoneDeliveryModeModels.forEach(blPickUpZoneDeliveryModeModel ->{
                if(blPickUpZoneDeliveryModeModel.isWarehousePickUp()){
                    newBlPickUpZoneDeliveryModeModels.add(blPickUpZoneDeliveryModeModel);
                }
            });
            return CollectionUtils.isNotEmpty(newBlPickUpZoneDeliveryModeModels) ? newBlPickUpZoneDeliveryModeModels
                : Collections.emptyList();

        }
        return Collections.emptyList();
    }
    /**
     * This method will check conditions for partner delivery locations
     *
     * @param blPickUpZoneDeliveryModeModels collection to remove unwanted records
     * @param result                         business days difference
     * @param pickUpZoneDeliveryModeModel    current model to check with condition
     */
    private void checkDeliveryModeValidityOfTypePartner(final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels,
                                                        final int result, final BlPickUpZoneDeliveryModeModel pickUpZoneDeliveryModeModel) {
        if (pickUpZoneDeliveryModeModel.isWarehousePickUp()) {
            if (result == BlInventoryScanLoggingConstants.ZERO && !BlDateTimeUtils.compareTimeWithCutOff(
                    pickUpZoneDeliveryModeModel.getCutOffTime())) {
                blPickUpZoneDeliveryModeModels.remove(pickUpZoneDeliveryModeModel);
            }
        } else {
            if (result <= BlInventoryScanLoggingConstants.TWO) {
                blPickUpZoneDeliveryModeModels.remove(pickUpZoneDeliveryModeModel);
            }
        }
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
    public Collection<BlRushDeliveryModeModel> getBlRushDeliveryModes(final String deliveryMode, final String pstCutOffTime,
                                                                      final String rentalStart, final String rentalEnd,
                                                                      final boolean payByCustomer) {

   	 return getBlZoneDeliveryModeDao().getBlRushDeliveryModes(deliveryMode, pstCutOffTime, payByCustomer).stream()
                .filter(model -> checkDaysToSkipForDeliveryMode(model, rentalStart, rentalEnd)).collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlRushDeliveryModeModel> getBlRushDeliveryModesForUsedGear(final String deliveryMode,
                                                                                 final boolean payByCustomer) {

        return getBlZoneDeliveryModeDao().getBlRushDeliveryModesForUsedGear(deliveryMode, payByCustomer);
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public ShippingCostModel getShippingCostForCalculatedDeliveryCost(final String calculatedCost,
                                                                      final ZoneDeliveryModeModel deliveryMethod) {
        return getBlZoneDeliveryModeDao().getShippingCostForCalculatedDeliveryCost(calculatedCost,
                deliveryMethod.getShippingCostCode().getCode());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getShippingCostAmount(final AbstractOrderModel order, final DeliveryModeModel deliveryMode) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "getShippingCostAmount", deliveryMode.getCode());
        if (deliveryMode instanceof BlPickUpZoneDeliveryModeModel) {
            final BlPickUpZoneDeliveryModeModel zoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) deliveryMode;
            return getAmountForAppropriateZoneModel(order, zoneDeliveryModeModel);
        } else if (deliveryMode instanceof BlRushDeliveryModeModel) {
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
     *
     * @return
     */
    @Override
    public double getAmountForAppropriateZoneModel(final AbstractOrderModel order, final ZoneDeliveryModeModel zoneDeliveryModeModel) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "getAmountForAppropriateZoneModel", zoneDeliveryModeModel.getCode());
        if (zoneDeliveryModeModel.isPayByCustomer()) {
            return getPayByCustomerShippingCost(order, zoneDeliveryModeModel);
        }
        return BlInventoryScanLoggingConstants.ZERO;
    }

    /**
     * This method will get cosy for only pay by customer delivery methods
     *
     * @param order                 model
     * @param zoneDeliveryModeModel delivery model
     * @return shipping amount
     */
    private double getPayByCustomerShippingCost(final AbstractOrderModel order, final ZoneDeliveryModeModel zoneDeliveryModeModel) {
        for (final ZoneDeliveryModeValueModel valueModel : zoneDeliveryModeModel.getValues()) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "getPayByCustomerShippingCost", valueModel.getPk());
            if (!valueModel.isFixedAmount()) {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO, "getPayByCustomerShippingCost : Inside", valueModel.getPk());
                final Map<String, Double> calculatedValueMap;
                try {
                    calculatedValueMap = getCalculatedWeightForDelivery(order);
                    final Double shippingCostModel = getShippingAmount(order, zoneDeliveryModeModel, calculatedValueMap);
                    return shippingCostModel != null ? Math.max(shippingCostModel, valueModel.getMinimum()) :
                            valueModel.getMinimum();
                } catch (final Exception e) {
                    BlLogger.logMessage(LOG, Level.ERROR, "Exception while calculating delivery cost");
                }
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "ShippingCostModel is null, Shipping amount: {} ",
                        BlInventoryScanLoggingConstants.ZERO);
                return BlInventoryScanLoggingConstants.ZERO;
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Shipping fixed amount: {} ", valueModel.getValue());
                return valueModel.getValue();
            }
        }
        return BlInventoryScanLoggingConstants.ZERO;
    }

    /**
     * This method will give shipping amount
     *
     * @param order                 order
     * @param zoneDeliveryModeModel shipping method
     * @param calculatedValueMap    calculated value map
     * @return shipping cost
     */
    private Double getShippingAmount(final AbstractOrderModel order, final ZoneDeliveryModeModel zoneDeliveryModeModel,
                                     final Map<String, Double> calculatedValueMap) {
   	 final String maxValue = String.valueOf(Math.max(
             calculatedValueMap.get(BlDeliveryModeLoggingConstants.TOTAL_WEIGHT), calculatedValueMap.get(
                     BlDeliveryModeLoggingConstants.DIMENSIONAL_WEIGHT)));
        if (!(order instanceof CartModel)) {
            order.setTotalWeight(calculatedValueMap.get(BlDeliveryModeLoggingConstants.TOTAL_WEIGHT));
            order.setDimensionalWeight(calculatedValueMap.get(BlDeliveryModeLoggingConstants.DIMENSIONAL_WEIGHT));
        }
        final ShippingCostModel shippingCostModel = getShippingCostForCalculatedDeliveryCost(maxValue, zoneDeliveryModeModel);
        if (shippingCostModel != null) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Shipping calculated amount: {} ", shippingCostModel.getAmount());
            if (BooleanUtils.isFalse(order.getIsRentalOrder())) {
                return (shippingCostModel.getAmount() * Double.valueOf(maxValue) / BlInventoryScanLoggingConstants.TWO);
            }
            return shippingCostModel.getAmount() * Double.valueOf(maxValue);
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
        Collection<AbstractOrderEntryModel> abstractOrderEntryModels;
        BigDecimal totalWeight = new BigDecimal(BlInventoryScanLoggingConstants.ZERO);
        int maxLength = BlInventoryScanLoggingConstants.ZERO;
        int maxHeight = BlInventoryScanLoggingConstants.ZERO;
        int sumWidth = BlInventoryScanLoggingConstants.ZERO;
        try {
            if (order instanceof CartModel) {
                final CartModel cart = (CartModel) order;
                abstractOrderEntryModels = cart.getEntries();
            } else {
                abstractOrderEntryModels = order.getEntries();
            }
            for (final Iterator<AbstractOrderEntryModel> iterator = abstractOrderEntryModels.iterator(); iterator.hasNext();)
				{
					final AbstractOrderEntryModel entry = iterator.next();
					final BlProductModel blSerialProduct = (BlProductModel) entry.getProduct();

                    totalWeight = getBigDecimal(totalWeight, entry);
                    if(blSerialProduct instanceof BlSerialProductModel) {
                        final BlProductModel blProduct =  (((BlSerialProductModel) blSerialProduct).getBlProduct());
                        sumWidth = getSumWidth(sumWidth, blProduct.getWidth(), entry.getQuantity().intValue());
                        maxHeight = getMaxHeight(maxHeight, blProduct.getHeight());
                        maxLength = getMaxLength(maxLength, blProduct.getLength());
                    }else if(null != blSerialProduct) {
                        sumWidth = getSumWidth(sumWidth, blSerialProduct.getWidth(), entry.getQuantity().intValue());
                        maxHeight = getMaxHeight(maxHeight, blSerialProduct.getHeight());
                        maxLength = getMaxLength(maxLength, blSerialProduct.getLength());
                    }

				}
            final double dimensionalWeight = ((double) (maxHeight * sumWidth * maxLength) /
                    getBlZoneDeliveryModeDao().getDimensionalFactorForDeliveryFromStore(BlDeliveryModeLoggingConstants.STORE));
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total weight: {} ", totalWeight.doubleValue());
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Dimensional weight: {} ", dimensionalWeight);

            valueMap.put(BlDeliveryModeLoggingConstants.TOTAL_WEIGHT, totalWeight.doubleValue());
            valueMap.put(BlDeliveryModeLoggingConstants.DIMENSIONAL_WEIGHT, dimensionalWeight);
        } catch (final Exception e) {
            BlLogger.logMessage(LOG, Level.ERROR, "Exception while calculating delivery cost");
        }
        return valueMap;
    }

    /**
     * This method will calulate total weight
     *
     * @param totalWeight     weight
     * @param entry product
     * @return big decimal
     */
    private BigDecimal getBigDecimal(final BigDecimal totalWeight, final AbstractOrderEntryModel entry) {
        final BlProductModel blSerialProduct = (BlProductModel) entry.getProduct();
        //Added condition to used gear products.
        double weight = BlInventoryScanLoggingConstants.ZERO;

        if(blSerialProduct instanceof BlSerialProductModel) {
            final BlProductModel serialProduct =  (((BlSerialProductModel) blSerialProduct).getBlProduct());
            if(null != serialProduct.getWeight()) {
                weight = serialProduct.getWeight().doubleValue() * entry.getQuantity();
                weight = totalWeight.doubleValue() + weight;
            }

            if (!(((BlSerialProductModel) entry.getProduct()).getBlProduct().getCode().equals(((BlSerialProductModel) blSerialProduct)
                    .getBlProduct().getCode()))) {
                weight = totalWeight.doubleValue() + weight;
            }
        }else{
            if(null != blSerialProduct.getWeight()) {
                weight = blSerialProduct.getWeight().doubleValue() * entry.getQuantity();
                weight = totalWeight.doubleValue() + weight;
            }
        }

        if (weight >= 0.0) {
            return BigDecimal.valueOf(weight);
        } else {
            return BigDecimal.valueOf(BlInventoryScanLoggingConstants.ZERO);
        }
    }

    /**
     * This method will calculate sum of all products width
     *
     * @param sumWidth width
     * @param width    width
     * @param qty      quantity
     * @return value
     */
    private int getSumWidth(final int sumWidth, final Integer width, final int qty) {
        return width != null && width > BlInventoryScanLoggingConstants.ZERO ? (sumWidth + (width * qty))
                : (sumWidth + (BlInventoryScanLoggingConstants.FIVE * qty));
    }

    /**
     * This method will calculate max height
     *
     * @param maxHeight height
     * @param height    height
     * @return height value
     */
    private int getMaxHeight(final int maxHeight, final Integer height) {
        return height != null && height > BlInventoryScanLoggingConstants.ZERO ? Math.max(height, maxHeight)
                : Math.max(BlInventoryScanLoggingConstants.FIVE, maxHeight);
    }

    /**
     * This method will calculate max length
     *
     * @param maxLength length
     * @param length    length
     * @return length value
     */
    private int getMaxLength(final int maxLength, final Integer length) {
        return length != null && length > BlInventoryScanLoggingConstants.ZERO ? Math.max(length, maxLength)
                : Math.max(BlInventoryScanLoggingConstants.FIVE, maxLength);
    }

    /**
     * This method will check delivery mode's model instance and send appropriate model further.
     *
     * @param deliveryMode model
     * @param rentalStart  start date
     * @param rentalEnd    the rental end
     * @return boolean for current delivery mode to disable or not
     */
    public boolean checkDaysToSkipForDeliveryMode(final DeliveryModeModel deliveryMode, final String rentalStart, final String rentalEnd) {
        final DayOfWeek currentDayOfWeek = BlDateTimeUtils.getDayOfWeek(BlDeliveryModeLoggingConstants.ZONE_PST, rentalStart);
        if (deliveryMode instanceof ZoneDeliveryModeModel && checkCartEntriesAvailability(rentalStart, rentalEnd, (ZoneDeliveryModeModel) deliveryMode)) {
            if (deliveryMode instanceof BlPickUpZoneDeliveryModeModel) {
                final BlPickUpZoneDeliveryModeModel zoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) deliveryMode;
                return getResultForDayToSkip(currentDayOfWeek, zoneDeliveryModeModel);
            } else if (deliveryMode instanceof BlRushDeliveryModeModel) {
                final BlRushDeliveryModeModel zoneDeliveryModeModel = (BlRushDeliveryModeModel) deliveryMode;
                return getResultForDayToSkip(currentDayOfWeek, zoneDeliveryModeModel);
            } else if (deliveryMode instanceof ZoneDeliveryModeModel) {
                final ZoneDeliveryModeModel zoneDeliveryModeModel = (ZoneDeliveryModeModel) deliveryMode;
                return getResultForDayToSkip(currentDayOfWeek, zoneDeliveryModeModel);
            }
        }
        return false;
    }

    /**
     * This mwthod will check current day against days to skip for delivery mode and if current day matches then that
     * delivery mode will be disabled for the day
     *
     * @param currentDayOfWeek      today
     * @param zoneDeliveryModeModel delivery mode
     * @return boolean, where delivery mode will be disabled or not
     */
    private boolean getResultForDayToSkip(final DayOfWeek currentDayOfWeek, final ZoneDeliveryModeModel zoneDeliveryModeModel) {
        final Collection<de.hybris.platform.cronjob.enums.DayOfWeek> dayOfWeeks = zoneDeliveryModeModel.getDaysToSkip();
        for (final de.hybris.platform.cronjob.enums.DayOfWeek day : dayOfWeeks) {
            if (day.getCode().equals(currentDayOfWeek.toString())) {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Current day is present in days to skip: {} ", currentDayOfWeek);
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
        if (validateZip(pinCode)) {
            try {
                final SameDayCityResData sameDayCityResData = getBlFedExSameDayService().getAvailability(getSameDayCityReqData(pinCode,
                        BlDeliveryModeLoggingConstants.SF.equals(deliveryType) ? sf : nyc));
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Checking same day fedex integration pinCode validity");
                return sameDayCityResData.getServiceApplicable() != null ? sameDayCityResData.getServiceApplicable() : Boolean.FALSE;
            } catch (final URISyntaxException e) {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Exception in Checking same day fedex integration pinCode validity", e);
                return false;
            }
        }
        return false;
    }

    /**
     * This method will take POS and will return zipCode of warehouse
     *
     * @param pos Point OF Service collection from warehouse
     * @return zipCode
     */
    private String getPOSAddress(final Collection<PointOfServiceModel> pos) {
        if (CollectionUtils.isNotEmpty(pos)) {
            for (final PointOfServiceModel model : pos) {
                final AddressModel addressModel = model.getAddress();
                if (addressModel != null) {
                    return addressModel.getPostalcode();
                }
            }
        }
        return null;
    }

    /**
     * This method will create Request data for Same Day city service
     *
     * @param pinCode validation zipcode
     * @param zipCode validation warehouse
     * @return Request data for service
     */
    private SameDayCityReqData getSameDayCityReqData(final String pinCode, final String zipCode) {
        final SameDayCityReqData sameDayCityReqData = new SameDayCityReqData();
        sameDayCityReqData.setDeliveryAddressZipCode(pinCode);
        sameDayCityReqData.setWarehouseZipCode(zipCode);
        return sameDayCityReqData;
    }

    /**
     * @param zipCode entered zipCode
     * @return true if success
     */
    private boolean validateZip(final String zipCode) {
        return Pattern.compile("^[0-9]{5}(?:-[0-9]{4})?$").matcher(zipCode).matches();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean checkCartEntriesAvailability(final String rentalStart, final String rentalEnd,
                                                final ZoneDeliveryModeModel deliveryModeModel) {
        final AtomicBoolean isAvailable = new AtomicBoolean(Boolean.TRUE);

        if (Objects.nonNull(deliveryModeModel)) {

            final int preDaysToDeduct = Integer.parseInt(deliveryModeModel.getPreReservedDays());

            final int postDaysToAdd = Integer.parseInt(deliveryModeModel.getPostReservedDays());

            final List<Date> holidayBlackoutDates = getBlDatePickerService()
                .getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);

            final Date rentalStartDate = BlDateTimeUtils
                .subtractDaysInRentalDates(preDaysToDeduct, rentalStart, holidayBlackoutDates);
            final Date rentalEndDate = BlDateTimeUtils.getFinalEndDateConsideringPostBlackoutDates(postDaysToAdd,
                rentalEnd, holidayBlackoutDates);


            final LocalDate rentalStartLocalDate = rentalStartDate.toInstant()
                .atZone(ZoneId.systemDefault()).toLocalDate();
            final LocalDate todayLocalDate = new Date().toInstant().atZone(ZoneId.systemDefault())
                .toLocalDate();
				final LocalDate todayLocalDate1 = new Date().toInstant().atZone(ZoneId.of(ZoneId.SHORT_IDS.get("PST"))).toLocalDate();

				LOG.info("******Rental startDate is : " + rentalStartDate + "--" + rentalStartLocalDate + "Today Local Date is : "
						+ todayLocalDate
						+ "Today formated PST zone date is " + todayLocalDate1 + "Delivery mode: " + deliveryModeModel.getCode()
						+ " cutoff time - " + deliveryModeModel.getCutOffTime());

            if (!isRentalStartDateBlackoutDate(rentalStartDate, holidayBlackoutDates) && (
				rentalStartLocalDate.isAfter(todayLocalDate1) || (rentalStartLocalDate.isEqual(todayLocalDate1)
						&& BlDateTimeUtils
                        .compareTimeWithCutOff(deliveryModeModel.getCutOffTime())))) {
						 LOG.info("******Rental startDate is Inside : " + rentalStartLocalDate + "Today Local Date is : "
								 + todayLocalDate + "Today formated PST zone date is " + todayLocalDate1 + "Delivery mode: "
								 + deliveryModeModel.getCode() + " cutoff time - " + deliveryModeModel.getCutOffTime());

            if (isEligibleDeliveryModeForOrderTransfer(deliveryModeModel)) {

                isAvailable.set(
                    checkAvailabilityForPossibleOrderTransferOrders(deliveryModeModel,
                            rentalStartDate, rentalEndDate, holidayBlackoutDates));

            } else {
                final Set<WarehouseModel> lWareHouses =
                    Objects.nonNull(deliveryModeModel.getWarehouse())
                        ? Sets.newHashSet(deliveryModeModel.getWarehouse())
                        : Sets.newHashSet(
                            getBaseStoreService().getCurrentBaseStore().getWarehouses());

                final CartModel cartModel = getBlCartService().getSessionCart();
                cartModel.getEntries().forEach(cartEntry -> {

                    final StockResult stockForEntireDuration =
                        ((BlProductModel) cartEntry.getProduct()).isBundleProduct()
                            ? getBlCommerceStockService()
                            .getStockForBundleProduct((BlProductModel) cartEntry.getProduct(),
                                lWareHouses, rentalStartDate, rentalEndDate)
                            : getBlCommerceStockService()
                                .getStockForEntireDuration(cartEntry.getProduct().getCode(),
                                    lWareHouses, rentalStartDate, rentalEndDate);

                    if (!productService.isAquatechProduct(cartEntry.getProduct())
                            && stockForEntireDuration.getAvailableCount() < cartEntry
                            .getQuantity()) {

                            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                                "Stock not sufficient for Actual rental start date : {} and actual rental end date : {} for product : {} for delivery mode : {}",
                                rentalStartDate, rentalEndDate, cartEntry.getProduct().getCode(),
                                deliveryModeModel.getCode());

                        isAvailable.set(Boolean.FALSE);
                        return;
                    }

                });

                return isAvailable.get();
            }
            }  else {

                BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                    "Rental Dates not eligible for new Actual rental start date : {} and rental end date : {} for delivery mode : {}",
                    rentalStartDate, rentalEndDate, deliveryModeModel.getCode());

                isAvailable.set(Boolean.FALSE);
                return isAvailable.get();
            }
        }
        return isAvailable.get();
    }

    /**
     * This method is to check the the given rental start date is itself a blackout date
     *
     * @param rentalStartDate
     * @param listOfBlackOutDates
     * @return true if it is a blackout date
     */
    private boolean isRentalStartDateBlackoutDate(final Date rentalStartDate,
        final Collection<Date> listOfBlackOutDates) {

        final boolean rentalDateBlackoutDate = listOfBlackOutDates.stream()
            .anyMatch(date -> DateUtils.isSameDay(date, rentalStartDate));

        if (rentalDateBlackoutDate) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                "Rental start Date : {} is a blackout date, so not eligible for renting.",
                rentalStartDate);
        }

        return rentalDateBlackoutDate;
    }

    /**
     * This method is to check the stock availability in different warehouses, in case of internal
     * trnasfer orders
     *
     * @param deliveryModeModel
     * @param rentalStartDate
     * @param rentalEndDate
     * @return true if stock available
     */
    private boolean checkAvailabilityForPossibleOrderTransferOrders(
        final ZoneDeliveryModeModel deliveryModeModel, final Date rentalStartDate,
        final Date rentalEndDate, final List<Date> holidayBlackoutDates) {

        final AtomicBoolean isAvailable = new AtomicBoolean(Boolean.TRUE);

            final Set<WarehouseModel> selectedWareHouse = Sets
                .newHashSet(deliveryModeModel.getWarehouse());

            final Set<WarehouseModel> otherWareHouse = getBaseStoreService().getCurrentBaseStore()
                .getWarehouses().stream().filter(warehouse -> !warehouse.getCode()
                    .equalsIgnoreCase(deliveryModeModel.getWarehouse().getCode())).collect(
                    Collectors.toSet());

            final CartModel cartModel = getBlCartService().getSessionCart();
            cartModel.getEntries().forEach(cartEntry -> {

                final StockResult stockForEntireDuration =
                    ((BlProductModel) cartEntry.getProduct()).isBundleProduct()
                        ? getBlCommerceStockService()
                        .getStockForBundleProduct((BlProductModel) cartEntry.getProduct(),
                            selectedWareHouse, rentalStartDate, rentalEndDate)
                        : getBlCommerceStockService()
                            .getStockForEntireDuration(cartEntry.getProduct().getCode(),
                                selectedWareHouse, rentalStartDate, rentalEndDate);

                if (!productService.isAquatechProduct(cartEntry.getProduct())
                    && stockForEntireDuration.getAvailableCount() < cartEntry.getQuantity()) {

                    //here not available, so check in other warehouse  with +1 start date
                    final Date newStartDate = BlDateTimeUtils
                        .getDateWithSubtractedDays(1, rentalStartDate, holidayBlackoutDates);

                    final LocalDate newStartLocalDate = newStartDate.toInstant()
                        .atZone(ZoneId.systemDefault()).toLocalDate();
                    final LocalDate todayLocalDate = new Date().toInstant()
                        .atZone(ZoneId.systemDefault()).toLocalDate();

                    if (newStartLocalDate.isAfter(todayLocalDate) || (
                        newStartLocalDate.isEqual(todayLocalDate) && BlDateTimeUtils
                            .compareTimeWithCutOff(deliveryModeModel.getCutOffTime()))) {

                        final StockResult stockForEntireDurationOtherWarehouse = getBlCommerceStockService()
                            .getStockForEntireDuration(
                                cartEntry.getProduct().getCode(), otherWareHouse, newStartDate,
                                rentalEndDate);
                        if (stockForEntireDurationOtherWarehouse.getAvailableCount() < cartEntry
                            .getQuantity()) {

                            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                                "Stock is not available for order transfer(+1)  rental start date : {} and actual rental end date : {} for product : {} for delivery mode : {}",
                                newStartDate, rentalEndDate, cartEntry.getProduct().getCode(),
                                deliveryModeModel.getCode());

                            //change the start date with +1 order transfer
                            isAvailable.set(Boolean.FALSE);
                            return;
                        }
                    } else {

                        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                            "New Actual rental start date : {} become past date than : {} for delivery mode : {}",
                            newStartDate, new Date(), deliveryModeModel.getCode());

                        isAvailable.set(Boolean.FALSE);
                        return;
                    }

                }

            });

        return isAvailable.get();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ShippingOptimizationModel getOptimizedShippingRecord(final int carrierId, final int warehouseCode, final String customerZip,
                                                                final int serviceDays, final int inbound) {
        return getBlZoneDeliveryModeDao().getOptimizedShippingRecord(carrierId, warehouseCode, customerZip, serviceDays, inbound);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ConsignmentModel> getAllGroundedConsignments() {
        return getBlZoneDeliveryModeDao().getAllGroundedConsignments(BlDateTimeUtils.getCurrentDateUsingCalendar(
                BlDeliveryModeLoggingConstants.ZONE_PST, BlDateTimeUtils.getStringToDateWithTimeZone(BlDateTimeUtils.getYesterdayDate(),
                        BlDeliveryModeLoggingConstants.ZONE_PST)) ,BlDateTimeUtils.getCurrentDateUsingCalendar(
                                BlDeliveryModeLoggingConstants.ZONE_PST, new Date()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public OptimizedShippingMethodModel getOptimizedShippingMethod(final String code) {
        return getBlZoneDeliveryModeDao().getOptimizedShippingMethod(code);
    }


	 /**
	  * {@inheritDoc}
	  */
	 @Override
	 public ZoneDeliveryModeModel getZoneDeliveryMode(final String code)
	 {
		 return getBlZoneDeliveryModeDao().getZoneDeliveryMode(code);
	 }


    /**
     * {@inheritDoc}
     */
    @Override
    public int checkDateForRental(final String currentDay, final String rentalStart) {
        final int days = BlDateTimeUtils.getDaysBetweenBusinessDays(currentDay, rentalStart);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Business days difference: {} ", days);
        return days;
    }

    /**
     * {@inheritDoc}
     */

    @Override
    public Collection<ZoneDeliveryModeModel> getAllBlDeliveryModes() {
        return getBlZoneDeliveryModeDao().getAllBlDeliveryModes();
    }

    /**
     * {@inheritDoc}
     */
    public boolean isShippingOnBlackoutDate(final List<String> lDeliveryModeAndGroupCode)
	 {
   	 final RentalDateDto rentalDatesFromSession = getBlDatePickerService().getRentalDatesFromSession();
		 if(Objects.isNull(rentalDatesFromSession))
		 {
			 BlLogger.logMessage(LOG, Level.ERROR, "No Rental dates found in session");
			 return Boolean.TRUE;
		 }
		 final Date rentalStartDate = BlDateTimeUtils.getDate(rentalDatesFromSession.getSelectedFromDate(), BlCoreConstants.DATE_FORMAT);
		 BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "All shipping method and group codes : {}", lDeliveryModeAndGroupCode);
		 final List<BlBlackoutDateModel> allBlackoutDatesForShippingMethods = getBlackOutDates(lDeliveryModeAndGroupCode);
		 if(CollectionUtils.isEmpty(allBlackoutDatesForShippingMethods))
		 {
			 return Boolean.FALSE;
		 }
		 final Map<String, List<BlBlackoutDateModel>> groupedDeliveryMethods = getGroupedBlackoutDatesMap(
				 allBlackoutDatesForShippingMethods);
		 final AtomicBoolean isShippingIsBlocked = new AtomicBoolean(Boolean.FALSE);
		 for (final String deliveryCode : lDeliveryModeAndGroupCode)
		 {
			 final List<BlBlackoutDateModel> deliveryBlackOutList = getBlackoutDatesForCode(groupedDeliveryMethods, deliveryCode);
			 if(CollectionUtils.isNotEmpty(deliveryBlackOutList))
			 {
				 removeRentalEndBlackoutDates(deliveryBlackOutList);
				 final List<Date> lBlackoutDate = getListOfDates(deliveryBlackOutList);
				 checkIfDatesIsSame(rentalStartDate, lBlackoutDate, isShippingIsBlocked);
				 BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Is Blackout date present for code : {} is {}", deliveryCode,
						 isShippingIsBlocked.get());
				 if (isShippingIsBlocked.get())
				 {
					 break;
				 }
			 }
		 }
		 return isShippingIsBlocked.get();
	 }

	 /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEligibleDeliveryModeForOrderTransfer(
        final ZoneDeliveryModeModel deliveryModeModel) {

        return ((deliveryModeModel instanceof BlPickUpZoneDeliveryModeModel && Arrays
            .asList(BlCoreConstants.BL_SAN_CARLOS, BlCoreConstants.BL_WALTHAM)
            .contains(deliveryModeModel.getCode()))
            || deliveryModeModel instanceof BlRushDeliveryModeModel);
    }

    /**
	  * Gets the list of dates.
	  *
	  * @param blackoutDates
	  *           the blackout dates
	  * @return the list of dates
	  */
	 private List<Date> getListOfDates(final List<BlBlackoutDateModel> blackoutDates)
	 {
		 return blackoutDates.stream().map(BlBlackoutDateModel::getBlackoutDate).collect(Collectors.toList());
	 }

	 /**
	  * Gets the black out dates.
	  *
	  * @param shippingGroupOrMethodCodes
	  *           the shipping group or method codes
	  * @return the black out dates
	  */
	 private List<BlBlackoutDateModel> getBlackOutDates(final List<String> shippingGroupOrMethodCodes)
	 {
		 return getBlBlackoutDatesDao().getAllBlackoutDatesForShippingMethods(shippingGroupOrMethodCodes);
	 }

	 /**
	  * Gets the grouped blackout dates map.
	  *
	  * @param allBlackoutDatesForShippingGroup
	  *           the all blackout dates for shipping group
	  * @return the grouped blackout dates map
	  */
	 private Map<String, List<BlBlackoutDateModel>> getGroupedBlackoutDatesMap(
			 final List<BlBlackoutDateModel> allBlackoutDatesForShippingGroup)
	 {
		 return allBlackoutDatesForShippingGroup.stream()
				 .collect(Collectors.groupingBy(blackoutDate -> blackoutDate.getBlockedShippingMethod().toString()));
	 }

	 /**
	  * Gets the blackout dates for code.
	  *
	  * @param groupedShippingGroups
	  *           the grouped shipping groups
	  * @param code
	  *           the code
	  * @return the blackout dates for code
	  */
	 private ArrayList<BlBlackoutDateModel> getBlackoutDatesForCode(
			 final Map<String, List<BlBlackoutDateModel>> groupedShippingGroups, final String code)
	 {
		 return Lists.newArrayList(CollectionUtils.emptyIfNull(groupedShippingGroups.get(code)));
	 }

	 /**
	  * Removes the rental end blackout dates.
	  *
	  * @param deliveryBlackOutList
	  *           the delivery black out list
	  */
	 private void removeRentalEndBlackoutDates(final List<BlBlackoutDateModel> deliveryBlackOutList)
	 {
		 deliveryBlackOutList.removeIf(delivery -> BlackoutDateTypeEnum.RENTAL_END_DATE.equals(delivery.getBlackoutDateType()));
	 }

	 /**
	  * Check if dates is same.
	  *
	  * @param rentalStartDate
	  *           the rental start date
	  * @param lBlackoutDate
	  *           the l blackout date
	  * @param isDateIsBlocked
	  *           the is date is blocked
	  */
	 private void checkIfDatesIsSame(final Date rentalStartDate, final List<Date> lBlackoutDate,
			 final AtomicBoolean isDateIsBlocked)
	 {
		 for(final Date bDate : lBlackoutDate)
		 {
			 if (DateUtils.isSameDay(rentalStartDate, bDate))
			 {
				 isDateIsBlocked.set(Boolean.TRUE);
				 break;
			 }
		 }
	 }

    public BlDeliveryModeDao getBlZoneDeliveryModeDao() {
        return blDeliveryModeDao;
    }

    public void setBlZoneDeliveryModeDao(final BlDeliveryModeDao blDeliveryModeDao) {
        this.blDeliveryModeDao = blDeliveryModeDao;
    }

    public UserService getUserService() {
        return userService;
    }

    public void setUserService(final UserService userService) {
        this.userService = userService;
    }

    public BlFedExSameDayService getBlFedExSameDayService() {
        return blFedExSameDayService;
    }

    public void setBlFedExSameDayService(final BlFedExSameDayService blFedExSameDayService) {
        this.blFedExSameDayService = blFedExSameDayService;
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
     * @return the blCartService
     */
    public BlCartService getBlCartService() {
        return blCartService;
    }

    /**
     * @param blCartService the blCartService to set
     */
    public void setBlCartService(final BlCartService blCartService) {
        this.blCartService = blCartService;
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
	 * @return the blBlackoutDatesDao
	 */
	public BlBlackoutDatesDao getBlBlackoutDatesDao()
	{
		return blBlackoutDatesDao;
	}

	/**
	 * @param blBlackoutDatesDao the blBlackoutDatesDao to set
	 */
	public void setBlBlackoutDatesDao(final BlBlackoutDatesDao blBlackoutDatesDao)
	{
		this.blBlackoutDatesDao = blBlackoutDatesDao;
	}

	/**
	 * @return the blDatePickerService
	 */
	public BlDatePickerService getBlDatePickerService()
	{
		return blDatePickerService;
	}

	/**
	 * @param blDatePickerService the blDatePickerService to set
	 */
	public void setBlDatePickerService(final BlDatePickerService blDatePickerService)
	{
		this.blDatePickerService = blDatePickerService;
	}

    public BlProductService getProductService() {
        return productService;
    }

    public void setProductService(final BlProductService productService) {
        this.productService = productService;
    }

	 /**
	  * @return the blDeliveryModeService
	  */
	 public BlDeliveryModeService getBlDeliveryModeService()
	 {
		 return blDeliveryModeService;
	 }

	 /**
	  * @param blDeliveryModeService
	  *           the blDeliveryModeService to set
	  */
	 public void setBlDeliveryModeService(final BlDeliveryModeService blDeliveryModeService)
	 {
		 this.blDeliveryModeService = blDeliveryModeService;
	 }
}
