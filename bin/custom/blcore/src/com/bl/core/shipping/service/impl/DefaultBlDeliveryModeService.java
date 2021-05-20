package com.bl.core.shipping.service.impl;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.data.StockResult;
import com.bl.core.enums.CarrierEnum;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.core.model.PartnerPickUpStoreModel;
import com.bl.core.model.ShippingCostModel;
import com.bl.core.model.ShippingGroupModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.shipping.dao.BlDeliveryModeDao;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.fexEx.data.SameDayCityReqData;
import com.bl.facades.fexEx.data.SameDayCityResData;
import com.bl.integration.services.BlFedExSameDayService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Sets;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeValueModel;
import de.hybris.platform.order.impl.DefaultZoneDeliveryModeService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.storelocator.model.PointOfServiceModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;
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

    @Resource(name = "userService")
    transient UserService userService;

    @Resource(name = "fedExSameDayServiceImpl")
    BlFedExSameDayService blFedExSameDayService;
    
    private BlCommerceStockService blCommerceStockService;
    
    private BlCartService blCartService;
    
    private BaseStoreService baseStoreService;

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
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesNotLike(final String carrier, final String mode,
                                                                               final String pstCutOffTime, final boolean payByCustomer) {
        return getBlZoneDeliveryModeDao().getShipToHomeDeliveryModesNotLike(carrier, mode, pstCutOffTime, payByCustomer);
    }

    /**
     * {@inheritDoc}
     * @return
     */
    @Override
    public Collection<ZoneDeliveryModeModel> getAllShipToHomeDeliveryModesWithRentalDates(final String rentalStart,
                                                                                          final String rentalEnd,
                                                                                          final boolean payByCustomer) {

        Collection<ZoneDeliveryModeModel> allDeliveryModes = new ArrayList<>();
        Collection<ZoneDeliveryModeModel> upsZoneDeliveryModes = getShipToHomeDeliveryModesWithRentalDates(rentalStart,
                rentalEnd, String.valueOf(CarrierEnum.UPS), payByCustomer).stream().filter(model -> checkDaysToSkipForDeliveryMode(model, rentalStart, rentalEnd)).
                collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(upsZoneDeliveryModes)) {
            allDeliveryModes.addAll(upsZoneDeliveryModes);
        }
        Collection<ZoneDeliveryModeModel> fedexZoneDeliveryModes = getShipToHomeDeliveryModesWithRentalDates(rentalStart,
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
    public Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesWithRentalDates(final String rentalStart,
                                                                                       final String rentalEnd,
                                                                                       final String carrier,
                                                                                       final boolean payByCustomer) {
        final String pstCutOffTime = BlDateTimeUtils.getCurrentTimeUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST);
        final int result = checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
                rentalStart);
            if (result >= BlInventoryScanLoggingConstants.TWO) {
                return getShipToHomeDeliveryModes(carrier, BlDeliveryModeLoggingConstants.DELIVERY_TYPE_STANDARD,
                        null, payByCustomer);
            } else if (result == BlInventoryScanLoggingConstants.ONE) {
                final DayOfWeek currentDayOfWeek = BlDateTimeUtils.getDayOfWeek(BlDeliveryModeLoggingConstants.ZONE_PST, new Date().toString());
                if(currentDayOfWeek.equals(DayOfWeek.SUNDAY) || currentDayOfWeek.equals(DayOfWeek.SATURDAY)) {
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
            return getPartnerZoneUPSStoreDeliveryModes(BlDeliveryModeLoggingConstants.DELIVERY_TYPE_OVERNIGHT,
                    pstCutOffTime, payByCustomer);
        } 
        return Collections.emptyList();
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
        if(CollectionUtils.isNotEmpty(blPickUpZoneDeliveryModeModels)) {
            final Collection<BlPickUpZoneDeliveryModeModel> newBlPickUpZoneDeliveryModeModels = new ArrayList<>(blPickUpZoneDeliveryModeModels);
            final int result = checkDateForRental(BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date())
                    , rentalStart);
            for(BlPickUpZoneDeliveryModeModel pickUpZoneDeliveryModeModel : blPickUpZoneDeliveryModeModels) {
                checkDeliveryModeValidityOfTypePartner(newBlPickUpZoneDeliveryModeModels, result, pickUpZoneDeliveryModeModel);
            }
            return CollectionUtils.isNotEmpty(newBlPickUpZoneDeliveryModeModels) ? newBlPickUpZoneDeliveryModeModels.stream()
                    .filter(model -> checkDaysToSkipForDeliveryMode(model, rentalStart, rentalEnd)).collect(Collectors.toList()) : Collections.emptyList();
        }
        return Collections.emptyList();
    }

    /**
     * This method will check conditions for partner delivery locations
     *
     * @param blPickUpZoneDeliveryModeModels collection to remove unwanted records
     * @param result business days difference
     * @param pickUpZoneDeliveryModeModel current model to check with condition
     */
    private void checkDeliveryModeValidityOfTypePartner(final Collection<BlPickUpZoneDeliveryModeModel> blPickUpZoneDeliveryModeModels,
                                                        final int result, final BlPickUpZoneDeliveryModeModel pickUpZoneDeliveryModeModel) {
        if(pickUpZoneDeliveryModeModel.isWarehousePickUp()) {
            if(!BlDateTimeUtils.compareTimeWithCutOff(pickUpZoneDeliveryModeModel.getCutOffTime())) {
                blPickUpZoneDeliveryModeModels.remove(pickUpZoneDeliveryModeModel);
            }
        } else {
            if(result <= BlInventoryScanLoggingConstants.TWO){
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

        return getBlZoneDeliveryModeDao().getBlRushDeliveryModes(
                deliveryMode, pstCutOffTime, payByCustomer).stream().filter(model -> checkDaysToSkipForDeliveryMode(model, rentalStart, rentalEnd))
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ShippingCostModel getShippingCostForCalculatedDeliveryCost(final String calculatedCost, final String deliveryMethod) {
        return getUserService().getCurrentUser() instanceof CustomerModel ? getBlZoneDeliveryModeDao()
                .getShippingCostForCalculatedDeliveryCost(calculatedCost, deliveryMethod, true) :
                getBlZoneDeliveryModeDao().getShippingCostForCalculatedDeliveryCost(calculatedCost, deliveryMethod, false);
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
        if(getUserService().getCurrentUser() instanceof CustomerModel) {
            return getPayByCustomerShippingCost(order, zoneDeliveryModeModel);
        }
        return BlInventoryScanLoggingConstants.ZERO;
    }

    /**
     * This method will get cosy for only pay by customer delivery methods
     *
     * @param order model
     * @param zoneDeliveryModeModel delivery model
     * @return shipping amount
     */
    private double getPayByCustomerShippingCost(AbstractOrderModel order, ZoneDeliveryModeModel zoneDeliveryModeModel) {
        for(ZoneDeliveryModeValueModel valueModel : zoneDeliveryModeModel.getValues()) {
            if(!valueModel.isFixedAmount()) {
                final Map<String, Double> calculatedValueMap;
                try {
                    calculatedValueMap = getCalculatedWeightForDelivery(order);
                    final Double shippingCostModel = getShippingAmount(order, zoneDeliveryModeModel, calculatedValueMap);
                    return shippingCostModel != null ? Math.max(shippingCostModel, valueModel.getMinimum()) :
                            valueModel.getMinimum();
                } catch(Exception e) {
                    BlLogger.logMessage(LOG, Level.ERROR,"Exception while calculating delivery cost");
                }
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"ShippingCostModel is null, Shipping amount: {} ",
                        BlInventoryScanLoggingConstants.ZERO);
                return (double) BlInventoryScanLoggingConstants.ZERO;
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Shipping fixed amount: {} ", valueModel.getValue());
                return valueModel.getValue();
            }
        }
        return BlInventoryScanLoggingConstants.ZERO;
    }

    /**
     * This method will give shipping amount
     * @param order order
     * @param zoneDeliveryModeModel shipping method
     * @param calculatedValueMap calculated value map
     * @return shipping cost
     */
    private Double getShippingAmount(final AbstractOrderModel order, final ZoneDeliveryModeModel zoneDeliveryModeModel,
                                     final Map<String, Double> calculatedValueMap) {
        if(!(order instanceof CartModel)) {
            order.setTotalWeight(calculatedValueMap.get(BlDeliveryModeLoggingConstants.TOTAL_WEIGHT));
            order.setDimensionalWeight(calculatedValueMap.get(BlDeliveryModeLoggingConstants.DIMENSIONAL_WEIGHT));
        }
        final ShippingCostModel shippingCostModel = getShippingCostForCalculatedDeliveryCost(String.valueOf(Math.max(
                calculatedValueMap.get(BlDeliveryModeLoggingConstants.TOTAL_WEIGHT), calculatedValueMap.get(
                        BlDeliveryModeLoggingConstants.DIMENSIONAL_WEIGHT))), zoneDeliveryModeModel.getShippingCostCode().getCode());
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
        Collection<AbstractOrderEntryModel> abstractOrderEntryModels;
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
                    getBlZoneDeliveryModeDao().getDimensionalFactorForDeliveryFromStore(BlDeliveryModeLoggingConstants.STORE);
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
     * This method will check delivery mode's model instance and send appropriate model further.
     *
     * @param deliveryMode model
     * @param rentalStart start date
     * @param rentalEnd the rental end
     * @return boolean for current delivery mode to disable or not
     */
    public boolean checkDaysToSkipForDeliveryMode(final DeliveryModeModel deliveryMode, final String rentalStart, final String rentalEnd) {
        final DayOfWeek currentDayOfWeek = BlDateTimeUtils.getDayOfWeek(BlDeliveryModeLoggingConstants.ZONE_PST, rentalStart);
        if(deliveryMode instanceof ZoneDeliveryModeModel && checkCartEntriesAvailability(rentalStart, rentalEnd, (ZoneDeliveryModeModel)deliveryMode)) {
      	  if(deliveryMode instanceof BlPickUpZoneDeliveryModeModel) {
              final BlPickUpZoneDeliveryModeModel zoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) deliveryMode;
              return getResultForDayToSkip(currentDayOfWeek, zoneDeliveryModeModel);
          } else if(deliveryMode instanceof BlRushDeliveryModeModel) {
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
        if(validateZip(pinCode)) {
            try {
                final SameDayCityResData sameDayCityResData = getBlFedExSameDayService().getAvailability(getSameDayCityReqData(pinCode,
                        getWarehouseZipCode(deliveryType, true)));
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Checking same day fedex integration pinCode validity");
                return sameDayCityResData.getServiceApplicable() != null ? sameDayCityResData.getServiceApplicable(): Boolean.FALSE;
            } catch (URISyntaxException e) {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Exception in Checking same day fedex integration pinCode validity", e);
                return false;
            }
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getWarehouseZipCode(final String deliveryType, final boolean payByCustomer) {
        final BlRushDeliveryModeModel blRushDeliveryModeModel = getBlZoneDeliveryModeDao().getBlRushDeliveryModeForWarehouseZipCode(
                deliveryType, payByCustomer);
        if(blRushDeliveryModeModel != null) {
            final WarehouseModel warehouseModel = blRushDeliveryModeModel.getWarehouse();
            if(warehouseModel != null) {
                return getPOSAddress(warehouseModel.getPointsOfService());
            }
        }
        return null;
    }

    /**
     * This method will take POS and will return zipCode of warehouse
     *
     * @param pos Point OF Service collection from warehouse
     * @return zipCode
     */
    private String getPOSAddress(final Collection<PointOfServiceModel> pos) {
        if(CollectionUtils.isNotEmpty(pos)) {
            for (PointOfServiceModel model : pos) {
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
     *
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
       final ZoneDeliveryModeModel deliveryModeModel) 
    {
   	 if (Objects.nonNull(deliveryModeModel))
    	{
    		final AtomicBoolean isAvailable = new AtomicBoolean(false);
    		final int numberOfDaysToSkip = deliveryModeModel.getNumberOfDaysToSkip().intValue();
    		final Date rentalStartDate = getRentalStartDate(rentalStart, numberOfDaysToSkip);
    		final Date rentalEndDate = getRentalEndDate(rentalEnd, numberOfDaysToSkip);
   		final Set<WarehouseModel> lWareHouses = Objects.nonNull(deliveryModeModel.getWarehouse())
   				? Sets.newHashSet(deliveryModeModel.getWarehouse()) 
   						: Sets.newHashSet(getBaseStoreService().getCurrentBaseStore().getWarehouses());
   		final CartModel cartModel = getBlCartService().getSessionCart();
   		cartModel.getEntries().forEach(cartEntry -> {
   			final StockResult stockForEntireDuration = getBlCommerceStockService().getStockForEntireDuration(
   					cartEntry.getProduct().getCode(), lWareHouses, rentalStartDate, rentalEndDate);
   			isAvailable.set(stockForEntireDuration.getAvailableCount() > 0);
   		});
   		return isAvailable.get();
   	} 
    	return false;
    }
    
    /**
     * Gets the rental start date by subtracting number of days to skip for selected delivery mode.
     *
     * @param rentalStartDate the rental start date
     * @param numberOfDaysToSkip the number of days to skip
     * @return the rental start date
     */
    private Date getRentalStartDate(final String rentalStartDate, int numberOfDaysToSkip)
    {
   	 LocalDate startDate = BlDateTimeUtils.convertStringDateToLocalDate(rentalStartDate, BlCoreConstants.DATE_FORMAT);
       int subtractedDays = 0;
       while (subtractedDays < numberOfDaysToSkip) {
      	 startDate = startDate.minusDays(1);
           if (!(startDate.getDayOfWeek() == DayOfWeek.SATURDAY || startDate.getDayOfWeek() == DayOfWeek.SUNDAY)) {
               ++subtractedDays;
           }
       }
       return Date.from(startDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
    }
    
    /**
     * Gets the rental end date by adding number of days to skip for selected delivery mode..
     *
     * @param rentalEndDate the rental end date
     * @param numberOfDaysToAdd the number of days to add
     * @return the rental end date
     */
    private Date getRentalEndDate(final String rentalEndDate, int numberOfDaysToAdd)
    {
   	 LocalDate endDate = BlDateTimeUtils.convertStringDateToLocalDate(rentalEndDate, BlCoreConstants.DATE_FORMAT);
   	 int addedDays = 0;
       while (addedDays < numberOfDaysToAdd) {
      	 endDate = endDate.plusDays(1);
           if (!(endDate.getDayOfWeek() == DayOfWeek.SATURDAY || endDate.getDayOfWeek() == DayOfWeek.SUNDAY)) {
               ++addedDays;
           }
       }
       return Date.from(endDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int checkDateForRental(final String currentDay, final String rentalStart) {
        final int days = BlDateTimeUtils.getDaysBetweenBusinessDays(currentDay, rentalStart);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Business days difference: {} ", days);
        return days;
    }
    
    /**
 	 * {@inheritDoc}
 	 */
 	
 	@Override
 	public Collection<ZoneDeliveryModeModel> getAllBlDeliveryModes()
 	{
 		return getBlZoneDeliveryModeDao().getAllBlDeliveryModes();
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

    public void setUserService(UserService userService) {
        this.userService = userService;
    }

    public BlFedExSameDayService getBlFedExSameDayService() {
        return blFedExSameDayService;
    }

    public void setBlFedExSameDayService(BlFedExSameDayService blFedExSameDayService) {
        this.blFedExSameDayService = blFedExSameDayService;
    }

	/**
	 * @return the blCommerceStockService
	 */
	public BlCommerceStockService getBlCommerceStockService()
	{
		return blCommerceStockService;
	}

	/**
	 * @param blCommerceStockService the blCommerceStockService to set
	 */
	public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService)
	{
		this.blCommerceStockService = blCommerceStockService;
	}

	/**
	 * @return the blCartService
	 */
	public BlCartService getBlCartService()
	{
		return blCartService;
	}

	/**
	 * @param blCartService the blCartService to set
	 */
	public void setBlCartService(BlCartService blCartService)
	{
		this.blCartService = blCartService;
	}

	/**
	 * @return the baseStoreService
	 */
	public BaseStoreService getBaseStoreService()
	{
		return baseStoreService;
	}

	/**
	 * @param baseStoreService the baseStoreService to set
	 */
	public void setBaseStoreService(BaseStoreService baseStoreService)
	{
		this.baseStoreService = baseStoreService;
	}
}
