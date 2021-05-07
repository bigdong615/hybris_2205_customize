package com.bl.core.shipping.service;

import com.bl.core.model.*;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;

import java.util.Collection;

/**
 * {javadoc}
 * @author Namrata Lohar
 */
public interface BlDeliveryModeService {

    /**
     * This method will fetch all the shipping groups from dao
     *
     * @return Collection of ShippingGroups
     */
    Collection<ShippingGroupModel> getAllShippingGroups();

    /**
     * method with contain business logic for Ship to Home, Hotel or Business Delivery Modes
     * @param rentalStart date
     * @param rentalEnd date
     * @param carrier UPS/FedEx
     * @return Collection of object with delivery-modes differentiating in UPS and FedEx
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesWithRentalDates(final String rentalStart,
                                                                                      final String rentalEnd,
                                                                                      final String carrier);

    /**
     * method with contain business logic for Ship to Home, Hotel or Business Delivery Modes to fetch all modes
     * @param rentalStart date
     * @param rentalEnd date
     * @return Map object with delivery-modes differentiating in UPS and FedEx
     */
    Collection<ZoneDeliveryModeModel> getAllShipToHomeDeliveryModesWithRentalDates(final String rentalStart,
                                                                                             final String rentalEnd);

    /**
     * This method will return all the delivery modes after selecting Ship to Home, Hotel or Business shipping group from dao.
     * @param carrier which will differentiate UPS or FedEx
     * @param mode i.e., Standard or Overnight
     * @param pstCutOffTime to check cutOffTime
     * @return Collection of ZoneDeliveryModeModels
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModes(final String carrier, final String mode, final String pstCutOffTime);

    /**
     * This method will return all the delivery modes after selecting Ship to Home, Hotel or Business shipping group from dao.
     * @param carrier which will differentiate UPS or FedEx
     * @param mode i.e., Standard or Overnight
     * @param pstCutOffTime to check cutOffTime
     * @return Collection of ZoneDeliveryModeModels
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesNotLike(final String carrier, final String mode, final String pstCutOffTime);

    /**
     *This method will fetch all the Partner PickUp Store from dao
     *
     * @return Collection of PartnerPickUp Stores
     */
    Collection<PartnerPickUpStoreModel> getAllPartnerPickUpStore();

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group for the UPS Store
     * from dao.
     *
     * @param mode i.e., standard or overnight
     * @param pstCutOffTime to check cutOffTime
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModes(final String mode, final String pstCutOffTime);

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group for the UPS Store
     * from dao with not like AM
     *
     * @param mode i.e., standard or overnight
     * @param pstCutOffTime to check cutOffTime
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModesNotLike(final String mode, final String pstCutOffTime);

    /**
     * method with contain business logic for Partner-PickUp Delivery Modes
     * @param rentalStart date
     * @param rentalEnd date
     * @return Collection of object with delivery-modes differentiating in Partner-PickUp Stores
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerPickUpDeliveryModesWithRentalDates(final String rentalStart,
                                                                                final String rentalEnd);

    /**
     * method with contain business logic for Partner-PickUp to fetch all modes
     * @param rentalStart date
     * @param rentalEnd date
     * @return Collection object with delivery-modes differentiating in Partner-PickUp Stores
     */
    Collection<BlPickUpZoneDeliveryModeModel> getAllPartnerPickUpDeliveryModesWithRentalDatesForUPSStore(final String rentalStart,
                                                                                                       final String rentalEnd);

    /**
     * This method will check validity of user entered pinCode for SF or NYC
     *
     * @param pinCode to be checked for validity
     * @param deliveryType i.e., SF or NYC
     * @return return true / false
     */
    boolean checkSFOrNYCPinCodeValidity(final String pinCode, final String deliveryType);

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group
     *  depending on the selected zone from dao.
     * @param partnerZone i.e., name
     * @param rentalStart date
     * @param rentalEnd date
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModes(final String partnerZone, final String rentalStart,
                                                                         final String rentalEnd);

    /**
     * This method will fetch the partner-pickup-zone from DB who has delivery-modes associated to it from dao
     * @param partnerZone i.e., name
     * @return PartnerPickUpStoreModel object
     */
    PartnerPickUpStoreModel getPartnerPickUpStoreFromPartnerZone(final String partnerZone);

    /**
     * This method will fetch all time windows for RushDelivery depending on deliveryType attribute from dao
     * @param deliveryMode to specify SF or NYC Shipping group
     * @param pstCutOffTime for time condition
     * @param pinCode entered by user
     * @return Collection of BlRushDeliveryModeModel
     */
    Collection<BlRushDeliveryModeModel> getBlRushDeliveryModes(final String pinCode, final String deliveryMode,
                                                               final String pstCutOffTime);

    /**
     * This method will fetch shipping cost model for calculated value for variable delivery cost model from dao
     *
     * @param calculatedCost value\
     * @param deliveryMethod name
     * @return double i.e., shipping cost model amount
     */
    ShippingCostModel getShippingCostForCalculatedDeliveryCost(final String calculatedCost, final String deliveryMethod);

    /**
     * This method will return amount for delivery mode selected for order
     *
     * @param order model
     * @param deliveryMode delivery method
     * @return double value of amount
     */
    double getShippingCostAmount(final AbstractOrderModel order, final DeliveryModeModel deliveryMode);

    /**
     * This method will return amount for delivery mode selected for order according to appropriate model
     *
     * @param order order
     * @param zoneDeliveryModeModel delivery mode
     * @return double value of amount
     */
    double getAmountForAppropriateZoneModel(final AbstractOrderModel order, final ZoneDeliveryModeModel zoneDeliveryModeModel);
}
