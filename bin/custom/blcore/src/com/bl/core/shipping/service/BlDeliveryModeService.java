package com.bl.core.shipping.service;

import com.bl.core.model.*;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.text.ParseException;
import java.util.Collection;
import java.util.List;

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
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of object with delivery-modes differentiating in UPS and FedEx
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesWithRentalDates(final String rentalStart, final String rentalEnd,
                                                                                final String carrier, final boolean payByCustomer);

    
    /**
     * method with contain business logic for Ship to Home, Hotel or Business Delivery Modes
     * @param carrier UPS/FedEx
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of object with delivery-modes differentiating in UPS and FedEx
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesWithoutRentalDates(final String carrier, final boolean payByCustomer);
                                                                                

    /**
     * method with contain business logic for Ship to Home, Hotel or Business Delivery Modes to fetch all modes
     * @param rentalStart date
     * @param rentalEnd date
     * @param payByCustomer to get customer related delivery modes
     * @return Map object with delivery-modes differentiating in UPS and FedEx
     */
    Collection<ZoneDeliveryModeModel> getAllShipToHomeDeliveryModesWithRentalDates(final String rentalStart, final String rentalEnd,
                                                                                   final boolean payByCustomer);
    
    /**
     * method with contain business logic for Ship to Home, Hotel or Business Delivery Modes to fetch all modes
     * @param payByCustomer to get customer related delivery modes
     * @return Map object with delivery-modes differentiating in UPS and FedEx
     */
    Collection<ZoneDeliveryModeModel> getAllShipToHomeDeliveryModesWithoutRentalDates(final boolean payByCustomer);
                                                                                   


    /**
     * This method will return all the delivery modes after selecting Ship to Home, Hotel or Business shipping group from dao.
     * @param carrier which will differentiate UPS or FedEx
     * @param mode i.e., Standard or Overnight
     * @param pstCutOffTime to check cutOffTime
     * @param payByCustomer ge delivery modes based on customer
     * @return Collection of ZoneDeliveryModeModels
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModes(final String carrier, final String mode, final String pstCutOffTime,
                                                                 final boolean payByCustomer);

    /**
     * This method will return all the delivery modes after selecting Ship to Home, Hotel or Business shipping group from dao.
     * @param carrier which will differentiate UPS or FedEx
     * @param mode i.e., Standard or Overnight
     * @param payByCustomer ge delivery modes based on customer
     * @return Collection of ZoneDeliveryModeModels
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesForUsedGear(final String carrier, final String mode,final boolean payByCustomer);

    /**
     * This method will return all the delivery modes after selecting Ship to Home, Hotel or Business shipping group from dao.
     * @param carrier which will differentiate UPS or FedEx
     * @param mode i.e., Standard or Overnight
     * @param pstCutOffTime to check cutOffTime
     * @param payByCustomer ge delivery modes based on customer
     * @return Collection of ZoneDeliveryModeModels
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesNotLike(final String carrier, final String mode, final String pstCutOffTime,
                                                                        final boolean payByCustomer);

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
     * @param payByCustomer ge delivery modes based on customer
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModes(final String mode, final String pstCutOffTime,
                                                                                  final boolean payByCustomer);

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group for the UPS Store
     * from dao.
     *
     * @param mode i.e., standard or overnight
     * @param payByCustomer ge delivery modes based on customer
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModesForUsedGear(final String mode, final boolean payByCustomer);
                                                                                  

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group for the UPS Store
     * from dao with not like AM
     *
     * @param mode i.e., standard or overnight
     * @param pstCutOffTime to check cutOffTime
     * @param payByCustomer ge delivery modes based on customer
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModesNotLike(final String mode, final String pstCutOffTime,
                                                                                         final boolean payByCustomer);

    /**
     * method with contain business logic for Partner-PickUp Delivery Modes
     * @param rentalStart date
     * @param rentalEnd date
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of object with delivery-modes differentiating in Partner-PickUp Stores
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerPickUpDeliveryModesWithRentalDates(final String rentalStart,
                                                                                final String rentalEnd, final boolean payByCustomer);
    
    /**
     * method with contain business logic for Partner-PickUp Delivery Modes
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of object with delivery-modes differentiating in Partner-PickUp Stores
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerPickUpDeliveryModesWithoutRentalDates(final boolean payByCustomer);


    /**
     * method with contain business logic for Partner-PickUp to fetch all modes
     * @param rentalStart date
     * @param rentalEnd date
     * @param payByCustomer to get customer related delivery modes
     * @return Collection object with delivery-modes differentiating in Partner-PickUp Stores
     */
    Collection<BlPickUpZoneDeliveryModeModel> getAllPartnerPickUpDeliveryModesWithRentalDatesForUPSStore(final String rentalStart,
                                                                                                         final String rentalEnd,
                                                                                                         final boolean payByCustomer);


    /**
     * Method contain business logic for Partner-PickUp to fetch all modes
     * @param payByCustomer to get customer related delivery modes
     * @return Collection object with delivery-modes differentiating in Partner-PickUp Stores
     */
    Collection<BlPickUpZoneDeliveryModeModel> getAllPartnerPickUpDeliveryModesWithoutRentalDatesForUPSStore(final boolean payByCustomer);
                                                                                                         

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
     *
     * @param partnerZone i.e., name
     * @param rentalStart date
     * @param rentalEnd date
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of BlPickUpZoneDeliveryModeModel
     * @throws ParseException the parse exception
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModes(final String partnerZone, final String rentalStart,
                                                                         final String rentalEnd, final boolean payByCustomer)
            throws ParseException;
    
    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group
     *  depending on the selected zone from dao.
     *
     * @param partnerZone i.e., name
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of BlPickUpZoneDeliveryModeModel
     * @throws ParseException the parse exception
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModesForUsedGear(final String partnerZone,final boolean payByCustomer)
            throws ParseException;

    /**
     * Get delivery mode data for new gear.
     * @param partnerZone
     * @param payByCustomer
     * @return
     * @throws ParseException
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModesForNewGear(final String partnerZone,final boolean payByCustomer)
        throws ParseException;

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
     * @param rentalStart date
     * @param rentalEnd date
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of BlRushDeliveryModeModel
     */
    Collection<BlRushDeliveryModeModel> getBlRushDeliveryModes(final String deliveryMode, final String pstCutOffTime,final String rentalStart,
                                                               final String rentalEnd, final boolean payByCustomer);

    /**
     * This method will fetch all time windows for RushDelivery depending on deliveryType attribute from dao
     * @param deliveryMode to specify SF or NYC Shipping group
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of BlRushDeliveryModeModel
     */
    Collection<BlRushDeliveryModeModel> getBlRushDeliveryModesForUsedGear(final String deliveryMode,final boolean payByCustomer);

    /**
     * This method will fetch shipping cost model for calculated value for variable delivery cost model from dao
     *
     * @param calculatedCost value
     * @param deliveryMethod name
     * @return double i.e., shipping cost model amount
     */
    ShippingCostModel getShippingCostForCalculatedDeliveryCost(final String calculatedCost, final ZoneDeliveryModeModel deliveryMethod);

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

    /**
     * This method will check conditions on rentalDate to return appropriate delivery modes
     *
     * @param currentDay  means current Day
     * @param rentalStart means start rental date
     * @return long i.e., difference in days
     */
    int checkDateForRental(final String currentDay, final String rentalStart);
    
 	/**
	  * This method will return all the delivery modes.
	  *
	  * @return Collection of ZoneDeliveryModeModels
	  */

 	public Collection<ZoneDeliveryModeModel> getAllBlDeliveryModes();
 	
 	/**
	  * This method will check cart entries for gear availability.
	  *
	  * @param rentalStart date
	  * @param rentalEnd date
	  * @param deliveryModeModel the delivery mode model
	  * @return boolean
	  */
   boolean checkCartEntriesAvailability(final String rentalStart, final String rentalEnd,
         final ZoneDeliveryModeModel deliveryModeModel);

    /**
     * This method will return ShippingOptimizationModel with input elements
     *
     * @param carrierId UPS/FedEx or 2/1
     * @param warehouseCode CA/MA or 1/2
     * @param customerZip from delivery address
     * @param serviceDays businessDays ground availability
     * @param inbound going/coming
     *
     * @return ShippingOptimizationModel
     */
    ShippingOptimizationModel getOptimizedShippingRecord(final int carrierId, final int warehouseCode, final String customerZip,
                                                         final int serviceDays, final int inbound);

    /**
     * This method will return ShippingOptimizationModel with input elements
     *
     * @param carrierId UPS/FedEx or 2/1
     * @param warehouseCode CA/MA or 1/2
     * @param customerZip from delivery address
     *
     * @return ShippingOptimizationModel
     */
    List<ShippingOptimizationModel> getOptimizedShippingRecords(final int carrierId, final int warehouseCode, final String customerZip);
    
    /**
     * This method will return ShippingOptimizationModel with input elements
     *
     * @param carrierId UPS/FedEx or 2/1
     * @param warehouseCode CA/MA or 1/2
     * @param customerZip from delivery address
     *
     * @return ShippingOptimizationModel
     */
    List<ShippingOptimizationModel> getOptimizedShippingRecordsForCarrierAndZip(final int carrierId, final String customerZip);
    
    /**
     * This method will return Collection of Consignment models with input
     *
     * @return Collection<ConsignmentModel>
     */
    Collection<ConsignmentModel> getAllGroundedConsignments();

    /**
     * javadoc
     * this method will return optimized shipping method model from dao
     *
     * @param code value
     * @return model
     */
    OptimizedShippingMethodModel getOptimizedShippingMethod(final String code);
    
    
    /**
     * javadoc
     * this method will return zone delivery mode model from dao
     *
     * @param code value
     * @return model
     */
    ZoneDeliveryModeModel getZoneDeliveryMode(final String code);
    
    
    /**
     * Check shipping blackout.
     *
     * @param lDeliveryModeAndGroupCode the l delivery mode and group code
     * @return true, if successful
     */
    public boolean isShippingOnBlackoutDate(final List<String> lDeliveryModeAndGroupCode);

    /**
     * This method will check whether the given delivery mode is suitable for internal transfer order.
     *
     * @param deliveryModeModel
     * @return true if the delivery mode supports internal transfer
     */
    boolean isEligibleDeliveryModeForOrderTransfer(final ZoneDeliveryModeModel deliveryModeModel);
    

    void updatePreAndPostServiceDays(List<ShippingOptimizationModel> shippingOptimizationModels, int preDaysToDeduct, int postDaysToAdd);

}
