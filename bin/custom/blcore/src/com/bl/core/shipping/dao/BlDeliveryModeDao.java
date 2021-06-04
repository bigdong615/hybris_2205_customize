package com.bl.core.shipping.dao;

import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.core.model.*;
import de.hybris.platform.core.model.ShippingOptimizationModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.util.Collection;

/**
 * {javadoc}
 * @author Namrata Lohar
 */
public interface BlDeliveryModeDao {

    /**
     * This method will fetch all the shipping group from DB who has delivery-modes associated to it
     * @return Collection of ShippingGroupModel
     */
    Collection<ShippingGroupModel> getAllShippingGroups();

    /**
     * This method will return all the delivery modes after selecting Ship to Home, Hotel or Business shipping group
     * depending on the selected carrier.
     * @param carrier ie., UPS or FedEx
     * @param mode i.e., standard or overnight
     * @param pstCutOffTime for time condition
     *
     * @return Collection of ZoneDeliveryModeModel
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModes(final String carrier, final String mode, final String pstCutOffTime,
                                                                 final boolean payByCustomer);

    /**
     * This method will return all the delivery modes after selecting Ship to Home, Hotel or Business shipping group
     * depending on the selected carrier with not AM delivery mode.
     * @param carrier ie., UPS or FedEx
     * @param mode i.e., standard or overnight
     * @param pstCutOffTime for time condition
     * @return Collection of ZoneDeliveryModeModel
     */
    Collection<ZoneDeliveryModeModel> getShipToHomeDeliveryModesNotLike(final String carrier, final String mode, final String pstCutOffTime,
                                                                        final boolean payByCustomer);

    /**
     * This method will fetch all the partner-pickup-zones from DB who has delivery-modes associated to it
     * @return Collection of PartnerPickUpStoreModel
     */
    Collection<PartnerPickUpStoreModel> getPartnerPickUpStore();

    /**
     * This method will fetch the partner-pickup-zone from DB who has delivery-modes associated to it
     * @param partnerZone i.e., name
     * @return PartnerPickUpStoreModel object
     */
    PartnerPickUpStoreModel getPartnerPickUpStoreFromPartnerZone(final String partnerZone);

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group
     *  depending on the selected zone.
     * @param partnerZone for PartnerPickupStore
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneDeliveryModes(final String partnerZone, final boolean payByCustomer);

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group for the UPS Store.
     *
     * @param mode i.e, standard or Overnight
     * @param pstCutOffTime for time condition
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModes(final String mode, final String pstCutOffTime,
                                                                                  final boolean payByCustomer);

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group for the UPS Store not
     * like AM condition
     *
     * @param mode i.e, standard or Overnight
     * @param pstCutOffTime for time condition
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeModel> getPartnerZoneUPSStoreDeliveryModesNotLike(final String mode, final String pstCutOffTime,
                                                                                         final boolean payByCustomer);

    /**
     * This method will fetch all time windows for RushDelivery depending on deliveryType attribute
     * @param deliveryMode to specify SF or NYC Shipping group
     * @param pstCutOffTime for time condition
     * @return Collection of BlRushDeliveryModeModel
     */
    Collection<BlRushDeliveryModeModel> getBlRushDeliveryModes(final String deliveryMode, final String pstCutOffTime, final boolean payByCustomer);

    /**
     * This method will fetch shipping cost model for calculated value for variable delivery cost model
     *
     * @param calculatedCost value\
     * @param deliveryMethod name
     * @return SHippingCostModel
     */
    ShippingCostModel getShippingCostForCalculatedDeliveryCost(final String calculatedCost, final String deliveryMethod);

    /**
     * This method will fetch dimensional factor stored on base store
     *
     * @param baseStore name/code
     * @return dimensionalFactor
     */
    Integer getDimensionalFactorForDeliveryFromStore(final String baseStore);

    /**
     * This method will give rush model for delivery type for warehouse zipCode
     *
     * @param deliveryMode
     * @param payByCustomer
     * @return
     */
    BlRushDeliveryModeModel getBlRushDeliveryModeForWarehouseZipCode(final String deliveryMode, final boolean payByCustomer);

	/**
	 * This method will return all the delivery mode
	 *
	 * @return
	 */
	Collection<ZoneDeliveryModeModel> getAllBlDeliveryModes();

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
     * This method will return Collection of Consignment models with input
     *
     * @param optimizedShippingMethodEnum 3 or 2 or overnight
     * @return Collection<ConsignmentModel>
     */
    Collection<ConsignmentModel> getAllGroundedConsignments(final String optimizedShippingMethodEnum);
}
