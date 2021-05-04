package com.bl.facades.shipping;

import com.bl.facades.shipping.data.BlPartnerPickUpStoreData;
import com.bl.facades.shipping.data.BlPickUpZoneDeliveryModeData;
import com.bl.facades.shipping.data.BlRushDeliveryModeData;
import com.bl.facades.shipping.data.BlShippingGroupData;
import com.bl.storefront.forms.BlPickUpByForm;
import de.hybris.platform.acceleratorfacades.order.AcceleratorCheckoutFacade;
import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import de.hybris.platform.commercefacades.order.data.ZoneDeliveryModeData;

import java.util.Collection;

/**
 * {javadoc}
 *
 * @auther Namrata Lohar
 */
public interface BlCheckoutFacade extends AcceleratorCheckoutFacade {

    /**
     * This method will fetch all the shipping groups from service
     *
     * @return Collection of ShippingGroups
     */
    Collection<BlShippingGroupData> getAllShippingGroups();

    /**
     * THis method will check shipping group and will call appropriate deliveryMode method
     *
     * @param shippingGroup selected on frontend
     * @param partnerZone selected on frontend
     * @param pinCode zipcode
     * @return Collection of data
     */
    Collection<? extends DeliveryModeData> getSupportedDeliveryModes(final String shippingGroup, final String partnerZone,
                                                                            final String pinCode);

    /**
     * method with contain business logic for Ship to Home, Hotel or Business Delivery Modes to fetch all modes from service
     * @param rentalStart date
     * @param rentalEnd date
     * @return Collection object with delivery-modes differentiating in UPS and FedEx
     */
    Collection<ZoneDeliveryModeData> getAllShipToHomeDeliveryModes(final String rentalStart, final String rentalEnd);

    /**
     *This method will fetch all the Partner PickUp Store from service
     *
     * @return Collection of PartnerPickUp Stores
     */
    Collection<BlPartnerPickUpStoreData> getAllPartnerPickUpStore();

    /**
     * method with contain business logic for Partner-PickUp to fetch all modes from service
     * @param rentalStart date
     * @param rentalEnd date
     * @return Collection object with delivery-modes differentiating in Partner-PickUp Stores
     */
    Collection<BlPickUpZoneDeliveryModeData> getAllUSPStoreDeliveryModes(final String rentalStart, final String rentalEnd);

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group
     *  depending on the selected zone from service.
     * @param partnerZone i.e., name
     * @param rentalStart date
     * @param rentalEnd date
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeData> getPartnerZoneDeliveryModes(final String partnerZone, final String rentalStart,
                                                                         final String rentalEnd);

    /**
     * This method will fetch all time windows for RushDelivery depending on deliveryType attribute from service
     * @param deliveryMode to specify SF or NYC Shipping group
     * @param pstCutOffTime for time condition
     * @param pinCode entered by user
     * @return Collection of BlRushDeliveryModeModel
     */
    Collection<BlRushDeliveryModeData> getBlRushDeliveryModes(final String pinCode, final String deliveryMode, final String pstCutOffTime);

    /**
     * This methodwill check pinCode validity for UPS stores
     *
     * @param pinCode
     * @return valid or not
     */
    boolean checkPartnerPickCodeValidity(final String pinCode);

    /**
     * This method will save pickUp person details on cart
     *
     * @param blPickUpByForm if pick up is scheduled by someone
     */
    void savePickUpInfoOnCart(final BlPickUpByForm blPickUpByForm);

    /**
     * This methos will reset the delivery details from cart
     *
     * @return success or error
     */
    String removeDeliveryDetails();
}
