package com.bl.facades.shipping;

import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.facades.shipping.data.BlPartnerPickUpStoreData;
import com.bl.facades.shipping.data.BlPickUpZoneDeliveryModeData;
import com.bl.facades.shipping.data.BlRushDeliveryModeData;
import com.bl.facades.shipping.data.BlShippingGroupData;
import com.bl.storefront.forms.BlPickUpByForm;
import de.hybris.platform.acceleratorfacades.order.AcceleratorCheckoutFacade;
import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import de.hybris.platform.commercefacades.order.data.ZoneDeliveryModeData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;

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
     * @param payByCustomer for getting customer related delivery modes
     * @return Collection of data
     */
    Collection<? extends DeliveryModeData> getSupportedDeliveryModes(final String shippingGroup, final String partnerZone,
                                                                     final boolean payByCustomer);

    /**
     * method with contain business logic for Ship to Home, Hotel or Business Delivery Modes to fetch all modes from service
     * @param rentalStart date
     * @param rentalEnd date
     * @param payByCustomer to get customer related delivery modes
     * @return Collection object with delivery-modes differentiating in UPS and FedEx
     */
    Collection<ZoneDeliveryModeData> getAllShipToHomeDeliveryModes(final String rentalStart, final String rentalEnd,
                                                                   final boolean payByCustomer);

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
     * @param payByCustomer to get customer related delivery modes
     * @return Collection object with delivery-modes differentiating in Partner-PickUp Stores
     */
    Collection<BlPickUpZoneDeliveryModeData> getAllUSPStoreDeliveryModes(final String rentalStart, final String rentalEnd,
                                                                         final boolean payByCustomer);

    /**
     * This method will fetch all the delivery modes after selecting Partner pickup store shipping group
     *  depending on the selected zone from service.
     * @param partnerZone i.e., name
     * @param rentalStart date
     * @param rentalEnd date
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of BlPickUpZoneDeliveryModeModel
     */
    Collection<BlPickUpZoneDeliveryModeData> getPartnerZoneDeliveryModes(final String partnerZone, final String rentalStart,
                                                                         final String rentalEnd, final boolean payByCustomer);

    /**
     * This method will fetch all time windows for RushDelivery depending on deliveryType attribute from service
     * @param deliveryMode to specify SF or NYC Shipping group
     * @param pstCutOffTime for time condition
     * @param payByCustomer to get customer related delivery modes
     * @return Collection of BlRushDeliveryModeModel
     */
    Collection<BlRushDeliveryModeData> getBlRushDeliveryModes(final String deliveryMode, final String pstCutOffTime,
                                                              final boolean payByCustomer);

    /**
     * This method will check pinCode validity for UPS stores
     *
     * @param pinCode pin
     * @return UpsLocatorResposeData
     */
    UpsLocatorResposeData checkPartnerPickCodeValidity(final String pinCode);

    /**
     * This method will save pickUp person details on cart
     *
     * @param blPickUpByForm if pick up is scheduled by someone
     * @return String for success ot failure
     */
    String savePickUpInfoOnCart(final BlPickUpByForm blPickUpByForm);

    /**
     * This methos will reset the delivery details from cart
     *
     * @return success or error
     */
    String removeDeliveryDetails();

    /**
     * Set the delivery mode on the cart Checks if the deliveryMode code is supported. If the code is not supported it
     * does not get set and a false is returned.
     *
     * @param deliveryModeCode the delivery mode
     * @param status for saving mode address in cart address
     * @return true if successful
     */
    boolean setDeliveryMode(final String deliveryModeCode, final boolean status);

    /**
     * This method will check validity of user entered pinCode for SF or NYC
     *
     * @param pinCode to be checked for validity
     * @param deliveryType i.e., SF or NYC
     * @return return true / false
     */
    boolean checkSFOrNYCPinCodeValidity(final String pinCode, final String deliveryType);

    /**
     * This method will save following details on cart for rush delivery
     *
     * @param deliveryNote for rush delivery
     * @param statusUpdate for rush delivery
     * @return success or failure
     */
    String setDeliveryDetails(final String deliveryNote, final boolean statusUpdate);

    /**
     * This methos will save address on cart
     *
     * @param addressModel model
     */
    void setUPSAddressOnCart(final AddressModel addressModel);
    
 	/**
 	 * This method will return all the delivery modes
 	 *
 	 * @param payByCustomer
 	 * @return Collection of ZoneDeliveryModeModels
 	 */
 	Collection<ZoneDeliveryModeModel> getAllDeliveryModes(final boolean payByCustomer);
}
