package com.bl.storefront.controllers.pages.checkout;

import com.bl.facades.shipping.data.BlPartnerPickUpStoreData;
import com.bl.facades.shipping.data.BlShippingGroupData;
import com.bl.storefront.forms.BlPickUpByForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.Collection;

/**
 * {javadoc}
 *
 * @auther Namrata Lohar
 */
public interface BlCheckoutStepController {

    /**
     * This method will fetch all the shipping groups from service
     *
     * @param model frontend data model
     * @param redirectAttributes redirect attributes
     * @return return delivery or pickup page with shipping groups
     */
    String getAllShippingGroups(final Model model, final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException;

    /**
     * THis method will check shipping group and will call appropriate deliveryMode method
     *
     * @param model frontend data model
     * @param redirectAttributes redirect attributes
     * @param shippingGroup selected on frontend
     * @param partnerZone selected on frontend
     * @return Collection
     */
    Collection<? extends DeliveryModeData> getSupportedDeliveryModes(final Model model, final RedirectAttributes redirectAttributes,
                                                                     final String shippingGroup, final String partnerZone);

    /**
     *This method will fetch all the Partner PickUp Store
     *
     * @return Collection of PartnerPickUp Stores
     */
    Collection<BlPartnerPickUpStoreData> getAllPartnerPickUpStore();

    /**
     * This method will save pickup person details on cart
     *
     * @param blPickUpByForm pickup person details
     * @param bindingResult binding attribute
     * @param model model attribute
     * @param redirectModel redirect parameters
     * @return string of success or failure
     */
    String savePickUpByFormOnCart(final BlPickUpByForm blPickUpByForm, final BindingResult bindingResult, final Model model,
                                  final RedirectAttributes redirectModel);

    /**
     * This method will remove added delivery details fromm cart
     *
     * @return error or success
     */
    String removeDeliveryDetailsFromCart();

}
