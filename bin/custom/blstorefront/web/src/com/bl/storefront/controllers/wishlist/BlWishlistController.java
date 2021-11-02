package com.bl.storefront.controllers.wishlist;

import atg.taglib.json.util.JSONException;
import atg.taglib.json.util.JSONObject;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.wishlist.BlWishListFacade;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractSearchPageController;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.store.services.BaseStoreService;
import javax.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.ControllerConstants;
import com.bl.storefront.controllers.pages.BlControllerConstants;


/**
 * This class handles the adding, removing product from wishlist and removing wishlist entries from
 * the wishlist
 *
 * @author Sahana SB
 */
@Controller
public class BlWishlistController extends AbstractSearchPageController {

	private static final Logger LOG = Logger.getLogger(BlWishlistController.class);
	private static final String REDIRECT_TO_BOOKMARKS_PAGE =
			REDIRECT_PREFIX + "/my-account/bookmarks";

	@Resource(name = "cartFacade")
	private BlCartFacade blCartFacade;

	@Resource(name = "wishlistFacade")
	private BlWishListFacade wishlistFacade;

	@Resource(name = "blDatePickerService")
	private BlDatePickerService blDatePickerService;

	@Resource(name = "blCommerceStockService")
	private BlCommerceStockService blCommerceStockService;

	@Resource(name = "baseStoreService")
	private BaseStoreService baseStoreService;

	/*
	 * Method to add Product to Wishlist from the Product Cards.
	 */
	@RequireHardLogIn
	@RequestMapping(value = "/wishlist/add", method = RequestMethod.GET)
	@ResponseBody
	public String addToWishlist(@RequestParam("productwishlistCode") final String code) {
		try {
			wishlistFacade.addToWishlist(code);
			return BlCoreConstants.SUCCESS;
		} catch (Exception e) {
			BlLogger.logMessage(LOG, Level.ERROR, "In default wishlist Product already present ", e);
			return BlCoreConstants.ERROR;
		}
	}

	/*
	 * Method to Remove Product from Wishlist from the Product Cards.
	 */
	@RequireHardLogIn
	@RequestMapping(value = "/removewishlist", method = RequestMethod.POST)
	@ResponseBody
	public String removeWishlistFromCards(@RequestParam("removeproductCode") final String code) {
		try {
			wishlistFacade.removeWishlist(code);
			return BlCoreConstants.SUCCESS;
		} catch (Exception e) {
			BlLogger.logMessage(LOG, Level.ERROR, "In default wishlist found more than one entry ", e);
			return BlCoreConstants.ERROR;
		}
	}

	/*
	 * Remove Wishlist Entry from the Accountbookmarks Page.
	 */
	@RequireHardLogIn
	@RequestMapping(value = "/removewishlistentry", method = RequestMethod.POST)
	public String removeWishlistEntry(@RequestParam("removeProductEntry") final String code) {
		try {
			wishlistFacade.removeWishlist(code);
			return REDIRECT_TO_BOOKMARKS_PAGE;
		} catch ( Exception e) {
			BlLogger.logMessage(LOG, Level.ERROR, "In default wishlist found more than one entry ", e);
			return REDIRECT_TO_BOOKMARKS_PAGE;
		}
	}

	/*
	 * Method to Add Wishlist Entry to Cart and remove it form the Wishlist entry
	 */
	@RequestMapping(value = "/bookmark/addtorental", method = RequestMethod.POST)
	public String addToCartAndRemoveEntry(@RequestParam(value = "page", defaultValue = "0") final int page,
			@RequestParam(value = "sort", required = false) final String sortCode, final Model model,
			@RequestParam("addtocartremoveProductEntry") final String code) {
		wishlistFacade.removeWishlist(code);
		try {
			final CartModificationData cartModification = blCartFacade.addToCart(code, 1, null);
		} catch (CommerceCartModificationException e) {
			BlLogger.logMessage(LOG, Level.ERROR, "Product with {} could not be added to cart", code);
		}
		return REDIRECT_TO_BOOKMARKS_PAGE;
	}
	/*
	 * Method check Only Rental Cart Present in cart
	 * @param Model
	 * return warning message or success
	 */
	@RequestMapping(value = "/bookmark/onlyRentalCartPresent", method = RequestMethod.POST , produces = "application/json")
	public String isOnlyRentalCartPresent(final Model model) throws JSONException {
		return productAllowedInAddToCart();
	}
	/*
	 * Method check product Allowed In AddToCart
	 * return warning message or success
	 */
private String productAllowedInAddToCart() throws JSONException {
		final boolean isGiftCart = blCartFacade.cartHasGiftCard(StringUtils.EMPTY);
		if (isGiftCart)
		{
			BlLogger.logMessage(LOG, Level.DEBUG, BlControllerConstants.GIFTCARDNOTALLOWE);
			return ControllerConstants.Views.Fragments.Cart.GiftCardNotAllowedWarningPopup;
		}
		return ControllerConstants.Views.Fragments.Cart.SUCCESS_JSON;
	}
}


