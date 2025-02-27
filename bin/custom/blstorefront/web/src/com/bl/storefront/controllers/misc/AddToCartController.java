/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.misc;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.core.utils.BlReplaceMentOrderUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.ControllerConstants;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.bl.storefront.forms.GiftCardPurchaseForm;
import de.hybris.platform.acceleratorfacades.product.data.ProductWrapperData;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.AbstractController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddToCartForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddToCartOrderForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddToEntryGroupForm;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.converters.populator.GroupCartModificationListPopulator;
import de.hybris.platform.commercefacades.order.data.AddToCartParams;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.commercefacades.storesession.StoreSessionFacade;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.util.Config;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.annotation.Resource;
import javax.validation.Valid;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

/**
 * Controller for Add to Cart functionality which is not specific to a certain page.
 */
@Controller
public class AddToCartController extends AbstractController {
    private static final String QUANTITY_ATTR = "quantity";
    private static final String TYPE_MISMATCH_ERROR_CODE = "typeMismatch";
    private static final String ERROR_MSG_TYPE = "errorMsg";
    private static final String QUANTITY_INVALID_BINDING_MESSAGE_KEY = "basket.error.quantity.invalid.binding";
    private static final String SHOWN_PRODUCT_COUNT = "blstorefront.storefront.minicart.shownProductCount";
    private static final String PRODUCT_LIMIT = "addtocart.dontforget.product.limit";
    private static final String REDIRECT_CART_URL = REDIRECT_PREFIX + "/cart";
    private static final Logger LOG = Logger.getLogger(AddToCartController.class);

    private static final String IS_QUANTITY_FROM_ADD_TO_CART_POPUP = "isQuantityFromAddToCartPopup";

    @Resource(name = "cartFacade")
    private CartFacade cartFacade;

    @Resource(name = "productVariantFacade")
    private ProductFacade productFacade;

    @Resource(name = "groupCartModificationListPopulator")
    private GroupCartModificationListPopulator groupCartModificationListPopulator;

    @Resource(name = "cartFacade")
    private BlCartFacade blCartFacade;

    @Resource(name = "enumerationService")
    private EnumerationService enumerationService;

    @Resource(name="sessionService")
    protected SessionService sessionService;

    @Resource(name = "storeSessionFacade")
    private StoreSessionFacade storeSessionFacade;

    @ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
    private RentalDateDto getRentalsDuration() {
        return BlRentalDateUtils.getRentalsDuration();
    }

    @RequestMapping(value = "/cart/add", method = RequestMethod.POST, produces = "application/json")
    public String addToCart(@RequestParam("productCodePost") final String code,
                            @RequestParam("serialProductCodePost") final String serialCode, final Model model, @Valid final AddToCartForm form,
                            final BindingResult bindingErrors) {
        validateParameterNotNull(code, "Product code must not be null");
        validateParameterNotNull(serialCode, "Serial code must not be null");

        if (bindingErrors.hasErrors()) {
            return getViewWithBindingErrorMessages(model, bindingErrors);
        }

  		final String warningPopup = productAllowedInAddToCart(code, serialCode);
  		if (warningPopup != null)
  		{
  			return warningPopup;
  		}

        final long qty = form.getQty();

        if (qty <= 0) {
            model.addAttribute(ERROR_MSG_TYPE, "basket.error.quantity.invalid");
            model.addAttribute(QUANTITY_ATTR, Long.valueOf(0L));
        } else {
            try {

                final CartModificationData cartModification = blCartFacade.addToCart(code, qty, serialCode);
                model.addAttribute(QUANTITY_ATTR, Long.valueOf(cartModification.getQuantityAdded()));
                model.addAttribute("entry", cartModification.getEntry());
                model.addAttribute("cartCode", cartModification.getCartCode());
                model.addAttribute("isQuote", cartFacade.getSessionCart().getQuoteData() != null ? Boolean.TRUE : Boolean.FALSE);

                if (cartModification.getQuantityAdded() == 0L) {
                    model.addAttribute(ERROR_MSG_TYPE, "basket.information.quantity.noItemsAdded." + cartModification.getStatusCode());
                } else if (cartModification.getQuantityAdded() < qty) {
                    model.addAttribute(ERROR_MSG_TYPE,
                            "basket.information.quantity.reducedNumberOfItemsAdded." + cartModification.getStatusCode());
                }
            } catch (final CommerceCartModificationException ex) {
                logDebugException(ex);
                model.addAttribute(ERROR_MSG_TYPE, "basket.error.occurred");
                model.addAttribute(QUANTITY_ATTR, Long.valueOf(0L));
            } catch (final UnknownIdentifierException ex) {
                LOG.debug(String.format("Product could not be added to cart - %s", ex.getMessage()));
                model.addAttribute(ERROR_MSG_TYPE, "basket.error.occurred");
                model.addAttribute(QUANTITY_ATTR, Long.valueOf(0L));
                return ControllerConstants.Views.Fragments.Cart.AddToCartPopup;
            }
        }

        model.addAttribute("product", productFacade.getProductForCodeAndOptions(code, Arrays.asList(ProductOption.BASIC,ProductOption.CATEGORIES,ProductOption.REQUIRED_DATA)));
        final List<ProductOption> PRODUCT_OPTIONS = Arrays.asList(ProductOption.BASIC, ProductOption.PRICE,
                ProductOption.REQUIRED_DATA, ProductOption.GALLERY, ProductOption.STOCK,ProductOption.REQUIRED_WISHLIST);
        final Integer productsLimit = Integer.valueOf(Config.getInt(PRODUCT_LIMIT, 50));
      final List<ProductReferenceTypeEnum> productReferenceTypeEnums= getEnumerationService().getEnumerationValues(ProductReferenceTypeEnum._TYPECODE);
      productReferenceTypeEnums.remove(ProductReferenceTypeEnum.CONSISTS_OF);
      final List<ProductReferenceData> productReferences = productFacade.getProductReferencesForCode(code,
          productReferenceTypeEnums, PRODUCT_OPTIONS, productsLimit);

        model.addAttribute(BlControllerConstants.PRODUCT_REFERENCE, productReferences);
        model.addAttribute(BlControllerConstants.MAXIMUM_LIMIT, productsLimit);

        if(BooleanUtils.isTrue(BlReplaceMentOrderUtils.isReplaceMentOrder()) && null != sessionService.getAttribute(
                BlCoreConstants.RETURN_REQUEST)) {
            model.addAttribute(BlControllerConstants.REPLACEMENT_ORDER, Boolean.TRUE);
        } else {
            model.addAttribute(BlControllerConstants.REPLACEMENT_ORDER, Boolean.FALSE);
        }

        addAmountToGetFreeShipping(model);
        return ControllerConstants.Views.Fragments.Cart.AddToCartPopup;


    }
 	// Created separate method for product allowed in AddToCart.
 	private String productAllowedInAddToCart(final String code, final String serialCode)
 	{
 		final boolean isGiftCart = blCartFacade.cartHasGiftCard(code);
 		if (blCartFacade.isGiftCardProduct(code))
 		{
 			if (isGiftCart)
 			{
 				BlLogger.logMessage(LOG, Level.DEBUG, BlControllerConstants.MULTIPLEGIFTCARD);
 				return ControllerConstants.Views.Fragments.Cart.MultipleGiftCardWarningPopup;
 			}
 			else if (blCartFacade.cartHasRentalOrUsedGearProducts())
 			{
 				BlLogger.logMessage(LOG, Level.DEBUG, BlControllerConstants.GIFTCARDNOTALLOWE);
 				return ControllerConstants.Views.Fragments.Cart.GiftCardNotAllowedWarningPopup;
 			}
 		}
 		else if(blCartFacade.isNewGearProductAllowToAdd(code,serialCode)){
      BlLogger.logMessage(LOG, Level.DEBUG, BlControllerConstants.ADDTOCARTWARNING);
      return ControllerConstants.Views.Fragments.Cart.AddToCartWarningPopup;
    }
 		else{
 			 if (isGiftCart)
 	 		{
 				BlLogger.logMessage(LOG, Level.DEBUG, BlControllerConstants.GIFTCARDNOTALLOWE);
 				return ControllerConstants.Views.Fragments.Cart.GiftCardNotAllowedWarningPopup;
 	 		}

 		else if (blCartFacade.isRentalProductAddedToCartInUsedGearCart(code, serialCode))
 		{
 			BlLogger.logMessage(LOG, Level.DEBUG, BlControllerConstants.ADDTOCARTWARNING);
 			return ControllerConstants.Views.Fragments.Cart.AddToCartWarningPopup;
 		}

 		}
 		return null;
 	}

 	// Created separate method for add to cart according to code.
 	private CartModificationData addToCart(final String code, final long qty, final String serialCode, final GiftCardPurchaseForm giftCardForm)
 			throws CommerceCartModificationException
 	{
 		return (blCartFacade.isGiftCardProduct(code)) ? blCartFacade.addToCart(code, qty, serialCode, giftCardForm) : blCartFacade.addToCart(code, qty, serialCode);
 	}

 	// Created separate method for to redirect gift card popup.
 	private String addToCartForGiftCard(final Model model, final CartModificationData cartModification, final String code)
 	{
 		model.addAttribute("product", productFacade.getProductForCodeAndOptions(code, Arrays.asList(ProductOption.BASIC)));
 		model.addAttribute("entry", cartModification.getEntry());
 		return ControllerConstants.Views.Fragments.Cart.AddToCartGiftCardPopup;

 	}


    //created separate method for add serial product to cart and redirect it to cart page.

    @RequestMapping(value = "/cart/usedgearadd", method = RequestMethod.GET, produces = "application/json")
    public String addToCartForUsedGear(@RequestParam(value = "productCodePost") final String code, @Valid final GiftCardPurchaseForm giftCardForm,
                                       @RequestParam(value = "serialProductCodePost") final String serialCode, final Model model,
                                       @Valid final AddToCartForm form, final BindingResult bindingErrors, final RedirectAttributes redirectAttributes) {
        validateParameterNotNull(code, "Product code must not be null");
        validateParameterNotNull(serialCode, "Serial code must not be null");

        if (bindingErrors.hasErrors()) {
            return getViewWithBindingErrorMessages(model, bindingErrors);
        }
      try{
      final String warningPopup = productAllowedInAddToCart(code, serialCode);
  		if (warningPopup != null)
  		{
  			return warningPopup;
  		}
      }catch(final Exception ex){
          BlLogger.logMessage(LOG, Level.ERROR, "Product Not Addd to cart",ex);
          return REDIRECT_CART_URL;
      }


        final long qty = form.getQty();

        if (qty <= 0) {
      	  GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, getLocalizedString("basket.error.quantity.invalid"), null);
        } else {
            try {

            	 final CartModificationData cartModification = addToCart(code, qty, serialCode, giftCardForm);
                if (cartModification.getQuantityAdded() == 0L) {
                   GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, getLocalizedString("basket.information.quantity.noItemsAdded."), null);
                } else if (cartModification.getQuantityAdded() < qty) {
               	 GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, getLocalizedString("basket.information.quantity.reducedNumberOfItemsAdded."), null);
                }
                if (blCartFacade.isGiftCardProduct(code)){
    					return addToCartForGiftCard(model, cartModification, code);
    				}
            } catch (final CommerceCartModificationException ex) {
                logDebugException(ex);
                GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, getLocalizedString("basket.error.occurred"), null);
            } catch (final UnknownIdentifierException ex) {
                LOG.debug(String.format("Product could not be added to cart - %s", ex.getMessage()));
                GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, getLocalizedString("basket.error.occurred"), null);
               return REDIRECT_CART_URL;
            }
        }

        return REDIRECT_CART_URL;
    }


    protected String getViewWithBindingErrorMessages(final Model model, final BindingResult bindingErrors) {
        for (final ObjectError error : bindingErrors.getAllErrors()) {
            if (isTypeMismatchError(error)) {
                model.addAttribute(ERROR_MSG_TYPE, QUANTITY_INVALID_BINDING_MESSAGE_KEY);
            } else {
                model.addAttribute(ERROR_MSG_TYPE, error.getDefaultMessage());
            }
        }
        return ControllerConstants.Views.Fragments.Cart.AddToCartPopup;
    }

    protected boolean isTypeMismatchError(final ObjectError error) {
        return error.getCode().equals(TYPE_MISMATCH_ERROR_CODE);
    }

    @RequestMapping(value = "/cart/addGrid", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    public final String addGridToCart(@RequestBody final AddToCartOrderForm form, final Model model) {
        final Set<String> multidErrorMsgs = new HashSet<String>();
        final List<CartModificationData> modificationDataList = new ArrayList<CartModificationData>();

        for (final OrderEntryData cartEntry : form.getCartEntries()) {
            if (!isValidProductEntry(cartEntry)) {
                LOG.error("Error processing entry");
            } else if (!isValidQuantity(cartEntry)) {
                multidErrorMsgs.add("basket.error.quantity.invalid");
            } else {
                final String errorMsg = addEntryToCart(modificationDataList, cartEntry, true);
                if (StringUtils.isNotEmpty(errorMsg)) {
                    multidErrorMsgs.add(errorMsg);
                }

            }
        }

        if (CollectionUtils.isNotEmpty(modificationDataList)) {
            groupCartModificationListPopulator.populate(null, modificationDataList);

            model.addAttribute("modifications", modificationDataList);
        }

        if (CollectionUtils.isNotEmpty(multidErrorMsgs)) {
            model.addAttribute("multidErrorMsgs", multidErrorMsgs);
        }

        model.addAttribute("numberShowing", Integer.valueOf(Config.getInt(SHOWN_PRODUCT_COUNT, 3)));


        return ControllerConstants.Views.Fragments.Cart.AddToCartPopup;
    }

    @RequestMapping(value = "/cart/addQuickOrder", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
    public final String addQuickOrderToCart(@RequestBody final AddToCartOrderForm form, final Model model) {
        final List<CartModificationData> modificationDataList = new ArrayList();
        final List<ProductWrapperData> productWrapperDataList = new ArrayList();
        final int maxQuickOrderEntries = Config.getInt("blstorefront.quick.order.rows.max", 25);
        final int sizeOfCartEntries = CollectionUtils.size(form.getCartEntries());
        form.getCartEntries().stream().limit(Math.min(sizeOfCartEntries, maxQuickOrderEntries)).forEach(cartEntry -> {
            String errorMsg = StringUtils.EMPTY;
            final String sku = !isValidProductEntry(cartEntry) ? StringUtils.EMPTY : cartEntry.getProduct().getCode();
            if (StringUtils.isEmpty(sku)) {
                errorMsg = "text.quickOrder.product.code.invalid";
            } else if (!isValidQuantity(cartEntry)) {
                errorMsg = "text.quickOrder.product.quantity.invalid";
            } else {
                errorMsg = addEntryToCart(modificationDataList, cartEntry, false);
            }

            if (StringUtils.isNotEmpty(errorMsg)) {
                productWrapperDataList.add(createProductWrapperData(sku, errorMsg));
            }
        });

        if (CollectionUtils.isNotEmpty(productWrapperDataList)) {
            model.addAttribute("quickOrderErrorData", productWrapperDataList);
            model.addAttribute("quickOrderErrorMsg", "basket.quick.order.error");
        }

        if (CollectionUtils.isNotEmpty(modificationDataList)) {
            model.addAttribute("modifications", modificationDataList);
        }

        return ControllerConstants.Views.Fragments.Cart.AddToCartPopup;
    }

    @RequestMapping(value = "/entrygroups/cart/addToEntryGroup", method =
            {RequestMethod.POST, RequestMethod.GET}) //NOSONAR
    public String addEntryGroupToCart(final Model model, @Valid final AddToEntryGroupForm form, final BindingResult bindingErrors) {
        if (bindingErrors.hasErrors()) {
            return getViewWithBindingErrorMessages(model, bindingErrors);
        }
        final long qty = 1;
        try {
            final AddToCartParams addToCartParams = new AddToCartParams();
            addToCartParams.setEntryGroupNumbers(new HashSet(Collections.singletonList(form.getEntryGroupNumber())));
            addToCartParams.setProductCode(form.getProductCode());
            addToCartParams.setQuantity(qty);
            addToCartParams.setStoreId(null);
            final CartModificationData cartModification = cartFacade.addToCart(addToCartParams);
            model.addAttribute(QUANTITY_ATTR, Long.valueOf(cartModification.getQuantityAdded()));
            model.addAttribute("entry", cartModification.getEntry());
            model.addAttribute("cartCode", cartModification.getCartCode());

            if (cartModification.getQuantityAdded() == 0L) {
                model.addAttribute(ERROR_MSG_TYPE, "basket.information.quantity.noItemsAdded." + cartModification.getStatusCode());
            } else if (cartModification.getQuantityAdded() < qty) {
                model.addAttribute(ERROR_MSG_TYPE,
                        "basket.information.quantity.reducedNumberOfItemsAdded." + cartModification.getStatusCode());
            }
        } catch (final CommerceCartModificationException ex) {
            logDebugException(ex);
            model.addAttribute(ERROR_MSG_TYPE, "basket.error.occurred");
            model.addAttribute(QUANTITY_ATTR, Long.valueOf(0L));
        }
        model.addAttribute("product",
                productFacade.getProductForCodeAndOptions(form.getProductCode(), Arrays.asList(ProductOption.BASIC)));

        return REDIRECT_PREFIX + "/cart";
    }

    protected ProductWrapperData createProductWrapperData(final String sku, final String errorMsg) {
        final ProductWrapperData productWrapperData = new ProductWrapperData();
        final ProductData productData = new ProductData();
        productData.setCode(sku);
        productWrapperData.setProductData(productData);
        productWrapperData.setErrorMsg(errorMsg);
        return productWrapperData;
    }

    protected void logDebugException(final Exception ex) {
        if (LOG.isDebugEnabled()) {
            LOG.debug(ex);
        }
    }

    protected String addEntryToCart(final List<CartModificationData> modificationDataList, final OrderEntryData cartEntry,
                                    final boolean isReducedQtyError) {
        String errorMsg = StringUtils.EMPTY;
        try {
            final long qty = cartEntry.getQuantity().longValue();
            final CartModificationData cartModificationData = cartFacade.addToCart(cartEntry.getProduct().getCode(), qty);
            if (cartModificationData.getQuantityAdded() == 0L) {
                errorMsg = "basket.information.quantity.noItemsAdded." + cartModificationData.getStatusCode();
            } else if (cartModificationData.getQuantityAdded() < qty && isReducedQtyError) {
                errorMsg = "basket.information.quantity.reducedNumberOfItemsAdded." + cartModificationData.getStatusCode();
            }

            modificationDataList.add(cartModificationData);

        } catch (final CommerceCartModificationException ex) {
            errorMsg = "basket.error.occurred";
            logDebugException(ex);
        }
        return errorMsg;
    }

    @RequestMapping(value = "/cart/addproduct", method = RequestMethod.POST, produces = "application/json")
    public String addToCartFromRecommendedComponent(@RequestParam("productCodePost") final String code,
                            @RequestParam("serialProductCodePost") final String serialCode , @RequestParam("productQuantity") final Long quantity, final Model model, @Valid final AddToCartForm form,
                            final BindingResult bindingErrors){
        validateParameterNotNull(code, "Product code must not be null");
        validateParameterNotNull(serialCode, "Serial code must not be null");

        if (bindingErrors.hasErrors()) {
            return getViewWithBindingErrorMessages(model, bindingErrors);
        }

        final String warningPopup = productAllowedInAddToCart(code, serialCode);
        if (warningPopup != null)
        {
            return warningPopup;
        }

        final long qty = quantity;

        if (qty <= 0) {
            model.addAttribute(ERROR_MSG_TYPE, "basket.error.quantity.invalid");
            model.addAttribute(QUANTITY_ATTR, Long.valueOf(0L));
        } else {
            try {

                sessionService.getCurrentSession().setAttribute(IS_QUANTITY_FROM_ADD_TO_CART_POPUP,Boolean.TRUE);
                final CartModificationData cartModification = blCartFacade.addToCart(code, qty, serialCode);
                model.addAttribute(QUANTITY_ATTR, Long.valueOf(cartModification.getQuantityAdded()));
                model.addAttribute("entry", cartModification.getEntry());
                model.addAttribute("cartCode", cartModification.getCartCode());
                model.addAttribute("isQuote", cartFacade.getSessionCart().getQuoteData() != null ? Boolean.TRUE : Boolean.FALSE);

                if (cartModification.getQuantityAdded() == 0L) {
                    model.addAttribute(ERROR_MSG_TYPE, "basket.information.quantity.noItemsAdded." + cartModification.getStatusCode());
                } else if (cartModification.getQuantityAdded() < qty) {
                    model.addAttribute(ERROR_MSG_TYPE,
                            "basket.information.quantity.reducedNumberOfItemsAdded." + cartModification.getStatusCode());
                }
            } catch (final CommerceCartModificationException ex) {
                logDebugException(ex);
                model.addAttribute(ERROR_MSG_TYPE, "basket.error.occurred");
                model.addAttribute(QUANTITY_ATTR, Long.valueOf(0L));
            } catch (final UnknownIdentifierException ex) {
                LOG.debug(String.format("Product could not be added to cart - %s", ex.getMessage()));
                model.addAttribute(ERROR_MSG_TYPE, "basket.error.occurred");
                model.addAttribute(QUANTITY_ATTR, Long.valueOf(0L));
                return ControllerConstants.Views.Fragments.Cart.AddToCartPopup;
            }
        }

        model.addAttribute("product", productFacade.getProductForCodeAndOptions(code, Arrays.asList(ProductOption.BASIC,ProductOption.CATEGORIES,ProductOption.REQUIRED_DATA)));
        final List<ProductOption> PRODUCT_OPTIONS = Arrays.asList(ProductOption.BASIC, ProductOption.PRICE,
                ProductOption.REQUIRED_DATA, ProductOption.GALLERY, ProductOption.STOCK,ProductOption.REQUIRED_WISHLIST);
        final Integer productsLimit = Integer.valueOf(Config.getInt(PRODUCT_LIMIT, 50));
        final List<ProductReferenceTypeEnum> productReferenceTypeEnums= getEnumerationService().getEnumerationValues(ProductReferenceTypeEnum._TYPECODE);
        productReferenceTypeEnums.remove(ProductReferenceTypeEnum.CONSISTS_OF);
        final List<ProductReferenceData> productReferences = productFacade.getProductReferencesForCode(code,
                productReferenceTypeEnums, PRODUCT_OPTIONS, productsLimit);

        model.addAttribute(BlControllerConstants.PRODUCT_REFERENCE, productReferences);
        model.addAttribute(BlControllerConstants.MAXIMUM_LIMIT, productsLimit);

        if(BooleanUtils.isTrue(BlReplaceMentOrderUtils.isReplaceMentOrder()) && null != sessionService.getAttribute(
                BlCoreConstants.RETURN_REQUEST)) {
            model.addAttribute(BlControllerConstants.REPLACEMENT_ORDER, Boolean.TRUE);
        } else {
            model.addAttribute(BlControllerConstants.REPLACEMENT_ORDER, Boolean.FALSE);
        }

        addAmountToGetFreeShipping(model);
        return ControllerConstants.Views.Fragments.Cart.AddToCartPopup;


    }

    private void addAmountToGetFreeShipping(final Model model)
    {
        PriceData remainingAmountToGetFreeShipping= blCartFacade.addAmountToGetFreeShipping(blCartFacade.getSessionCart(),storeSessionFacade.getCurrentCurrency());
        model.addAttribute("amountToGetFreeShipping", remainingAmountToGetFreeShipping );
        model.addAttribute("minTotalForFreeShipping",Config.getParameter("bl.min.subtotal.for.free.shipping"));
    }

    protected boolean isValidProductEntry(final OrderEntryData cartEntry) {
        return cartEntry.getProduct() != null && StringUtils.isNotBlank(cartEntry.getProduct().getCode());
    }

    protected boolean isValidQuantity(final OrderEntryData cartEntry) {
        return cartEntry.getQuantity() != null && cartEntry.getQuantity().longValue() >= 1L;
    }

    public EnumerationService getEnumerationService() {
        return enumerationService;
    }

    public void setEnumerationService(final EnumerationService enumerationService) {
        this.enumerationService = enumerationService;
    }
}
