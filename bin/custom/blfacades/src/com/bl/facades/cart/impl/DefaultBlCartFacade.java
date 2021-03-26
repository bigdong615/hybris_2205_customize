package com.bl.facades.cart.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.facades.cart.BlCartFacade;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.log4j.Logger;
import org.apache.log4j.Level;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.Collections;

/**
 * @author Neeraj Singh
 */
public class DefaultBlCartFacade implements BlCartFacade {

    private static final Logger LOGGER = Logger.getLogger(DefaultBlCartFacade.class);

    @Resource(name = "commerceCartService")
    private CommerceCartService commerceCartService;

    private ModelService modelService;

    private CartService cartService;

    /**
     * {@inheritDoc}
     * @return
     */
    @Override
    public void removeCartEntries() {

        final CartModel cartModel = getCartService().getSessionCart();

        if (!CollectionUtils.isEmpty(cartModel.getEntries())) {

            try {
                final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
                commerceCartParameter.setEnableHooks(true);
                commerceCartParameter.setCart(cartModel);
                getCommerceCartService().removeAllEntries(commerceCartParameter);
                getModelService().removeAll(cartModel.getAllPromotionResults());
                cartModel.setAppliedCouponCodes(Collections.emptySet());
                getModelService().save(cartModel);
                getModelService().refresh(cartModel);

                BlLogger.logFormattedMessage(LOGGER,Level.DEBUG,BlCoreConstants.EMPTY_STRING,"All entries removed from cart with code : {}", cartModel.getCode());
            } catch (final Exception ex) {
                BlLogger.logMessage(LOGGER, Level.ERROR, "Unable to remove cart entries :", ex);
            }
        }
    }

    private CommerceCartService getCommerceCartService() {
        return commerceCartService;
    }

    public void setCommerceCartService(CommerceCartService commerceCartService) {
        this.commerceCartService = commerceCartService;
    }

    public ModelService getModelService () {
        return modelService;
    }

    public void setModelService (ModelService modelService){
        this.modelService = modelService;
    }

    public CartService getCartService () {
        return cartService;
    }

    public void setCartService (CartService cartService){
        this.cartService = cartService;
    }
}