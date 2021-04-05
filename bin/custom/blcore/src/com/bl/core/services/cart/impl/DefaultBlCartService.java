package com.bl.core.services.cart.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.cart.BlCartService;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.impl.DefaultCartService;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.CollectionUtils;

/**
 * Default implementation of the {@link BlCartService}.
 * @author  Neeraj Singh
 */
public class DefaultBlCartService extends DefaultCartService implements BlCartService {

    private static final Logger LOGGER = Logger.getLogger(DefaultBlCartService.class);

    private CommerceCartService commerceCartService; // NOSONAR

    /**
     * {@inheritDoc}
     */
    @Override
    public void clearCartEntries(final CartModel cartModel) {

        if (!CollectionUtils.isEmpty(cartModel.getEntries())) {

                final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
                commerceCartParameter.setEnableHooks(true);
                commerceCartParameter.setCart(cartModel);
                getCommerceCartService().removeAllEntries(commerceCartParameter);

                BlLogger.logFormattedMessage(LOGGER, Level.DEBUG, BlCoreConstants.EMPTY_STRING,"All entries removed from cart with code : {}", cartModel.getCode());
        }
    }

    public CommerceCartService getCommerceCartService() {
        return commerceCartService;
    }

    public void setCommerceCartService(CommerceCartService commerceCartService) {
        this.commerceCartService = commerceCartService;
    }
}
