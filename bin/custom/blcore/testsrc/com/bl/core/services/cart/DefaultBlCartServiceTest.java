package com.bl.core.services.cart;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.servicelayer.model.ModelService;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Collections;

@UnitTest
public class DefaultBlCartServiceTest {

    @Mock
    private CommerceCartService commerceCartService;

    @Mock
    private ModelService modelService;

    private CartModel cartModel;

    @Before
    public void setup(){

        MockitoAnnotations.initMocks(this);
        cartModel = Mockito.mock(CartModel.class);
    }

    @Test
    public void testClearCartEntries(){

        Assert.assertNotNull(cartModel);
        final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
        commerceCartParameter.setEnableHooks(true);
        commerceCartParameter.setCart(cartModel);
        commerceCartService.removeAllEntries(commerceCartParameter);

        modelService.removeAll(cartModel.getAllPromotionResults());
        cartModel.setAppliedCouponCodes(Collections.emptySet());
        modelService.save(cartModel);
        modelService.refresh(cartModel);
    }
}
