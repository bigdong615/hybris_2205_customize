package com.bl.core.services.cart.impl;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import java.util.Collections;
import java.util.List;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

@UnitTest
public class DefaultBlCartServiceTest {

     @Mock
     private CommerceCartService commerceCartService;

     @Mock
     private CartModel cartModel;

    @Before
    public void setup(){
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testClearCartEntries(){

        final AbstractOrderEntryModel entryModel = mock(AbstractOrderEntryModel.class);
        final List<AbstractOrderEntryModel> entries = Collections.singletonList(entryModel);

        given(cartModel.getEntries()).willReturn(entries);
        final CommerceCartParameter parameter = new CommerceCartParameter();
        parameter.setEnableHooks(true);
        parameter.setCart(cartModel);
        commerceCartService.removeAllEntries(parameter);
    }
}
