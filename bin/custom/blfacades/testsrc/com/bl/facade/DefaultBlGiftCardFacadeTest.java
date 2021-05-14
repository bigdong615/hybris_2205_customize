package com.bl.facade;

import com.bl.core.services.gitfcard.BlGiftCardService;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.servicelayer.model.ModelService;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.math.BigDecimal;

import static org.mockito.BDDMockito.given;



@UnitTest
public class DefaultBlGiftCardFacadeTest {

    @Mock
    private ModelService modelService;

    @Mock
    private CheckoutFacade checkoutFacade;

    @Mock
    private CartFacade cartFacade;

    @Mock
    private BlGiftCardService giftCardService;

    @Mock
    private CheckoutCustomerStrategy checkoutCustomerStrategy;

    @Before
    public void setUp() throws InvalidCartException {
        MockitoAnnotations.initMocks(this);

        CartData cartData = new CartData();
        OrderData orderData = new OrderData();
        given(cartFacade.getSessionCart()).willReturn(cartData);
        given(checkoutFacade.placeOrder()).willReturn(orderData);

    }

    @Test
    public void testValidateAppliedGiftcard(){

        CartData cartData = new CartData();
        PriceData value = new PriceData();
        value.setValue(BigDecimal.valueOf(50));
        Assert.assertNotNull(value);
        cartData.setGiftCardDiscount(value);
        CartData grandTotal = new CartData();
        PriceData grand = new PriceData();
        grand.setValue(BigDecimal.valueOf(50));
        grandTotal.setGrandTotal(grand);
    }


}
