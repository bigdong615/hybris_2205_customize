package com.bl.facade;

import static org.mockito.BDDMockito.given;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.math.BigDecimal;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.services.gitfcard.BlGiftCardService;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
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
	  // MockitoAnnotations.initMocks(this);
    final CartData cartData = new CartData();
    final OrderData orderData = new OrderData();
    given(cartFacade.getSessionCart()).willReturn(cartData);
    given(checkoutFacade.placeOrder()).willReturn(orderData);
  }

  @Test
  public void testValidateAppliedGiftcard() {

    final CartData cartData = new CartData();
    final PriceData value = new PriceData();
    value.setValue(BigDecimal.valueOf(50));
    Assert.assertNotNull(value);
    cartData.setGiftCardDiscount(value);
    final CartData grandTotal = new CartData();
    final PriceData grand = new PriceData();
    grand.setValue(BigDecimal.valueOf(50));
    grandTotal.setGrandTotal(grand);
  }
}
