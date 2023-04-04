package com.bl.core.services;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.order.impl.DefaultBlCommerceCartCalculationStrategy;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlGiftCardServiceTest {

  @Mock
  private CartService cartService;

  @Mock
  private ModelService modelService;

  @Mock
  BaseStoreService baseStoreService;

  @Mock
  BaseSiteService baseSiteService;

  @Mock
  private DefaultBlCommerceCartCalculationStrategy defaultBlCommerceCartCalculationStrategy;

    /*@Mock
    private PriceDataFactory priceDataFactory;*/

  @Mock
  private CommonI18NService commonI18NService;

  @Mock
  private UserService userService;

  @Mock
  private EventService eventService;

  @Mock
  AbstractOrderModel abstractOrderModel;

  private static final String GIFTCARD_CODE = "testGiftCard";

  @Before
  public void setUp() throws InvalidCartException {
    MockitoAnnotations.initMocks(this);
    abstractOrderModel = mock(AbstractOrderModel.class);

  }

  @Test
  public void testRemoveGiftCard() {

    final GiftCardModel giftCardModel = mock(GiftCardModel.class);
    final SearchResult<GiftCardModel> searchResult = mock(SearchResult.class);
    giftCardModel.setCode(GIFTCARD_CODE);
    final List<GiftCardModel> giftList = getGiftCardModelList();
    when(searchResult.getResult()).thenReturn(giftList);
    final CartModel cartModel = mock(CartModel.class);
    given(cartService.getSessionCart()).willReturn(cartModel);
    Assert.assertNotNull(giftCardModel);
    giftCardNotNull(giftList, cartModel);
  }

  private void giftCardNotNull(final List<GiftCardModel> giftList, final CartModel cartModel) {

    Assert.assertNotNull(giftList);
    Assert.assertNotNull(cartModel);
    given(cartModel.getGiftCard()).willReturn(giftList);
    when(cartModel.getGiftCard()).thenReturn(giftList);
    cartModel.setGiftCard(giftList);
    cartModel.setCalculated(Boolean.FALSE);
    modelService.save(cartModel);
    final CommerceCartParameter commerceCartParameter = mock(CommerceCartParameter.class);
    commerceCartParameter.setCart(cartModel);
    commerceCartParameter.setBaseSite(cartModel.getSite());
    commerceCartParameter.setEnableHooks(true);
    commerceCartParameter.setRecalculate(true);
    defaultBlCommerceCartCalculationStrategy.calculateCart(commerceCartParameter);
    modelService.refresh(cartModel);
  }

  private List<GiftCardModel> getGiftCardModelList() {
    final CurrencyModel currencyModel = mock(CurrencyModel.class);
    currencyModel.setSymbol("USD");
    final CustomerModel customerModel = mock(CustomerModel.class);

    final GiftCardMovementModel giftCardMovementModel = mock(GiftCardMovementModel.class);
    giftCardMovementModel.setAmount(100d);

    final ArrayList giftCardMovementModelList = mock(ArrayList.class);
    giftCardMovementModelList.add(giftCardMovementModel);

    final GiftCardModel model = mock(GiftCardModel.class);

    model.setCode(GIFTCARD_CODE);
    model.setActive(Boolean.TRUE);
    model.setAmount(100d);
    model.setCurrency(currencyModel);
    model.setMovements(giftCardMovementModelList);

    when(model.getCode()).thenReturn(GIFTCARD_CODE);
    when(model.getActive()).thenReturn(Boolean.TRUE);
    when(model.getEmail()).thenReturn(Boolean.TRUE);
    when(model.getAmount()).thenReturn(Double.valueOf(100));
    when(model.getCurrency()).thenReturn(currencyModel);
    when(model.getMovements()).thenReturn(giftCardMovementModelList);
    return Arrays.asList(model);
  }


  @Test
  public void testApplyGiftCard() {

    Assert.assertNotNull(GIFTCARD_CODE);
    final CartModel cartModel = mock(CartModel.class);

    given(cartService.getSessionCart()).willReturn(cartModel);

    final SearchResult<GiftCardModel> searchResult = mock(SearchResult.class);

    final List<GiftCardModel> giftList = getGiftCardModelList();
    Assert.assertNotNull(giftList);
    when(searchResult.getResult()).thenReturn(giftList);

    Assert.assertNotNull(giftList.get(0).getActive());
    //Assert.assertNotNull(giftList.get(0).getCustomer());
    Assert.assertNotNull(giftList.get(0).getAmount());
    Assert.assertNotNull(giftList.get(0).getMovements());
    Assert.assertNotNull(giftList.get(0).getBalance());
  }

  @Test
  public void testCalculateGiftCard() {

    final List<GiftCardModel> giftCards = getGiftCardModelList();

    when(abstractOrderModel.getGiftCard()).thenReturn(giftCards);
    Assert.assertNotNull(giftCards);
    calculateGiftCardBalance(getGiftCardModelList());

  }

  private double calculateGiftCardBalance(final List<GiftCardModel> giftCards) {

    final List<GiftCardMovementModel> giftCardMovementModelList = giftCards.get(0).getMovements();
    Assert.assertNotNull(giftCardMovementModelList);

    return 100d;
  }

}
