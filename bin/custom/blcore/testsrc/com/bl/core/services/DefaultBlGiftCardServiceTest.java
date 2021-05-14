package com.bl.core.services;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.order.impl.BLCommerceCartCalculationStrategy;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
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
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.runners.MockitoJUnitRunner;

import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.mock;
import static org.mockito.BDDMockito.doReturn;
import static org.mockito.BDDMockito.when;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;


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
    private BLCommerceCartCalculationStrategy blCommerceCartCalculationStrategy;

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

    private static final String GIFTCARD_CODE ="testGiftCard";

    @Before
    public void setUp() throws InvalidCartException {
        MockitoAnnotations.initMocks(this);
        abstractOrderModel = mock(AbstractOrderModel.class);

    }

    @Test
    public void testRemoveGiftcard(){

        //List<GiftCardModel> giftList = mock(ArrayList.class);
        GiftCardModel giftCardModel = mock(GiftCardModel.class);
        //giftList.add(giftCardModel);

        final SearchResult<GiftCardModel> searchResult = mock(SearchResult.class);

        giftCardModel.setCode(GIFTCARD_CODE);

        List<GiftCardModel> giftList = getGiftCardModelList();
        when(searchResult.getResult()).thenReturn(giftList);

        //when(gif  tCardDao.getGiftCard("test")).thenReturn(giftCardModel);
        final CartModel cartModel = mock(CartModel.class);
        given(cartService.getSessionCart()).willReturn(cartModel);

        giftCardNotNull(giftList, cartModel);
    }

    private void giftCardNotNull(final List<GiftCardModel> giftList, final CartModel cartModel){

        given(cartModel.getGiftCard()).willReturn(giftList);
        Collection<String> couponCodes = mock(Collection.class);
        couponCodes.add("gcTest1");
        couponCodes.add("gcTest2");
        cartModel.setAppliedCouponCodes(couponCodes);

        when(cartModel.getAppliedCouponCodes()).thenReturn(couponCodes);

        cartModel.setGiftCard(giftList);
        cartModel.setAppliedCouponCodes(couponCodes);
        cartModel.setCalculated(Boolean.FALSE);
        modelService.save(cartModel);

        final CommerceCartParameter commerceCartParameter = mock(CommerceCartParameter.class);
        commerceCartParameter.setCart(cartModel);
        commerceCartParameter.setBaseSite(cartModel.getSite());
        commerceCartParameter.setEnableHooks(true);
        commerceCartParameter.setRecalculate(true);
        blCommerceCartCalculationStrategy.calculateCart(commerceCartParameter);

        modelService.refresh(cartModel);

    }



    private List<GiftCardModel> getGiftCardModelList() {
        CurrencyModel currencyModel = mock(CurrencyModel.class);
        currencyModel.setSymbol("USD");
        CustomerModel customerModel = mock(CustomerModel.class);

        GiftCardMovementModel giftCardMovementModel = mock(GiftCardMovementModel.class);
        giftCardMovementModel.setAmount(100d);

        ArrayList giftCardMovementModelList = mock(ArrayList.class);
        giftCardMovementModelList.add(giftCardMovementModel);



        GiftCardModel model = mock(GiftCardModel.class);

        model.setCode(GIFTCARD_CODE);
        model.setActive(Boolean.TRUE);
        model.setCustomer(customerModel);
        model.setAmount(100d);
        model.setCurrency(currencyModel);
        model.setMovements(giftCardMovementModelList);

        when(model.getCode()).thenReturn(GIFTCARD_CODE);
        when(model.getActive()).thenReturn(Boolean.TRUE);
        when(model.getEmail()).thenReturn(Boolean.TRUE);
        when(model.getAmount()).thenReturn(Double.valueOf(100));
        when(model.getCurrency()).thenReturn(currencyModel);
        when(model.getCustomer()).thenReturn(customerModel);
        when(model.getMovements()).thenReturn(giftCardMovementModelList);
        return Arrays.asList(model);
    }


    @Test
    public void testApplyGiftCard(){

        Assert.assertNotNull(GIFTCARD_CODE);
        final CartModel cartModel = mock(CartModel.class);

        given(cartService.getSessionCart()).willReturn(cartModel);

        final SearchResult<GiftCardModel> searchResult = mock(SearchResult.class);

        List<GiftCardModel> giftList = getGiftCardModelList();
        Assert.assertNotNull(giftList);
        when(searchResult.getResult()).thenReturn(giftList);

        Assert.assertNotNull(giftList.get(0).getActive());
        Assert.assertNotNull(giftList.get(0).getCustomer());
        Assert.assertNotNull(giftList.get(0).getAmount());
        Assert.assertNotNull(giftList.get(0).getMovements());
        Assert.assertNotNull(giftList.get(0).getBalance());
    }

   @Test
    public void testCalculateGiftCard(){


        final List<GiftCardModel> giftCards = getGiftCardModelList();

        when(abstractOrderModel.getGiftCard()).thenReturn(giftCards);
        Assert.assertNotNull(giftCards);
        calculateGiftCardBalance(getGiftCardModelList());

    }

    private double calculateGiftCardBalance(List<GiftCardModel> giftCards){

        List<GiftCardMovementModel> giftCardMovementModelList = giftCards.get(0).getMovements();
        Assert.assertNotNull(giftCardMovementModelList);

        return 100d;
    }

}
