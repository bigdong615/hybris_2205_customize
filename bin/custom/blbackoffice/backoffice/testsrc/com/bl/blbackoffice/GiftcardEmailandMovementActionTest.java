package com.bl.blbackoffice;

import com.bl.blbackoffice.actions.GiftcardEmailandMovementAction;
import com.bl.core.event.BlGiftCardEmailEvent;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.hybris.cockpitng.actions.ActionContext;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.services.BaseStoreService;
import org.junit.Assert;
import org.junit.Before;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.BDDMockito.when;

@UnitTest
public class GiftcardEmailandMovementActionTest {

    @Spy
    @InjectMocks
    private GiftcardEmailandMovementAction action;

    @Mock
    private ModelService modelService;

    @Mock
    ActionContext<GiftCardModel> actionContext;

    @Mock
    private EventService eventService;

    @Mock
    private BaseSiteService baseSiteService;

    @Mock
    private BaseStoreService baseStoreService;

    @Mock
    private GiftCardModel giftCardModel;

    @Mock
    BlGiftCardEmailEvent event;


    private static final String GIFTCARD_CODE = "test";
    @Before
    public void setup(){

        MockitoAnnotations.initMocks(this);
        //giftCardModel = mock(GiftCardModel.class);

        CurrencyModel currencyModel = mock(CurrencyModel.class);
        CustomerModel customerModel = mock(CustomerModel.class);

        giftCardModel.setCode(GIFTCARD_CODE);
        giftCardModel.setActive(Boolean.TRUE);
        //giftCardModel.setCustomer(customerModel);
        giftCardModel.setAmount(100d);
        giftCardModel.setCurrency(currencyModel);
        giftCardModel.setEmail(Boolean.TRUE);
    }

    @Test
    public void testPerform(){

        Assert.assertNotNull(actionContext.getData());
        when(actionContext.getData()).thenReturn(giftCardModel);

    }

    @Test
    public void giftCardModelNotNull(){

        Assert.assertNotEquals(0,100d);

        GiftCardMovementModel movements = new GiftCardMovementModel();
        Assert.assertNotNull(movements);

        movements.setAmount(giftCardModel.getAmount());
        movements.setCommited(Boolean.TRUE);
        movements.setCurrency(giftCardModel.getCurrency());
        movements.setGiftCard(giftCardModel);
        movements.setTransactionId(giftCardModel.getCode());

        giftCardModel.setIssuer("admin");

        //Assert.assertEquals(100d,giftCardModel2.getAmount().doubleValue());
        Assert.assertEquals(true,movements.getCommited());

        modelService.save(movements);
        modelService.save(giftCardModel);
        modelService.refresh(giftCardModel);
        modelService.refresh(movements);
    }

    @Test
    public void testCanPerformWhenDataIsNull(){

        final boolean result = getAction().canPerform(actionContext);

        assertThat(result).isFalse();

    }

    @Test
    public void testCanPerformWhenDataIsNotNull() throws  ClassCastException, ComparisonFailure {
        when(actionContext.getData()).thenReturn(giftCardModel);

        final boolean result = getAction().canPerform(actionContext);
        assertThat(result).isTrue();
    }

    public GiftcardEmailandMovementAction getAction(){
        return action;
    }

    @Test
    public void testInitializeEvent(){

        Assert.assertNotNull(event);
        event.setSite(baseSiteService.getBaseSiteForUID("BL"));
        event.setBaseStore(baseStoreService.getBaseStoreForUid("BL"));
        event.setGiftcard(giftCardModel);
    }
}
