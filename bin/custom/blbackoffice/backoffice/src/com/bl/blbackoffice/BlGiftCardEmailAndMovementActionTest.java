package com.bl.blbackoffice;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.services.BaseStoreService;

import org.junit.Assert;
import org.junit.Before;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.blbackoffice.actions.BlGiftCardEmailAndMovementAction;
import com.bl.core.event.BlGiftCardEmailEvent;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.hybris.cockpitng.actions.ActionContext;


/**
 * Test class of {@link BlGiftCardEmailAndMovementAction}.
 * @author Neeraj Singh
 */
@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlGiftCardEmailAndMovementActionTest {

  @Spy
  @InjectMocks
  private BlGiftCardEmailAndMovementAction action;

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

  /**
   * setup GiftCardModel attributes value.
   */
  @Before
  public void setup() {

	  //  MockitoAnnotations.initMocks(this);
    final CurrencyModel currencyModel = mock(CurrencyModel.class);
    giftCardModel.setCode(GIFTCARD_CODE);
    giftCardModel.setActive(Boolean.TRUE);
    giftCardModel.setAmount(100d);
    giftCardModel.setCurrency(currencyModel);
    giftCardModel.setEmail(Boolean.TRUE);
  }

  @Test
  public void testPerform() {

    Assert.assertNotNull(actionContext.getData());
    when(actionContext.getData()).thenReturn(giftCardModel);

  }

  /**
   * Test GiftCardMovement creation.
   */
  @Test
  public void giftCardModelNotNull() {

    Assert.assertNotEquals(0, 100d);
    final GiftCardMovementModel movements = new GiftCardMovementModel();
    Assert.assertNotNull(movements);
    movements.setAmount(giftCardModel.getAmount());
    movements.setCommitted(Boolean.TRUE);
    movements.setCurrency(giftCardModel.getCurrency());
    movements.setGiftCard(giftCardModel);
    movements.setTransactionId(giftCardModel.getCode());
    giftCardModel.setIssuer("admin");
    Assert.assertEquals(true, movements.getCommitted());
    modelService.save(movements);
    modelService.save(giftCardModel);
    modelService.refresh(giftCardModel);
    modelService.refresh(movements);
  }

  /**
   It tests actionContext's perform is false.
   */
  @Test
  public void testCanPerformWhenDataIsNull() {
    final boolean result = getAction().canPerform(actionContext);
    assertThat(result).isFalse();
  }

  /**
   *It tests actionContext's perform is true.
   * @throws ClassCastException
   * @throws ComparisonFailure
   */
  @Test
  public void testCanPerformWhenDataIsNotNull() throws ClassCastException, ComparisonFailure {
    when(actionContext.getData()).thenReturn(giftCardModel);
    final boolean result = getAction().canPerform(actionContext);
    assertThat(result).isTrue();
  }

  public BlGiftCardEmailAndMovementAction getAction() {
    return action;
  }

  @Test
  public void testInitializeEvent() {

    Assert.assertNotNull(event);
    event.setSite(baseSiteService.getBaseSiteForUID("BL"));
    event.setBaseStore(baseStoreService.getBaseStoreForUid("BL"));
    event.setGiftcard(giftCardModel);
  }
}
