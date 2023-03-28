package com.bl.core.services.cart.impl;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceCartService;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceRemoveEntriesStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * @author Neeraj Singh
 */
@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlCartServiceTest {

  @InjectMocks
  private final DefaultCommerceCartService commerceCartService = new DefaultCommerceCartService();

  @InjectMocks
  @Spy
  private final DefaultCommerceRemoveEntriesStrategy commerceRemoveEntriesStrategy = new DefaultCommerceRemoveEntriesStrategy();

  @InjectMocks
  @Spy
  private final DefaultCommerceCartCalculationStrategy cartCalculationStrategy = new DefaultCommerceCartCalculationStrategy();

  @Mock
  private CartModel cartModel;

  @Mock
  private ModelService modelService;

  @Mock
  private CalculationService calculationService;

  private final Date rentalStartDate = new Date();

  private final Date rentalEndDate = new Date();

  @Before
  public void setup() {
	  // MockitoAnnotations.initMocks(this);
    commerceCartService.setCommerceCartCalculationStrategy(cartCalculationStrategy);
    commerceCartService.setCommerceRemoveEntriesStrategy(commerceRemoveEntriesStrategy);
  }

  @Test
  public void testClearCartEntries() {

    final AbstractOrderEntryModel entryModel = mock(AbstractOrderEntryModel.class);
    final List<AbstractOrderEntryModel> entries = Collections.singletonList(entryModel);

    given(cartModel.getEntries()).willReturn(entries);
    final CommerceCartParameter parameter = new CommerceCartParameter();
    parameter.setEnableHooks(true);
    parameter.setCart(cartModel);
    commerceCartService.removeAllEntries(parameter);
    Mockito.verify(modelService, Mockito.times(1)).removeAll(entries);
  }

  @Test
  public void testSetRentalDatesOnCart()
  {
	  cartModel.setRentalStartDate(rentalStartDate);
	  cartModel.setRentalEndDate(rentalEndDate);
	  modelService.save(cartModel);
      Mockito.verify(modelService, Mockito.times(1)).save(cartModel);
  }
}
