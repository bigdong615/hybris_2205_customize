package com.bl.facades.cart;

import static de.hybris.platform.testframework.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.anyObject;
import static org.mockito.BDDMockito.given;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.order.data.AddToCartParams;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commerceservices.i18n.CommerceCommonI18NService;
import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.model.BlProductModel;
import com.bl.facades.cart.impl.DefaultBlCartFacade;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlCartFacadeTest {

  @InjectMocks
  private final DefaultBlCartFacade cartFacade = Mockito.spy(new DefaultBlCartFacade());

  public static final String PRODUCT_CODE1 = "1001";
  public static final String PRODUCT_CODE2 = "1002";
  public static final String PRODUCT_CODE3 = "1003";
  public static final String PRODUCT_NAME1 = "canon camera";
  public static final String PRODUCT_NAME2 = "nikon camera";
  public static final String PRODUCT_NAME3 = "sony camera";
  public static final Integer ENTER_NUMBER1 = 0;
  public static final Integer ENTER_NUMBER2 = 1;
  public static final Integer ENTER_NUMBER3 = 2;
  private static final String CART = "cart";

  public CartModel cartModel;
  @Mock
  public ModelService modelService;
  @Mock
  public CommerceCommonI18NService commerceCommonI18NService;
  @Mock
  public I18NService i18NService;
  @Mock
  public Converter<AddToCartParams, CommerceCartParameter> commerceCartParameterConverter;
  @Mock
  public CommerceCartService commerceCartService;
  @Mock
  public Converter<CommerceCartModification, CartModificationData> cartModificationConverter;
  @Mock
  public CartService cartService;

  BlProductModel blProductModel1;
  BlProductModel blProductModel2;
  BlProductModel blProductModel3;
  AbstractOrderEntryModel orderEntryModel1;
  AbstractOrderEntryModel orderEntryModel2;
  AbstractOrderEntryModel orderEntryModel3;
  List entryList;
  CommerceCartParameter parameter;
  CommerceCartModification modification;

  @Before
  public void prepare(){
	  // MockitoAnnotations.initMocks(this);

    given(i18NService.getCurrentLocale()).willReturn(Locale.ENGLISH);
    parameter = new CommerceCartParameter();
    modification = new CommerceCartModification();
    blProductModel1=populateProductData(PRODUCT_CODE1,PRODUCT_NAME1,Boolean.TRUE);
    blProductModel2=populateProductData(PRODUCT_CODE2,PRODUCT_NAME2,Boolean.FALSE);
    blProductModel3=populateProductData(PRODUCT_CODE3,PRODUCT_NAME3,Boolean.FALSE);
    orderEntryModel1 = populateOrderEntryData(ENTER_NUMBER1,blProductModel1);
    orderEntryModel2 = populateOrderEntryData(ENTER_NUMBER2,blProductModel2);
    orderEntryModel3 = populateOrderEntryData(ENTER_NUMBER3,blProductModel3);
    entryList= new ArrayList<AbstractOrderEntryModel>();
    entryList.add(orderEntryModel1);
    entryList.add(orderEntryModel2);
    entryList.add(orderEntryModel3);
    cartModel = new CartModel();
    cartModel.setCode(CART);
    cartModel.setGuid(CART);
    cartModel.setEntries(entryList);
  }

  @Test
  public void shouldRemovedDiscontinuedProductFromCurrentCart() throws CommerceCartModificationException {
    given(Boolean.valueOf(cartService.hasSessionCart())).willReturn(Boolean.TRUE);
    given(cartService.getSessionCart()).willReturn(cartModel);
    given(cartFacade.getCommerceCartParameterConverter().convert(anyObject())).willReturn(parameter);
    given(commerceCartService.updateQuantityForCartEntry(parameter)).willReturn(modification);
    final String removedProductName = cartFacade.removeDiscontinueProductFromCart(cartModel,Boolean.TRUE);
    assertNotNull(removedProductName);
    assertEquals(removedProductName,PRODUCT_NAME1);
  }

  @Test
  public void shouldRemovedDiscontinuedProductFromGivenCart() throws CommerceCartModificationException {
    given(Boolean.valueOf(cartService.hasSessionCart())).willReturn(Boolean.TRUE);
    given(cartService.getSessionCart()).willReturn(cartModel);
    given(cartFacade.getCommerceCartParameterConverter().convert(anyObject())).willReturn(parameter);
    given(commerceCartService.updateQuantityForCartEntry(parameter)).willReturn(modification);
    final String removedProductName = cartFacade.removeDiscontinueProductFromCart(cartModel,Boolean.FALSE);
    assertNotNull(removedProductName);
    assertEquals(removedProductName,PRODUCT_NAME1);
  }

  private BlProductModel populateProductData(final String productCode,final String productName,final Boolean isDiscontinue){
    final BlProductModel blProductModel = new BlProductModel();
    blProductModel.setCode(PRODUCT_CODE1);
    blProductModel.setName(PRODUCT_NAME1,i18NService.getCurrentLocale());
    blProductModel.setDiscontinued(isDiscontinue);
    return blProductModel;
  }

  private AbstractOrderEntryModel populateOrderEntryData(final Integer entryNumber ,final BlProductModel blProductModel ) {
    final AbstractOrderEntryModel orderEntryModel = new AbstractOrderEntryModel();
    orderEntryModel.setEntryNumber(entryNumber);
    orderEntryModel.setProduct(blProductModel);
    return orderEntryModel;
  }
}
