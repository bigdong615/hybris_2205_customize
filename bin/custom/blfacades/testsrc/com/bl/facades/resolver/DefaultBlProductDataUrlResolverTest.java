package com.bl.facades.resolver;

import static de.hybris.platform.testframework.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.data.ProductData;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.facades.constants.BlFacadesConstants;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlProductDataUrlResolverTest {

  @InjectMocks
  private final DefaultBlProductDataUrlResolver resolver = Mockito
      .spy(new DefaultBlProductDataUrlResolver());
  public static String PRODUCT_CODE = "testcode";
  public static String RENTAL_PAGE_URL = (new StringBuffer()
      .append(BlFacadesConstants.DEFAULT_REDIRECT_URL)
      .append(BlFacadesConstants.RENTAL_PAGE_IDENTIFIER)
      .append(BlFacadesConstants.PRODUCT_URL).append(PRODUCT_CODE)).toString();
  public static String USED_PAGE_URL = (new StringBuffer()
      .append(BlFacadesConstants.DEFAULT_REDIRECT_URL)
      .append(BlFacadesConstants.USED_PAGE_IDENTIFIER)
      .append(BlFacadesConstants.PRODUCT_URL).append(PRODUCT_CODE)).toString();
  ;
  @Mock
  ProductData productData;

  @Before
  public void prepare() {
	  //MockitoAnnotations.initMocks(this);
    when(productData.getCode()).thenReturn(PRODUCT_CODE);
  }

  @Test
  public void shouldGetRentalUrl() {
    when(productData.getProductPageType()).thenReturn(BlFacadesConstants.RENTAL_PAGE_IDENTIFIER);
    final String resolveUrl = resolver.resolveInternal(productData);
    assertNotNull(resolveUrl);
    assertEquals(resolveUrl, RENTAL_PAGE_URL);
  }

  @Test
  public void shouldGetUsedGearUrl() {
    when(productData.getProductPageType()).thenReturn(BlFacadesConstants.USED_PAGE_IDENTIFIER);
    final String resolveUrl = resolver.resolveInternal(productData);
    assertNotNull(resolveUrl);
    assertEquals(resolveUrl, USED_PAGE_URL);
  }

}
