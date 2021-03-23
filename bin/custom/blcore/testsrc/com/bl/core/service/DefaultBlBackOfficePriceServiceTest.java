package com.bl.core.service;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.bl.core.enums.DurationEnum;
import com.bl.core.service.impl.DefaultBlBackOfficePriceService;
import com.bl.core.strategies.BlProductDynamicPriceStrategy;
import com.bl.core.util.BlPriceRatioUtil;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.europe1.model.PriceRowModel;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.runners.MockitoJUnitRunner;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlBackOfficePriceServiceTest
{
  @InjectMocks
  private DefaultBlBackOfficePriceService defaultBlBackOfficePriceService = new DefaultBlBackOfficePriceService();
  @Mock
  private BlProductDynamicPriceStrategy blProductDynamicPriceStrategy;
  @Mock
  private BlPriceRatioUtil blProductPriceRatioUtil;


  @Before
  public void setup()
  {
    defaultBlBackOfficePriceService.setBlProductPriceRatioUtil(blProductPriceRatioUtil);
    defaultBlBackOfficePriceService.setBlProductDynamicPriceStrategy(blProductDynamicPriceStrategy);
  }

  @Test
  public void testGetProductPrice() throws ParseException
  {
     MockitoAnnotations.initMocks(this);
     final ProductModel productModel = mock(ProductModel.class);
     final PriceRowModel priceRowModel = mock(PriceRowModel.class);

     Date startDate = new Date(2021,03,01);
     Date returnDate = new Date(2021,03,05);

    when(defaultBlBackOfficePriceService.getBlProductPriceRatioUtil().getPriceRatios(productModel)).thenReturn(getPriceList());
    defaultBlBackOfficePriceService.getProductPrice(productModel,startDate,returnDate);

  }

  @Test
  public void testGetProductPriceWhenProductNull() throws ParseException
  {
    MockitoAnnotations.initMocks(this);
    final ProductModel productModel =null ;
    Date startDate = new Date(2021,03,01);
    Date returnDate = new Date(2021,03,05);
    defaultBlBackOfficePriceService.getProductPrice(productModel,startDate,returnDate);
  }


  private Map<Integer, BigDecimal> getPriceList()
  {
    Map<Integer, BigDecimal> priceList = new HashMap<>();
    priceList.put(3,new BigDecimal(10));
    priceList.put(5,new BigDecimal(15));
    priceList.put(7,new BigDecimal(17));
    priceList.put(14,new BigDecimal(24));
    priceList.put(21,new BigDecimal(41));
    priceList.put(28,new BigDecimal(48));
    priceList.put(35,new BigDecimal(55));
    return priceList;
  }
}
