package com.bl.core.util;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.europe1.model.PriceRowModel;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.dao.BlStandardPricingRatioDao;
import com.bl.core.model.BlStandardPricingRatioModel;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlPriceRatioUtilTest
{

  @InjectMocks
  private final BlPriceRatioUtil blPriceRatioUtil =  Mockito.spy(new BlPriceRatioUtil());
  @Mock
  private BlStandardPricingRatioDao blStandardPricingRatioDao;
  @Mock
  private EnumerationService enumerationService;


  @Before
  public void setUp()
  {
	  //MockitoAnnotations.initMocks(this);
    blPriceRatioUtil.setBlStandardPricingRatioDao(blStandardPricingRatioDao);
    blPriceRatioUtil.setEnumerationService(enumerationService);
  }

  @Test
  public void testGetPriceRatios()
  {

    final ProductModel productModel = mock(ProductModel.class);
    final PriceRowModel priceRowModel = mock(PriceRowModel.class);
    priceRowModel.setPrice(30.00);
    final List<PriceRowModel> priceRowModelList =new ArrayList<>();
    priceRowModelList.add(priceRowModel);
    productModel.setEurope1Prices(priceRowModelList);
    productModel.setCode("1320808");
    when(productModel.getEurope1Prices()).thenReturn(priceRowModelList);

    final List<BlStandardPricingRatioModel> blStandardPricingRatioModels = new ArrayList<>();

    when(blStandardPricingRatioDao.getStandardPricingRatio()).thenReturn(blStandardPricingRatioModels);

    blPriceRatioUtil.getPriceRatios(productModel);
  }

}
