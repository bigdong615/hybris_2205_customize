package com.bl.core.services;

import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.europe1.model.PriceRowModel;
import de.hybris.platform.product.UnitService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.internal.dao.DefaultGenericDao;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.enums.PricingTierEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlPricingLogicModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.services.calculation.impl.DefaultBlPricingService;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlPricingServiceTest {

  private DefaultBlPricingService blPricingService;
  @Mock
  private DefaultGenericDao blPricingDao;
  @Mock
  private DefaultGenericDao priceRowDao;
  @Mock
  private ModelService modelService;
  @Mock
  private CommonI18NService commonI18NService;
  @Mock
  private EnumerationService enumerationService;
  @Mock
  private UnitService unitService;

  private BlProductModel blProductModel;
  private PriceRowModel priceRowModel;
  private BlPricingLogicModel pricingLogicModel1;
  private BlPricingLogicModel pricingLogicModel2;
  private BlPricingLogicModel pricingLogicModel3;


  @Before
  public void setUp()
  {
	  // MockitoAnnotations.initMocks(this);
    blPricingDao = new DefaultGenericDao(BlPricingLogicModel._TYPECODE);
    blPricingService= new DefaultBlPricingService();
    blPricingService.setEnumerationService(enumerationService);
    blPricingService.setModelService(modelService);
    blPricingService.setUnitService(unitService);
    blPricingService.setCommonI18NService(commonI18NService);
    blProductModel = new BlProductModel();
    blProductModel.setProductType(ProductTypeEnum.CAMERAS);
    priceRowModel = new PriceRowModel();
    priceRowModel.setPrice(100.00);
    priceRowModel.setProduct(blProductModel);
    final List<BlPricingLogicModel> blPricingList = new ArrayList<>();
    pricingLogicModel1 = mock(BlPricingLogicModel.class);
    pricingLogicModel2 = mock(BlPricingLogicModel.class);
    pricingLogicModel3 = mock(BlPricingLogicModel.class);
    given(pricingLogicModel1.getTier()).willReturn(PricingTierEnum.ONE);
    given(pricingLogicModel2.getTier()).willReturn(PricingTierEnum.TWO);
    given(pricingLogicModel3.getTier()).willReturn(PricingTierEnum.THREE);
    given(pricingLogicModel1.getLessThan()).willReturn(Double.valueOf(600));
    given(pricingLogicModel2.getGreaterThan()).willReturn(Double.valueOf(600));
    given(pricingLogicModel2.getLessThan()).willReturn(Double.valueOf(1200));
    given(pricingLogicModel3.getGreaterThan()).willReturn(Double.valueOf(1200));
    given(pricingLogicModel1.getPctOfRetail()).willReturn(10);
    given(pricingLogicModel2.getPctOfRetail()).willReturn(7);
    given(pricingLogicModel3.getPctOfRetail()).willReturn(5);
    blPricingList.add(pricingLogicModel1);
    blPricingList.add(pricingLogicModel2);
    blPricingList.add(pricingLogicModel3);
    given(blPricingDao.find(Collections.singletonMap(BlPricingLogicModel.PRODUCTTYPE,blProductModel.getProductType()))).willReturn(blPricingList);
    given(modelService.create(PriceRowModel.class)).willReturn(new PriceRowModel());
    given(priceRowDao.find(anyMap())).willReturn((List)priceRowModel);
  }

  @Test
  public void testGetSevenDayPriceForCameraProductFromTier3()
  {
    blProductModel.setRetailPrice(2000.00);
    priceRowModel.setPrice(100.00);

    final PriceRowModel sevenDayPrice = blPricingService.createOrUpdateSevenDayPrice(blProductModel,blProductModel.getRetailPrice(),true);
    Assert.assertEquals(priceRowModel.getPrice(),sevenDayPrice.getPrice());
  }

  @Test
  public void testGetSevenDayPriceForCameraProductFromTier2()
  {
    blProductModel.setRetailPrice(900.00);
    priceRowModel.setPrice(63.00);

    final PriceRowModel sevenDayPrice = blPricingService.createOrUpdateSevenDayPrice(blProductModel,blProductModel.getRetailPrice(),true);
    Assert.assertEquals(priceRowModel.getPrice(),sevenDayPrice.getPrice());
  }
  @Test
  public void testGetSevenDayPriceForCameraProductFromTier1()
  {
    blProductModel.setRetailPrice(400.00);
    priceRowModel.setPrice(40.00);

    final PriceRowModel sevenDayPrice = blPricingService.createOrUpdateSevenDayPrice(blProductModel,blProductModel.getRetailPrice(),true);
    Assert.assertEquals(priceRowModel.getPrice(),sevenDayPrice.getPrice());
  }

  @Test
  public void testGetUpdatedSevenDayPriceForCameraProductFromTier1()
  {
    blProductModel.setRetailPrice(1000.00);
    final PriceRowModel sevenDayPrice = blPricingService.createOrUpdateSevenDayPrice(blProductModel,blProductModel.getRetailPrice(),false);
    Assert.assertEquals(priceRowModel.getPrice(),sevenDayPrice.getPrice());
  }

}


