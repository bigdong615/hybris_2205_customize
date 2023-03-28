package com.bl.core.strategies;

import de.hybris.bootstrap.annotations.UnitTest;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.strategies.impl.BlDefaultProductDynamicPriceStrategy;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlDefaultProductDynamicPriceStrategyTest
{

  @InjectMocks
  BlDefaultProductDynamicPriceStrategy blDefaultProductDynamicPriceStrategy = new BlDefaultProductDynamicPriceStrategy();

  @Test
  public void testGetPriceRatios()
  {
	  // MockitoAnnotations.initMocks(this);
    final Map<Integer, BigDecimal> priceList = new HashMap<>();
    priceList.put(3,new BigDecimal(10));
    priceList.put(5,new BigDecimal(15));
    priceList.put(7,new BigDecimal(17));
    priceList.put(14,new BigDecimal(24));
    priceList.put(21,new BigDecimal(41));
    priceList.put(28,new BigDecimal(48));
    priceList.put(35,new BigDecimal(55));

    final long rentalDays = 5;

    blDefaultProductDynamicPriceStrategy.getDynamicPrice(priceList,rentalDays);
    final Map<Integer, BigDecimal> priceList2 = new HashMap<>();
    Assert.assertNull(blDefaultProductDynamicPriceStrategy.getDynamicPrice(priceList2,rentalDays));
  }

}
