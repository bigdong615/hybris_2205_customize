package com.bl.core.services;

import static org.mockito.BDDMockito.given;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.servicelayer.internal.dao.DefaultGenericDao;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.core.services.pricingratio.impl.DefaultBlPricingRatioService;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlPricingRatioServiceTest {

  private DefaultBlPricingRatioService blPricingRatioService;
  @Mock
  private EnumerationService enumerationService;

  @Mock
  private DefaultGenericDao blConstrainedRatioDao;

  @Mock
  private DefaultGenericDao blStandardRatioDao;

  private BlStandardPricingRatioModel standardPricingRatioModel1;
  private BlStandardPricingRatioModel standardPricingRatioModel2;
  private BlConstrainedPricingRatioModel constrainedPricingRatioModel1;
  private BlConstrainedPricingRatioModel constrainedPricingRatioModel2;
  private DurationEnum tenDay;
  private DurationEnum fourteenDay;
  private Map<String,Object> paramMap;

  @Before
  public void setUp() {
	  // MockitoAnnotations.initMocks(this);
    blPricingRatioService = new DefaultBlPricingRatioService();
    blConstrainedRatioDao = new DefaultGenericDao(BlConstrainedPricingRatioModel._TYPECODE);
    blStandardRatioDao = new DefaultGenericDao(BlStandardPricingRatioModel._TYPECODE);
    paramMap = new HashMap<>();
    standardPricingRatioModel1 = new BlStandardPricingRatioModel();
    standardPricingRatioModel2 = new BlStandardPricingRatioModel();
    constrainedPricingRatioModel1 = new BlConstrainedPricingRatioModel();
    constrainedPricingRatioModel2 = new BlConstrainedPricingRatioModel();
    standardPricingRatioModel1.setPricingRatio(1.36);
    standardPricingRatioModel2.setPricingRatio(1.76);
    constrainedPricingRatioModel1.setPricingRatio(1.36);
    constrainedPricingRatioModel2.setPricingRatio(1.85);
    tenDay = enumerationService.getEnumerationValue(DurationEnum.class, "10");
    fourteenDay = enumerationService.getEnumerationValue(DurationEnum.class, "14");
    paramMap.put("duration",tenDay);
    paramMap.put("duration",fourteenDay);
    given(blStandardRatioDao.find(Collections.singletonMap(BlStandardPricingRatioModel.DURATION,tenDay))).willReturn((List)standardPricingRatioModel1);
    given(blStandardRatioDao.find(Collections.singletonMap(BlStandardPricingRatioModel.DURATION,fourteenDay))).willReturn((List)standardPricingRatioModel1);
    given(blConstrainedRatioDao.find(Collections.singletonMap(BlConstrainedPricingRatioModel.DURATION,tenDay))).willReturn((List)constrainedPricingRatioModel1);
    given(blConstrainedRatioDao.find(Collections.singletonMap(BlConstrainedPricingRatioModel.DURATION,fourteenDay))).willReturn((List)constrainedPricingRatioModel2);
  }

  @Test
  public void shouldGetStandardPricingRatioByDuration()
  {
    final BlStandardPricingRatioModel blStandardPricingRatio = blPricingRatioService.getStandardPricingRatioByDuration(fourteenDay);
    Assert.assertEquals(standardPricingRatioModel2.getPricingRatio(),blStandardPricingRatio.getPricingRatio());
  }

  @Test
  public void shouldGetConstrainedPricingRatioByDuration()
  {
    final BlConstrainedPricingRatioModel blConstrainedPricingRatio = blPricingRatioService.getConstrainedPricingRatioByDuration(fourteenDay);
    Assert.assertEquals(constrainedPricingRatioModel2.getPricingRatio(),blConstrainedPricingRatio.getPricingRatio());
  }



}
