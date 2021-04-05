package com.bl.core.services;

import static org.mockito.BDDMockito.given;

import com.bl.core.dao.pricingratio.BlPricingRatioDao;
import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.core.services.pricingratio.impl.DefaultBlPricingRatioService;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.enumeration.EnumerationService;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@UnitTest
public class DefaultBlPricingRatioServiceTest {

  private DefaultBlPricingRatioService blPricingRatioService;
  @Mock
  private EnumerationService enumerationService;

  @Mock
  private BlPricingRatioDao blPricingRatioDao;

  private BlStandardPricingRatioModel standardPricingRatioModel1;
  private BlStandardPricingRatioModel standardPricingRatioModel2;
  private BlConstrainedPricingRatioModel constrainedPricingRatioModel1;
  private BlConstrainedPricingRatioModel constrainedPricingRatioModel2;
  private DurationEnum tenDay;
  private DurationEnum fourteenDay;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    blPricingRatioService = new DefaultBlPricingRatioService();
    blPricingRatioService.setBlPricingRatioDao(blPricingRatioDao);
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
    given(blPricingRatioDao.getStandardPricingRatioByDuration(tenDay)).willReturn(standardPricingRatioModel1);
    given(blPricingRatioDao.getStandardPricingRatioByDuration(fourteenDay)).willReturn(standardPricingRatioModel2);
    given(blPricingRatioDao.getConstrainedPricingRatioByDuration(tenDay)).willReturn(constrainedPricingRatioModel1);
    given(blPricingRatioDao.getConstrainedPricingRatioByDuration(fourteenDay)).willReturn(constrainedPricingRatioModel2);
  }

  @Test
  public void shouldGetStandardPricingRatioByDuration()
  {
    BlStandardPricingRatioModel blStandardPricingRatio = blPricingRatioService.getStandardPricingRatioByDuration(fourteenDay);
    Assert.assertEquals(standardPricingRatioModel2.getPricingRatio(),blStandardPricingRatio.getPricingRatio());
  }

  @Test
  public void shouldGetConstrainedPricingRatioByDuration()
  {
    BlConstrainedPricingRatioModel blConstrainedPricingRatio = blPricingRatioService.getConstrainedPricingRatioByDuration(fourteenDay);
    Assert.assertEquals(constrainedPricingRatioModel2.getPricingRatio(),blConstrainedPricingRatio.getPricingRatio());
  }



}
