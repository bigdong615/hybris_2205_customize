package com.bl.core.promotions.promotionengineservices.service;

import static org.mockito.BDDMockito.given;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.promotionengineservices.dao.PromotionDao;
import de.hybris.platform.promotionengineservices.model.PromotionSourceRuleModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedPromotionModel;
import de.hybris.platform.promotions.model.PromotionGroupModel;
import de.hybris.platform.ruleengineservices.enums.RuleStatus;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.time.DateUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.promotions.promotionengineservices.service.impl.DefaultBlPromotionService;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlPromotionServiceTest{

  private static final String PROMOTION_CODE = "TEST_UG_CC_PROMOTION";
  public static final String COUPON_MESSAGE = "Using the coupon code UGTEST2021";

  @InjectMocks
  private final DefaultBlPromotionService blPromotionService = Mockito.spy(new DefaultBlPromotionService());

  @Mock
  private PromotionDao promotionDao;
  @Mock
  private BaseStoreService baseStoreService;
  @Mock
  private BaseStoreModel baseStoreModel;
  @Mock
  private RuleBasedPromotionModel promotionModel;
  @Mock
  private PromotionGroupModel promotionGroupModel;
  @Mock
  private PromotionSourceRuleModel promotionSourceRule;




  @Before
    public void prepare(){
		 // MockitoAnnotations.initMocks(this);
      blPromotionService.setPromotionDao(promotionDao);
      blPromotionService.setBaseStoreService(baseStoreService);
      final List<PromotionSourceRuleModel> promotionSourceRules = new ArrayList<>();
      promotionSourceRules.add(promotionSourceRule);
      given(baseStoreService.getCurrentBaseStore()).willReturn(baseStoreModel);
      given(promotionDao.findPromotionByCode(PROMOTION_CODE)).willReturn(promotionModel);
      given(promotionModel.getPromotionGroup()).willReturn(promotionGroupModel);
      given(promotionModel.getPromotionGroup().getPromotionSourceRules()).willReturn(promotionSourceRules);
      given(promotionSourceRule.getCode()).willReturn(PROMOTION_CODE);
      given(promotionSourceRule.getStatus()).willReturn(RuleStatus.PUBLISHED);
      given(promotionSourceRule.getStartDate()).willReturn(DateUtils.addDays(new Date(), -10));
      given(promotionSourceRule.getEndDate()).willReturn(DateUtils.addDays(new Date(), 30));
    }

@Test
  public void testIsUsedGearCategoryPromotionActive() {
    given(baseStoreService.getCurrentBaseStore()).willReturn(baseStoreModel);
    given(baseStoreModel.getUsedGearPromotionCode()).willReturn(PROMOTION_CODE);
    given(baseStoreModel.getUsedGearPromotionDiscount()).willReturn(Integer.valueOf(10));
    given(baseStoreModel.getUsedGearPromotionMessage()).willReturn(COUPON_MESSAGE);
    final boolean isUsedGearPromoActive = blPromotionService.isUsedGearCategoryPromotionActive();
    Assert.assertTrue(isUsedGearPromoActive);
  }

  @Test
  public void testWhenPromotionCodeIsNotPresent() {
    given(baseStoreService.getCurrentBaseStore()).willReturn(baseStoreModel);
    given(baseStoreModel.getUsedGearPromotionCode()).willReturn("TEST_PROMO");
    given(baseStoreModel.getUsedGearPromotionDiscount()).willReturn(Integer.valueOf(10));
    given(baseStoreModel.getUsedGearPromotionMessage()).willReturn(COUPON_MESSAGE);
    final boolean isUsedGearPromoActive = blPromotionService.isUsedGearCategoryPromotionActive();
    Assert.assertFalse(isUsedGearPromoActive);
  }

  @Test
  public void testWhenPromotionCodeAndDiscountIsEmpty(){
    given(baseStoreService.getCurrentBaseStore()).willReturn(baseStoreModel);
    given(baseStoreModel.getUsedGearPromotionCode()).willReturn(null);
    given(baseStoreModel.getUsedGearPromotionDiscount()).willReturn(null);
    given(baseStoreModel.getUsedGearPromotionMessage()).willReturn(COUPON_MESSAGE);
    final boolean isUsedGearPromoActive = blPromotionService.isUsedGearCategoryPromotionActive();
    Assert.assertFalse(isUsedGearPromoActive);
  }

}
