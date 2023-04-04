package com.bl.facades.populators;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderAdjustTotalActionModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderEntryAdjustActionModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedPromotionModel;
import de.hybris.platform.promotions.model.AbstractPromotionActionModel;
import de.hybris.platform.promotions.model.PromotionResultModel;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlOrderAppliedVouchersPopulatorTest {

  private BlOrderAppliedVouchersPopulator blOrderAppliedVouchersPopulator;

  @Before
  public void startUp() {
	  // MockitoAnnotations.initMocks(this);
    blOrderAppliedVouchersPopulator = new BlOrderAppliedVouchersPopulator();
  }

  // When promotionResultModel is  match with RuleBasedOrderEntryAdjustActionModel
  @Test
  public void testPopulateWhenInstanceOfRuleBasedOrderEntryAdjustActionModel() {
    final AbstractOrderModel source = new AbstractOrderModel();
    final AbstractOrderData target = new AbstractOrderData();
    final Collection<String> stringCollection = new ArrayList<>();
    stringCollection.add("TestPromo");
    final PromotionResultModel promotionResultModel1 = new PromotionResultModel();
    final RuleBasedOrderEntryAdjustActionModel abstractPromotionActionModel = new RuleBasedOrderEntryAdjustActionModel();
    abstractPromotionActionModel.setGuid("Test-Guid");
    abstractPromotionActionModel.setUsedCouponCodes(stringCollection);
    abstractPromotionActionModel.setAmount(BigDecimal.valueOf(33.33));
    abstractPromotionActionModel.setPromotionResult(promotionResultModel1);
    final Collection<AbstractPromotionActionModel> abstractPromotionActionModelCollection = new ArrayList<>();
    abstractPromotionActionModelCollection.add(abstractPromotionActionModel);
    final PromotionResultModel promotionResultModel =new PromotionResultModel();
    promotionResultModel.setActions(abstractPromotionActionModelCollection);
    final Set<PromotionResultModel> promotionResultModelSet = new HashSet<>();
    promotionResultModelSet.add(promotionResultModel);
    source.setAllPromotionResults(promotionResultModelSet);
    blOrderAppliedVouchersPopulator.populate(source , target);
    Assert.assertTrue(target.getAppliedVouchers().contains("TestPromo"));
    Assert.assertTrue(target.getPromotionAmountMap().get("TestPromo").compareTo(BigDecimal.valueOf(33.33)) == 0);
  }

  // When promotionResultModel is  match with RuleBasedOrderAdjustTotalActionModel
  @Test
  public void testPopulateWhenInstanceOfRuleBasedOrderAdjustTotalActionModel() {
    final AbstractOrderModel source = new AbstractOrderModel();
    final AbstractOrderData target = new AbstractOrderData();
    final Collection<String> stringCollection = new ArrayList<>();
    stringCollection.add("AutoPromo");
    final PromotionResultModel promotionResultModel1 = new PromotionResultModel();
    final RuleBasedOrderAdjustTotalActionModel abstractPromotionActionModel = new RuleBasedOrderAdjustTotalActionModel();
    abstractPromotionActionModel.setGuid("Auto-Promo-Test-Guid");
    abstractPromotionActionModel.setUsedCouponCodes(stringCollection);
    abstractPromotionActionModel.setAmount(BigDecimal.valueOf(17.00));
    abstractPromotionActionModel.setPromotionResult(promotionResultModel1);
    final Collection<AbstractPromotionActionModel> abstractPromotionActionModelCollection = new ArrayList<>();
    abstractPromotionActionModelCollection.add(abstractPromotionActionModel);
    final PromotionResultModel promotionResultModel =new PromotionResultModel();
    promotionResultModel.setActions(abstractPromotionActionModelCollection);
    final Set<PromotionResultModel> promotionResultModelSet = new HashSet<>();
    promotionResultModelSet.add(promotionResultModel);
    source.setAllPromotionResults(promotionResultModelSet);
    blOrderAppliedVouchersPopulator.populate(source , target);
    Assert.assertTrue(target.getAppliedVouchers().contains("AutoPromo"));
    Assert.assertTrue(target.getPromotionAmountMap().get("AutoPromo").compareTo(BigDecimal.valueOf(17.00)) == 0);
  }


  // When promotionResultModel is either match with RuleBasedOrderEntryAdjustActionModel or RuleBasedOrderAdjustTotalActionModel
  @Test
  public void testPopulateWhenSourcegetAllPromotionResultsIsEmpty() {
    final AbstractOrderModel source = new AbstractOrderModel();
    final AbstractOrderData target = new AbstractOrderData();
   source.setAllPromotionResults(Collections.emptySet());
    blOrderAppliedVouchersPopulator.populate(source ,target);
    Assert.assertTrue(CollectionUtils.isEmpty(target.getAppliedVouchers()));
    Assert.assertTrue(MapUtils.isEmpty(target.getPromotionAmountMap()));
  }



  // When promotionResultModel is either match with RuleBasedOrderEntryAdjustActionModel or RuleBasedOrderAdjustTotalActionModel
  @Test
  public void testPopulate() {
    final AbstractOrderModel source = new AbstractOrderModel();
     final Set<PromotionResultModel> promotionResults = new HashSet<>();
     final PromotionResultModel promotionResultModel = new PromotionResultModel();
     final RuleBasedPromotionModel ruleBasedPromotionModel = new RuleBasedPromotionModel();
    promotionResultModel.setPromotion(ruleBasedPromotionModel);
    final AbstractPromotionActionModel abstractPromotionActionModel = new AbstractPromotionActionModel();
    final Collection<AbstractPromotionActionModel> value = new ArrayList<>();
    value.add(abstractPromotionActionModel);
    promotionResultModel.setActions(value);
    promotionResultModel.setAllPromotionActions(Collections.emptySet());
    promotionResults.add(promotionResultModel);
    source.setAllPromotionResults(promotionResults);
    final AbstractOrderData target = new AbstractOrderData();
    blOrderAppliedVouchersPopulator.populate(source , target);
    Assert.assertTrue(CollectionUtils.isEmpty(target.getAppliedVouchers()));
    Assert.assertTrue(MapUtils.isEmpty(target.getPromotionAmountMap()));
  }
}
