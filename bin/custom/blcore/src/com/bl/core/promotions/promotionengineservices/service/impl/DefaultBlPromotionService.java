package com.bl.core.promotions.promotionengineservices.service.impl;

import com.bl.core.promotions.promotionengineservices.service.BlPromotionService;
import de.hybris.platform.promotionengineservices.dao.PromotionDao;
import de.hybris.platform.promotionengineservices.model.PromotionSourceRuleModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedPromotionModel;
import de.hybris.platform.ruleengineservices.enums.RuleStatus;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.Collection;
import java.util.Date;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

/**
 * This class is addded to check if usedgearCategory promotion is active
 *
 * @author Ritika
 */
public class DefaultBlPromotionService implements BlPromotionService {

  private PromotionDao promotionDao;
  private BaseStoreService baseStoreService;


  @Override
  public boolean isUsedGearCategoryPromotionActive() {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    if(baseStoreModel != null && StringUtils.isNotBlank(baseStoreModel.getUsedGearPromotionCode())) {
      final RuleBasedPromotionModel promotionModel = (RuleBasedPromotionModel) getPromotionDao().findPromotionByCode(baseStoreModel.getUsedGearPromotionCode());
      if(promotionModel  != null && promotionModel.getPromotionGroup() != null && CollectionUtils.isNotEmpty(promotionModel.getPromotionGroup().getPromotionSourceRules())) {
        Collection<PromotionSourceRuleModel> promotionSourceRules = promotionModel.getPromotionGroup().getPromotionSourceRules();
        final Optional<PromotionSourceRuleModel> sourceRuleModel = promotionSourceRules.stream().filter(rule ->
            StringUtils.equalsIgnoreCase(rule.getCode(), baseStoreModel.getUsedGearPromotionCode())
                && RuleStatus.PUBLISHED.equals(rule.getStatus()) && validatePromotionDates(
                rule.getStartDate(), rule.getEndDate())).findAny();
            return  sourceRuleModel.isPresent();
      }
    }

    return false;
  }

  /**
   * Validate if current date is eligible to apply promotion
   * @param startDate
   * @param endDate
   * @return
   */
  private boolean validatePromotionDates(final Date startDate, final Date endDate) {
    final Date currentDate = new Date();
    return startDate != null && endDate != null && currentDate.getTime() >= startDate.getTime() &&
        currentDate.getTime() <= endDate.getTime();
  }

  public PromotionDao getPromotionDao() {
    return promotionDao;
  }

  public void setPromotionDao(PromotionDao promotionDao) {
    this.promotionDao = promotionDao;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }
}
