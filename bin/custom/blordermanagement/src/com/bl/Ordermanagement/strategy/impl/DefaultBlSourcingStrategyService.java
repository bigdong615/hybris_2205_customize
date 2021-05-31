package com.bl.Ordermanagement.strategy.impl;

import com.bl.Ordermanagement.strategy.BlSourcingStrategyService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.sourcing.strategy.SourcingStrategy;
import de.hybris.platform.warehousing.sourcing.strategy.SourcingStrategyMapper;
import de.hybris.platform.warehousing.sourcing.strategy.SourcingStrategyService;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Required;

public class DefaultBlSourcingStrategyService implements BlSourcingStrategyService, InitializingBean {
  private List<SourcingStrategy> defaultStrategies;

  public DefaultBlSourcingStrategyService() {
  }

  public List<SourcingStrategy> getStrategies(
      SourcingContext context, Collection<SourcingStrategyMapper> mappers) {
    List<SourcingStrategy> strategies = new ArrayList();
    Iterator var5 = mappers.iterator();

    while(var5.hasNext()) {
      SourcingStrategyMapper mapper = (SourcingStrategyMapper)var5.next();
      if (mapper.isMatch(context)) {
        strategies.add(mapper.getStrategy());
      }
    }

    return strategies;
  }

  public List<SourcingStrategy> getDefaultStrategies() {
    return this.defaultStrategies;
  }

  public void afterPropertiesSet() throws Exception {
    if (CollectionUtils.isEmpty(this.defaultStrategies)) {
      throw new IllegalArgumentException("Default strategies cannot be empty.");
    }
  }

  @Required
  public void setDefaultStrategies(List<SourcingStrategy> defaultStrategies) {
    this.defaultStrategies = defaultStrategies;
  }
}
