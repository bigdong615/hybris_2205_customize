package com.bl.Ordermanagement.strategy.impl;

import com.bl.Ordermanagement.strategy.BlSourcingStrategyService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.sourcing.strategy.SourcingStrategy;
import de.hybris.platform.warehousing.sourcing.strategy.SourcingStrategyMapper;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.InitializingBean;

/**
 * It is used to get strategies.
 *
 * @author Sunil
 */
public class DefaultBlSourcingStrategyService implements BlSourcingStrategyService, InitializingBean {

  private List<SourcingStrategy> defaultStrategies;

  public DefaultBlSourcingStrategyService() {
    //default constructor
  }

  /**
   * To get list of sourcing strategies.
   *
   * @param context  -  the SourcingContext
   * @param mappers   -  the list of SourcingStrategyMapper
   * @return SourcingStrategy list
   */
  public List<SourcingStrategy> getStrategies(
      final SourcingContext context, final Collection<SourcingStrategyMapper> mappers) {

    final List<SourcingStrategy> strategies = new ArrayList<>();
    final Iterator<SourcingStrategyMapper> var5 = mappers.iterator();

    while (var5.hasNext()) {
      final SourcingStrategyMapper mapper = var5.next();
      final Boolean isMatching = mapper.isMatch(context);
      if (Boolean.TRUE.equals(isMatching)) {
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

  public void setDefaultStrategies(final List<SourcingStrategy> defaultStrategies) {
    this.defaultStrategies = defaultStrategies;
  }
}
