package com.bl.core.promotions.ruledefinitions.conditions;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.ruledefinitions.AmountOperator;
import de.hybris.platform.ruledefinitions.conditions.builders.RuleIrAttributeConditionBuilder;
import de.hybris.platform.ruledefinitions.conditions.builders.RuleIrGroupConditionBuilder;
import de.hybris.platform.ruleengineservices.compiler.RuleCompilerContext;
import de.hybris.platform.ruleengineservices.compiler.RuleConditionTranslator;
import de.hybris.platform.ruleengineservices.compiler.RuleIrAttributeOperator;
import de.hybris.platform.ruleengineservices.compiler.RuleIrCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrFalseCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrGroupCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrGroupOperator;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rule.data.RuleConditionData;
import de.hybris.platform.ruleengineservices.rule.data.RuleConditionDefinitionData;
import de.hybris.platform.ruleengineservices.rule.data.RuleParameterData;
import java.util.Map;

/**
 * This class is created for Determining the minimum extended rental duration
 * promotion conditions for rental products
 *
 * @author Ritika
 */
public class RuleExtendedDurationConditionTranslator implements RuleConditionTranslator {

  @Override
  public RuleIrCondition translate(final RuleCompilerContext ruleCompilerContext,final RuleConditionData condition,final RuleConditionDefinitionData ruleConditionDefinitionData) {

    final Map<String, RuleParameterData> conditionParameters = condition.getParameters();
    final Integer rentalDuration = conditionParameters.get("extendedRentalDuration").getValue();
    final AmountOperator operatorParameter = conditionParameters.get("extendedDurationOperator").getValue();

    final RuleIrGroupCondition irRentalDurationCondition = RuleIrGroupConditionBuilder.newGroupConditionOf(RuleIrGroupOperator.AND).build();

    if (rentalDuration == null) {
      return new RuleIrFalseCondition();
    } else {
      final String cartRaoVariable = ruleCompilerContext.generateVariable(CartRAO.class);
      irRentalDurationCondition.getChildren().add(RuleIrAttributeConditionBuilder.newAttributeConditionFor(cartRaoVariable)
          .withAttribute("extendedDurationDays").withOperator(RuleIrAttributeOperator.valueOf(operatorParameter.name())).withValue(rentalDuration).build());
    }
    return irRentalDurationCondition;
  }
}