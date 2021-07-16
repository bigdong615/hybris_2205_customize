package com.bl.core.promotions.ruledefinitions.conditions;

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

public class RuleUsedGearOnSaleConditionTranslator implements RuleConditionTranslator
{

  @Override
  public RuleIrCondition translate(final RuleCompilerContext ruleCompilerContext, final RuleConditionData condition,
      final RuleConditionDefinitionData conditionDefinition)
  {
    final Map<String, RuleParameterData> conditionParameters = condition.getParameters();
    final Boolean valueParameter = conditionParameters.get("value").getValue();

    final RuleIrGroupCondition irRentalCartCondition = RuleIrGroupConditionBuilder
        .newGroupConditionOf(RuleIrGroupOperator.AND).build();

    if (valueParameter == null)
    {
      return new RuleIrFalseCondition();
    }
    else
    {
      final String cartRaoVariable = ruleCompilerContext.generateVariable(CartRAO.class);
      irRentalCartCondition.getChildren().add(RuleIrAttributeConditionBuilder.newAttributeConditionFor(cartRaoVariable)
          .withAttribute("usedGearOnSale").withOperator(RuleIrAttributeOperator.EQUAL).withValue(valueParameter).build());
    }

    return irRentalCartCondition;
  }
}