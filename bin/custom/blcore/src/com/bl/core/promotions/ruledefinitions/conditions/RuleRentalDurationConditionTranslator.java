package com.bl.core.promotions.ruledefinitions.conditions;

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
 * This class is created for Determining the minimum or exact rental duration
 * promotion conditions for rental products
 *
 * @author Ritika
 */
public class RuleRentalDurationConditionTranslator implements RuleConditionTranslator {

  public static final String RENTAL_DURATION = "rentalDuration";
  public static final String RENTAL_DURATION_OPERATOR = "rentalDurationOperator";
  public static final String RENTAL_DURATION_DAYS = "rentalDurationDays";

  @Override
  public RuleIrCondition translate(final RuleCompilerContext ruleCompilerContext,final RuleConditionData condition,final RuleConditionDefinitionData ruleConditionDefinitionData) {

    final Map<String, RuleParameterData> conditionParameters = condition.getParameters();
    final Integer rentalDuration = conditionParameters.get(RENTAL_DURATION).getValue();
    final AmountOperator operatorParameter = conditionParameters.get(RENTAL_DURATION_OPERATOR).getValue();

    final RuleIrGroupCondition irRentalDurationCondition = RuleIrGroupConditionBuilder.newGroupConditionOf(RuleIrGroupOperator.AND).build();

    if (rentalDuration == null) {
      return new RuleIrFalseCondition();
    } else {
      final String cartRaoVariable = ruleCompilerContext.generateVariable(CartRAO.class);
      irRentalDurationCondition.getChildren().add(RuleIrAttributeConditionBuilder.newAttributeConditionFor(cartRaoVariable)
          .withAttribute(RENTAL_DURATION_DAYS).withOperator(RuleIrAttributeOperator.valueOf(operatorParameter.name())).withValue(rentalDuration).build());
    }
    return irRentalDurationCondition;
  }
}