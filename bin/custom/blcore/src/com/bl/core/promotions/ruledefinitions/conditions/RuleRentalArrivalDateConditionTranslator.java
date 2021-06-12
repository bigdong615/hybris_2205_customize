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
import java.util.Date;
import java.util.Map;

/**
 * This class is created for setting a rental arrival date
 * in promotion conditions
 *
 * @author Ritika
 */
public class RuleRentalArrivalDateConditionTranslator implements RuleConditionTranslator {

  public static final String RENTAL_ARRIVAL_DATE = "rentalArrivalDate";
  public static final String OPERATOR = "operator";

  @Override
  public RuleIrCondition translate(final RuleCompilerContext ruleCompilerContext,final RuleConditionData condition,final RuleConditionDefinitionData ruleConditionDefinitionData) {

    final Map<String, RuleParameterData> conditionParameters = condition.getParameters();

    final Date valueParameter = conditionParameters.get(RENTAL_ARRIVAL_DATE).getValue();
    final AmountOperator operatorParameter = conditionParameters.get(OPERATOR).getValue();

    final RuleIrGroupCondition irRentalArrivalDateCondition = RuleIrGroupConditionBuilder
        .newGroupConditionOf(RuleIrGroupOperator.AND).build();

    if (valueParameter == null) {
      return new RuleIrFalseCondition();
    } else {
      final String cartRaoVariable = ruleCompilerContext.generateVariable(CartRAO.class);
      irRentalArrivalDateCondition.getChildren().add(RuleIrAttributeConditionBuilder.newAttributeConditionFor(cartRaoVariable)
          .withAttribute(RENTAL_ARRIVAL_DATE).withOperator(RuleIrAttributeOperator.valueOf(operatorParameter.name())).withValue(valueParameter).build());
    }
    return irRentalArrivalDateCondition;
  }
}

