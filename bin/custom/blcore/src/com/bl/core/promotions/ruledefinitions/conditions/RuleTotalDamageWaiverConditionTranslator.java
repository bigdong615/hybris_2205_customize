package com.bl.core.promotions.ruledefinitions.conditions;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.ruledefinitions.AmountOperator;
import de.hybris.platform.ruledefinitions.conditions.AbstractRuleConditionTranslator;
import de.hybris.platform.ruledefinitions.conditions.builders.IrConditions;
import de.hybris.platform.ruledefinitions.conditions.builders.RuleIrAttributeConditionBuilder;
import de.hybris.platform.ruledefinitions.conditions.builders.RuleIrGroupConditionBuilder;
import de.hybris.platform.ruleengineservices.compiler.RuleCompilerContext;
import de.hybris.platform.ruleengineservices.compiler.RuleIrAttributeOperator;
import de.hybris.platform.ruleengineservices.compiler.RuleIrCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrGroupCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrGroupOperator;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rule.data.RuleConditionData;
import de.hybris.platform.ruleengineservices.rule.data.RuleConditionDefinitionData;
import de.hybris.platform.ruleengineservices.rule.data.RuleParameterData;
import java.math.BigDecimal;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 * @author Ritika
 * This condition translator is added to check total damage waiver price conditon
 *
 */
public class RuleTotalDamageWaiverConditionTranslator  extends AbstractRuleConditionTranslator {


  public RuleIrCondition translate(RuleCompilerContext context, RuleConditionData condition, RuleConditionDefinitionData conditionDefinition) {
    Map<String, RuleParameterData> conditionParameters = condition.getParameters();
    RuleParameterData operatorParameter = conditionParameters.get(BlCoreConstants.OPERATOR);
    RuleParameterData valueParameter = conditionParameters.get(BlCoreConstants.TOTAL_WITH_DAMAGE_WAIVER);


    if (this.verifyAllPresent(operatorParameter, valueParameter)) {
      AmountOperator operator = operatorParameter.getValue();
      Map<String, BigDecimal> value = valueParameter.getValue();
      if (this.verifyAllPresent(operator, value)) {
        return this.getTotalDamageWaiverConditions(context, operator, value);
      }
    }

    return IrConditions.empty();
  }

  protected RuleIrGroupCondition getTotalDamageWaiverConditions(RuleCompilerContext context, AmountOperator operator, Map<String, BigDecimal> value) {
    RuleIrGroupCondition irDamageWaiverTotalCondition = RuleIrGroupConditionBuilder.newGroupConditionOf(RuleIrGroupOperator.OR).build();
    this.addDamageWaiverTotal(context, operator, value, irDamageWaiverTotalCondition);
    return irDamageWaiverTotalCondition;
  }

  protected void addDamageWaiverTotal(RuleCompilerContext context, AmountOperator operator, Map<String, BigDecimal> value, RuleIrGroupCondition irCartTotalCondition) {
    String cartRaoVariable = context.generateVariable(CartRAO.class);
    Iterator<Entry<String, BigDecimal>> var7 = value.entrySet().iterator();

    while(var7.hasNext()) {
      Entry<String, BigDecimal> entry = var7.next();
      if (this.verifyAllPresent(entry.getKey(), entry.getValue())) {
        RuleIrGroupCondition irCurrencyGroupCondition = RuleIrGroupConditionBuilder.newGroupConditionOf(RuleIrGroupOperator.AND).build();
        List<RuleIrCondition> ruleIrConditions = irCurrencyGroupCondition.getChildren();
        ruleIrConditions.add(RuleIrAttributeConditionBuilder.newAttributeConditionFor(cartRaoVariable).withAttribute("currencyIsoCode").withOperator(
            RuleIrAttributeOperator.EQUAL).withValue(entry.getKey()).build());
        ruleIrConditions.add(RuleIrAttributeConditionBuilder.newAttributeConditionFor(cartRaoVariable).withAttribute(BlCoreConstants.TOTAL_INCLUDING_DAMAGE_WAIVER).withOperator(RuleIrAttributeOperator.valueOf(operator.name())).withValue(entry.getValue()).build());
        irCartTotalCondition.getChildren().add(irCurrencyGroupCondition);
      }
    }

  }


}
