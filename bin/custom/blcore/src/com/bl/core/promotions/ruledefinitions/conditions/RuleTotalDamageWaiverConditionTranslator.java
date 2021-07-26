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
 * This condition translator is added to check total damage waiver price conditon
 * @author Ritika
 *
 */
public class RuleTotalDamageWaiverConditionTranslator  extends AbstractRuleConditionTranslator {


  /**
   * Translator to read and map condition values
   * @param context
   * @param condition
   * @param conditionDefinition
   * @return translated condition
   */
  public RuleIrCondition translate(final RuleCompilerContext context,final RuleConditionData condition,final RuleConditionDefinitionData conditionDefinition) {
    final Map<String, RuleParameterData> conditionParameters = condition.getParameters();
    final RuleParameterData operatorParameter = conditionParameters.get(BlCoreConstants.OPERATOR);
    final RuleParameterData valueParameter = conditionParameters.get(BlCoreConstants.TOTAL_WITH_DAMAGE_WAIVER);


    if (this.verifyAllPresent(operatorParameter, valueParameter)) {
      final AmountOperator operator = operatorParameter.getValue();
      final Map<String, BigDecimal> value = valueParameter.getValue();
      if (this.verifyAllPresent(operator, value)) {
        return this.getTotalDamageWaiverConditions(context, operator, value);
      }
    }

    return IrConditions.empty();
  }

  /**
   * Checking condition for damage waiver cost
   * @param context
   * @param operator
   * @param value
   * @return group condition
   */
  protected RuleIrGroupCondition getTotalDamageWaiverConditions(final RuleCompilerContext context,final AmountOperator operator, final Map<String, BigDecimal> value) {
    final RuleIrGroupCondition irDamageWaiverTotalCondition = RuleIrGroupConditionBuilder.newGroupConditionOf(RuleIrGroupOperator.OR).build();
    this.addDamageWaiverTotal(context, operator, value, irDamageWaiverTotalCondition);
    return irDamageWaiverTotalCondition;
  }

  /**
   * Add damage waiver cost
   * @param context
   * @param operator
   * @param value
   * @param irCartTotalCondition
   */
  protected void addDamageWaiverTotal(final RuleCompilerContext context, AmountOperator operator,final Map<String, BigDecimal> value, final RuleIrGroupCondition irCartTotalCondition) {
    final String cartRaoVariable = context.generateVariable(CartRAO.class);
    Iterator<Entry<String, BigDecimal>> var7 = value.entrySet().iterator();

    while(var7.hasNext()) {
      Entry<String, BigDecimal> entry = var7.next();
      if (this.verifyAllPresent(entry.getKey(), entry.getValue())) {
        final RuleIrGroupCondition irCurrencyGroupCondition = RuleIrGroupConditionBuilder.newGroupConditionOf(RuleIrGroupOperator.AND).build();
        List<RuleIrCondition> ruleIrConditions = irCurrencyGroupCondition.getChildren();
        ruleIrConditions.add(RuleIrAttributeConditionBuilder.newAttributeConditionFor(cartRaoVariable).withAttribute(BlCoreConstants.CURRENCY_ISOCODE).withOperator(
            RuleIrAttributeOperator.EQUAL).withValue(entry.getKey()).build());
        ruleIrConditions.add(RuleIrAttributeConditionBuilder.newAttributeConditionFor(cartRaoVariable).withAttribute(BlCoreConstants.TOTAL_INCLUDING_DAMAGE_WAIVER).withOperator(RuleIrAttributeOperator.valueOf(operator.name())).withValue(entry.getValue()).build());
        irCartTotalCondition.getChildren().add(irCurrencyGroupCondition);
      }
    }
  }
}
