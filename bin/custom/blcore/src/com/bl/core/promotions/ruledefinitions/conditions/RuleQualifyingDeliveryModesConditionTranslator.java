package com.bl.core.promotions.ruledefinitions.conditions;

import com.bl.core.constants.BlCoreConstants;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;
import de.hybris.platform.ruledefinitions.conditions.builders.IrConditions;
import de.hybris.platform.ruledefinitions.conditions.builders.RuleIrAttributeConditionBuilder;
import de.hybris.platform.ruledefinitions.conditions.builders.RuleIrGroupConditionBuilder;
import de.hybris.platform.ruleengineservices.compiler.RuleCompilerContext;
import de.hybris.platform.ruleengineservices.compiler.RuleConditionTranslator;
import de.hybris.platform.ruleengineservices.compiler.RuleIrAttributeCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrAttributeOperator;
import de.hybris.platform.ruleengineservices.compiler.RuleIrCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrFalseCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrGroupCondition;
import de.hybris.platform.ruleengineservices.compiler.RuleIrGroupOperator;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rule.data.RuleConditionData;
import de.hybris.platform.ruleengineservices.rule.data.RuleConditionDefinitionData;
import de.hybris.platform.ruleengineservices.rule.data.RuleParameterData;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;

/**
 * This class created for custom condition for delivery mode
 * @author Manikandan
 */
public class RuleQualifyingDeliveryModesConditionTranslator implements RuleConditionTranslator {

  /**
   * This method set the required data for deliver modes
   */
  @Override
  public RuleIrCondition translate(final RuleCompilerContext ruleCompilerContext, final RuleConditionData ruleConditionData,
      final RuleConditionDefinitionData ruleConditionDefinitionData) {
    final Map<String, RuleParameterData> conditionParameters = ruleConditionData.getParameters();
    final RuleParameterData ruleParameterData = conditionParameters.get("deliveryModes");

    if (ruleParameterData == null) {
      return new RuleIrFalseCondition();
    }   else {
      List<String> qualifyingDeliveryModes = (List) ruleParameterData.getValue();
      //qualifyingDeliveryModes.add("ANY_ZONE");
      if (CollectionUtils.isEmpty(qualifyingDeliveryModes)) {
        return new RuleIrFalseCondition();
      } else {
        RuleIrAttributeCondition irShippingAllowedCondition = new RuleIrAttributeCondition();
        String cartRaoVariable = ruleCompilerContext.generateVariable(CartRAO.class);
        irShippingAllowedCondition.setVariable(cartRaoVariable);
        irShippingAllowedCondition.setAttribute("orderDeliveryMode");
        irShippingAllowedCondition.setOperator(RuleIrAttributeOperator.IN);
        irShippingAllowedCondition.setValue(qualifyingDeliveryModes);
        return irShippingAllowedCondition;
      }
    }
  }
}