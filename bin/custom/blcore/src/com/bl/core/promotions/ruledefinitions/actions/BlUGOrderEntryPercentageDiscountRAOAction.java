package com.bl.core.promotions.ruledefinitions.actions;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.ruleengineservices.rao.DiscountRAO;
import de.hybris.platform.ruleengineservices.rao.OrderEntryRAO;
import de.hybris.platform.ruleengineservices.rao.RuleEngineResultRAO;
import de.hybris.platform.ruleengineservices.rule.evaluation.RuleActionContext;
import de.hybris.platform.ruleengineservices.rule.evaluation.actions.AbstractRuleExecutableSupport;
import java.math.BigDecimal;
import java.util.Iterator;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Added to discount only used gear onSale products
 *
 * @author Ritika
 */
public class BlUGOrderEntryPercentageDiscountRAOAction extends AbstractRuleExecutableSupport {

  private static final Logger LOG = Logger.getLogger(BlUGOrderEntryPercentageDiscountRAOAction.class);
  private ProductService productService;

  public BlUGOrderEntryPercentageDiscountRAOAction() {
    //No code
  }

  /**
   * Perform action
   * @param context
   * @return
   */
  @Override
  public boolean performActionInternal(final RuleActionContext context) {
    boolean isPerformed = false;
    Optional<BigDecimal> amount = this.extractAmountForCurrency(context, context.getParameter("value"));
    if (amount.isPresent()) {
      Set<OrderEntryRAO> orderEntries = context.getCartRao().getEntries();
      OrderEntryRAO orderEntryRAO;
      if (CollectionUtils.isNotEmpty(orderEntries)) {
        for(Iterator<OrderEntryRAO> var6 = orderEntries.iterator(); var6.hasNext(); isPerformed |= this.processOrderEntry(context, orderEntryRAO, amount.get())) {
          orderEntryRAO = var6.next();
        }
      }
    }

    return isPerformed;
  }

  /**
   * Process Order Entry for discounts
   * @param context
   * @param orderEntryRao
   * @param value
   * @return
   */
  protected boolean processOrderEntry(final RuleActionContext context, final OrderEntryRAO orderEntryRao, final BigDecimal value) {
    boolean isPerformed = false;
    int consumableQuantity = this.getConsumptionSupport().getConsumableQuantity(orderEntryRao);
    if (consumableQuantity > 0 && isUsedGearProductEntry(orderEntryRao.getProductCode())) {
      DiscountRAO discount = this.getRuleEngineCalculationService().addOrderEntryLevelDiscount(orderEntryRao, false, value);
      this.setRAOMetaData(context, discount);
      this.getConsumptionSupport().consumeOrderEntry(orderEntryRao, consumableQuantity, discount);
      RuleEngineResultRAO result = context.getValue(RuleEngineResultRAO.class);
      result.getActions().add(discount);
      context.scheduleForUpdate(orderEntryRao, orderEntryRao.getOrder(), result);
      context.insertFacts(discount);
      context.insertFacts(discount.getConsumedEntries());
      isPerformed = true;
    }

    return isPerformed;
  }

  /**
   * check if it is used gear product entry
   * @param productCode
   * @return
   */
  private boolean isUsedGearProductEntry(final String productCode) {

     final BlSerialProductModel blSerialProduct = (BlSerialProductModel) getProductService().getProductForCode(productCode);
     if(blSerialProduct != null && blSerialProduct.getBlProduct() != null){
       final BlProductModel blProduct = blSerialProduct.getBlProduct();
       final boolean isOnSaleUsedGearSKU =  BooleanUtils.isTrue(blProduct.getForSale()) && blProduct.getOnSale()!= null && BooleanUtils.isTrue(blProduct.getOnSale());
       BlLogger.logMessage(LOG, Level.INFO, "The condition evaluated for the Used Gear entry"+ (isOnSaleUsedGearSKU && BooleanUtils.isTrue(blSerialProduct.getForSale()) && blSerialProduct.getOnSale() != null && BooleanUtils.isTrue(blSerialProduct.getOnSale())));
       return  isOnSaleUsedGearSKU && BooleanUtils.isTrue(blSerialProduct.getForSale()) && blSerialProduct.getOnSale() != null && BooleanUtils.isTrue(blSerialProduct.getOnSale());
     }
     return  false;
    }

  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }
}
