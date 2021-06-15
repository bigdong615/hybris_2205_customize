package com.bl.core.promotions.ruledefinitions.actions;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;


import com.bl.core.price.service.BlCommercePriceService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.product.ProductModel;

import de.hybris.platform.product.ProductService;
import de.hybris.platform.ruleengineservices.calculation.NumberedLineItem;
import de.hybris.platform.ruleengineservices.rao.AbstractRuleActionRAO;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rao.DiscountRAO;
import de.hybris.platform.ruleengineservices.rao.OrderEntryRAO;
import de.hybris.platform.ruleengineservices.rao.RuleEngineResultRAO;
import de.hybris.platform.ruleengineservices.rule.evaluation.RuleActionContext;
import de.hybris.platform.ruleengineservices.rule.evaluation.actions.AbstractRuleExecutableSupport;

import de.hybris.platform.servicelayer.dto.converter.Converter;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This action is created to provide discount for
 * free rental dates applicable on promotion
 * @author Ritika
 *
 */
public class RuleFreeRentalDatesRAOAction extends AbstractRuleExecutableSupport {

  private static final Logger LOG = Logger.getLogger(RuleFreeRentalDatesRAOAction.class);

  private BlCommercePriceService blCommercePriceService;
  private ProductService productService;
  private Converter<OrderEntryRAO, NumberedLineItem> orderEntryRaoToNumberedLineItemConverter;

  public RuleFreeRentalDatesRAOAction() {
  }

  /**
   * Protected method called to implement logic for additional rental days
   *
   * @param context
   * @return
   */
  @Override
  public boolean performActionInternal(RuleActionContext context) {

    List<Date> freeRentalDates = (List<Date>) context.getParameter(BlCoreConstants.FREE_RENTAL_DATES);
    return this.performAction(context, freeRentalDates);
  }

  /**
   * Action on the Rule context to apply promotion action for the free rental days with discount
   * calculation to cartRAO
   *
   * @param context
   * @param freeRentalDates
   * @return will return true if action performed
   */
  protected boolean performAction(final RuleActionContext context, final List<Date> freeRentalDates) {
    CartRAO cartRAO = context.getCartRao();
    final BigDecimal noDiscountTotal;
    if (CollectionUtils.isNotEmpty(freeRentalDates)) {
      int noDiscountDays = cartRAO.getRentalDurationDays() - getFreeRentalDaysFromRentalDuration(cartRAO,freeRentalDates);
      noDiscountTotal = getPromotionRentalDurationPrice(cartRAO, context, noDiscountDays);
      BigDecimal finalDiscount = cartRAO.getSubTotal().subtract(noDiscountTotal).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
      BlLogger.logMessage(LOG, Level.INFO, "Old cart Total: " + cartRAO.getTotal());
      BlLogger.logMessage(LOG, Level.INFO, "Discount: " + finalDiscount);
      BlLogger.logMessage(LOG, Level.INFO, "New Subtotal: " + noDiscountTotal);
      cartRAO.setSubTotal(noDiscountTotal);
      //cartRAO.setTotal(noDiscountTotal);
      //cartRAO.setTotalIncludingCharges(noDiscountTotal);
      DiscountRAO discount = this.getRuleEngineCalculationService().addOrderLevelDiscount(cartRAO, true, finalDiscount );
      BlLogger.logMessage(LOG, Level.INFO, "Discount calculated for free dates: " + finalDiscount);
      RuleEngineResultRAO result = context.getRuleEngineResultRao();
      result.getActions().add(discount);
      this.setRAOMetaData(context, new AbstractRuleActionRAO[]{discount});
      context.scheduleForUpdate(new Object[]{cartRAO, discount});
      context.insertFacts(new Object[]{discount});

      return true;
    }
    return false;
  }

  /**
   * Get total free days in the Rental duration
   * @param cartRAO
   * @param freeRentalDates
   * @return
   */
  private int getFreeRentalDaysFromRentalDuration(final CartRAO cartRAO,final  List<Date> freeRentalDates) {
    int freeDays = 0;
    for(Date freeDate: freeRentalDates) {
      if ((cartRAO.getRentalArrivalDate().compareTo(freeDate) * freeDate.compareTo(cartRAO.getRentalToDate()) >= 0)){
        freeDays = freeDays + 1;
      }
    }
    return freeDays;
  }


  /**
   * Get Promotion Duration Price for Additional Rental days to calculate discount
   *
   * @param cartRao
   * @param context
   * @param rentalDays
   * @return
   */

  private BigDecimal getPromotionRentalDurationPrice(final CartRAO cartRao,
      final RuleActionContext context, final Integer rentalDays) {
    BigDecimal totalRentalPrice = BigDecimal.ZERO;
    int  entryNumber = 1;
    for (OrderEntryRAO entry : cartRao.getEntries()) {
      if (Objects.nonNull(entry.getPrice()) && rentalDays > 0) {
        BlProductModel blProduct = (BlProductModel) this.findProduct(entry.getProductCode(), context);
        Double basePrice = CollectionUtils.isNotEmpty(blProduct.getEurope1Prices()) ? blProduct.getEurope1Prices().iterator().next().getPrice() : 0.0D;
        BigDecimal updatedEntryRentalPrice = getBlCommercePriceService().getDynamicPriceDataForProduct(blProduct.getConstrained(),basePrice, rentalDays.longValue()).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
        updatedEntryRentalPrice = rentalDays == 1 || rentalDays == 2 ? updatedEntryRentalPrice.divide(new BigDecimal(3)).multiply(new BigDecimal(rentalDays)).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE) : updatedEntryRentalPrice;
        updatedEntryRentalPrice = updatedEntryRentalPrice.multiply(new BigDecimal(entry.getAvailableQuantity())).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);

        totalRentalPrice = totalRentalPrice.add(updatedEntryRentalPrice).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
        entry.setPrice(updatedEntryRentalPrice);
        entry.setEntryNumber(entryNumber);
       // entry.setBasePrice(updatedEntryRentalPrice);
        //entry.setTotalPrice(updatedEntryRentalPrice);
        BlLogger.logMessage(LOG, Level.INFO,
            " extended day cart entry price for : "+rentalDays +"rental days" + "for product" + entry
                .getProductCode() +" is : "+ updatedEntryRentalPrice );

        this.getOrderEntryRaoToNumberedLineItemConverter().convert(entry);
        entryNumber++;
      }
    }
    return totalRentalPrice;
  }




  /**
   * Find product by code
   *
   * @param productCode
   * @param context
   * @return
   */
  protected ProductModel findProduct(String productCode, RuleActionContext context) {
    ProductModel product = null;
    try {
      product = this.getProductService().getProductForCode(productCode);
    } catch (Exception var5) {
      BlLogger.logMessage(LOG, Level.ERROR,
          "no product found for code" + productCode + "in rule" + this.getRuleCode(context)
              + "cannot apply rule action.");

    }
    return product;
  }


  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }

  public Converter<OrderEntryRAO, NumberedLineItem> getOrderEntryRaoToNumberedLineItemConverter() {
    return orderEntryRaoToNumberedLineItemConverter;
  }

  public void setOrderEntryRaoToNumberedLineItemConverter(
      Converter<OrderEntryRAO, NumberedLineItem> orderEntryRaoToNumberedLineItemConverter) {
    this.orderEntryRaoToNumberedLineItemConverter = orderEntryRaoToNumberedLineItemConverter;
  }

  public BlCommercePriceService getBlCommercePriceService() {
    return blCommercePriceService;
  }

  public void setBlCommercePriceService(
      BlCommercePriceService blCommercePriceService) {
    this.blCommercePriceService = blCommercePriceService;
  }
}
