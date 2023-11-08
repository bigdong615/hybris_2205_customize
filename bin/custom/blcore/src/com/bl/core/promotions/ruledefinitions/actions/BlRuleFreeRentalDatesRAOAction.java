package com.bl.core.promotions.ruledefinitions.actions;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;


import com.bl.core.price.service.BlCommercePriceService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.product.ProductModel;

import de.hybris.platform.product.ProductService;
import de.hybris.platform.ruleengineservices.calculation.NumberedLineItem;
import de.hybris.platform.ruleengineservices.rao.*;
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
public class BlRuleFreeRentalDatesRAOAction extends AbstractRuleExecutableSupport {

  private static final Logger LOG = Logger.getLogger(BlRuleFreeRentalDatesRAOAction.class);

  private BlCommercePriceService blCommercePriceService;
  private ProductService productService;
  private Converter<OrderEntryRAO, NumberedLineItem> orderEntryRaoToNumberedLineItemConverter;

  public BlRuleFreeRentalDatesRAOAction() {
    //Do Nothing
  }

  /**
   * This method is called to implement logic for freeDates discount action
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
   * calculation to cartRao
   *
   * @param context
   * @param freeRentalDates
   * @return will return true if action performed
   */
  protected boolean performAction(final RuleActionContext context, final List<Date> freeRentalDates) {
    CartRAO cartRao = context.getCartRao();
    if (CollectionUtils.isNotEmpty(freeRentalDates) && cartRao.getRentalDurationDays() > 0 && getFreeRentalDaysFromRentalDuration(cartRao,freeRentalDates) > 0) {
      final int noDiscountDays = cartRao.getRentalDurationDays() - getFreeRentalDaysFromRentalDuration(cartRao,freeRentalDates);
      final BigDecimal noDiscountTotal = getPromotionRentalDurationPrice(cartRao, context, noDiscountDays);
      BigDecimal finalDiscount = cartRao.getSubTotal().subtract(noDiscountTotal).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
      BlLogger.logMessage(LOG, Level.DEBUG, "Total Rental days cart subTotal: " + cartRao.getSubTotal());
      BlLogger.logMessage(LOG, Level.DEBUG, "Discount: " + finalDiscount);
      BlLogger.logMessage(LOG, Level.DEBUG, "Subtotal for non-discounted days: " + noDiscountTotal);
      cartRao.setSubTotal(noDiscountTotal);
      DiscountRAO discount = this.getRuleEngineCalculationService().addOrderLevelDiscount(cartRao, true, finalDiscount );
      BlLogger.logMessage(LOG, Level.DEBUG, "Discount calculated for free dates: " + finalDiscount);
      RuleEngineResultRAO result = context.getRuleEngineResultRao();
      result.getActions().add(discount);
      this.setRAOMetaData(context, discount);
      context.scheduleForUpdate(cartRao, discount);
      context.insertFacts(discount);
      return true;
    }
    return false;
  }

  /**
   * Get total free days in the Rental duration
   * @param cartRao
   * @param freeRentalDates
   * @return
   */
  private int getFreeRentalDaysFromRentalDuration(final CartRAO cartRao,final  List<Date> freeRentalDates) {
    int totalFreeDays = 0;
    for(Date freeDate: freeRentalDates) {
      if (((null != cartRao.getRentalArrivalDate() && cartRao.getRentalToDate() != null) && cartRao.getRentalArrivalDate().compareTo(freeDate) * freeDate.compareTo(cartRao.getRentalToDate()) >= 0)){
        totalFreeDays = totalFreeDays + 1;
      }
    }
    return totalFreeDays;
  }


  /**
   * Get Duration Price for non discounted Rental days
   *
   * @param cartRao
   * @param context
   * @param rentalDays
   * @return
   */
  private BigDecimal getPromotionRentalDurationPrice(final CartRAO cartRao, final RuleActionContext context, final Integer rentalDays) {
    BigDecimal totalRentalPrice = BigDecimal.ZERO;
    int  entryNumber = 0;
    for (OrderEntryRAO entry : cartRao.getEntries()) {
      if (Objects.nonNull(entry.getPrice()) && rentalDays > 0) {
        final BlProductModel blProduct = (BlProductModel) this.findProduct(entry.getProductCode(), context);
        if (blProduct != null) {
          final Double basePrice = CollectionUtils.isNotEmpty(blProduct.getEurope1Prices()) ? blProduct.getEurope1Prices().iterator().next().getPrice() : 0.0D;
          BigDecimal updatedEntryRentalPrice = getBlCommercePriceService().getDynamicPriceDataForProduct(blProduct.getConstrained(), basePrice, rentalDays.longValue()).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
          updatedEntryRentalPrice = rentalDays <= 2 ? updatedEntryRentalPrice.divide(new BigDecimal(3)).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE).multiply(new BigDecimal(rentalDays)).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE)  : updatedEntryRentalPrice;
          updatedEntryRentalPrice = updatedEntryRentalPrice.multiply(new BigDecimal(entry.getAvailableQuantity())).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
          totalRentalPrice = totalRentalPrice.add(updatedEntryRentalPrice).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
          entry.setPrice(updatedEntryRentalPrice);
          entry.setEntryNumber(entryNumber);
          BlLogger.logMessage(LOG, Level.DEBUG," extended day cart entry price for : " + rentalDays + "rental days" + "for product" + entry.getProductCode() + " is : " + updatedEntryRentalPrice);
          this.getOrderEntryRaoToNumberedLineItemConverter().convert(entry);
          entryNumber++;
        }
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
  protected ProductModel findProduct(final String productCode,final RuleActionContext context) {
    try {
      return  this.getProductService().getProductForCode(productCode);
    } catch (final Exception exception) {
      BlLogger.logMessage(LOG, Level.ERROR,
          "no product found for code" + productCode + "in rule" + this.getRuleCode(context)
              + "cannot apply rule action.");

    }
    return null;
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
