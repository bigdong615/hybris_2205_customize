package com.bl.core.promotions.ruledefinitions.actions;

import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rao.DiscountRAO;
import de.hybris.platform.ruleengineservices.rao.OrderEntryRAO;
import de.hybris.platform.ruleengineservices.rao.RuleEngineResultRAO;
import de.hybris.platform.ruleengineservices.rule.evaluation.RuleActionContext;
import de.hybris.platform.ruleengineservices.rule.evaluation.actions.AbstractRuleExecutableSupport;
import de.hybris.platform.servicelayer.model.ModelService;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;

/**
 * This action is created to allow extended free rental days
 * @author Ritika
 */
public class BlExtendedFreeRentalDaysRAOAction extends AbstractRuleExecutableSupport {

  private static final Logger LOG = Logger.getLogger(BlExtendedFreeRentalDaysRAOAction.class);
  public static final String FREE_RENTAL_DAYS = "extendedFreeRentalDays";

  private BlDatePickerService blDatePickerService;
  private BlCommercePriceService blCommercePriceService;
  private ProductService productService;
  private BlCartService cartService;
  private ModelService modelService;


  public BlExtendedFreeRentalDaysRAOAction() {
    // Do Nothing
  }

  /**
   * Protected method called to implement logic for additional rental days
   * @param context
   * @return
   */
  @Override
  public boolean performActionInternal(final RuleActionContext context) {

    final Integer freeRentalDays = context.getParameter(FREE_RENTAL_DAYS, Integer.class);
    return this.performAction(context, freeRentalDays);
  }

  /**
   * Action on the Rule context to apply promotion action for the free rental days
   * with discount calculation to cartRAO
   * @param context
   * @param freeRentalDays
   * @return will return true if action performed
   */
  protected boolean performAction(final RuleActionContext context, final Integer freeRentalDays) {
    final CartRAO cartRAO = context.getCartRao();

    if(freeRentalDays > 0 && cartRAO.getRentalDurationDays() > 0) {
      final BigDecimal newExtendedDaysSubtotal;
      final Date updatedRentalToDate = getUpdatedEndDate(cartRAO,freeRentalDays);
      final int rentalDays = cartRAO.getRentalDurationDays() + freeRentalDays;
      newExtendedDaysSubtotal = getPromotionRentalDurationPrice(cartRAO,context, rentalDays);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Subtotal for extended rental days :{}  is: {}",rentalDays, newExtendedDaysSubtotal);
      //original subtotal without rental days
      final BigDecimal existingSubTotal = getPromotionRentalDurationPrice(cartRAO,context, cartRAO.getRentalDurationDays());

      if( BlDateTimeUtils.getDaysBetweenDates(cartRAO.getRentalArrivalDate(),updatedRentalToDate) == rentalDays) {

			//Need to revisit, not sending rental dates in formated way, for tealium
			getBlDatePickerService().addRentalDatesIntoSession(
					BlDateTimeUtils.convertDateToStringDate(cartRAO.getRentalArrivalDate(), BlCoreConstants.DATE_FORMAT),
					BlDateTimeUtils.convertDateToStringDate(updatedRentalToDate, BlCoreConstants.DATE_FORMAT), "", "", "");
        getCartService().updatePromotionalEndDate(updatedRentalToDate);
        cartRAO.setRentalToDate(updatedRentalToDate);
      }
      final BigDecimal finalDiscount = newExtendedDaysSubtotal.subtract(existingSubTotal).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Old sub Total:{}", existingSubTotal);
      //set values on cartRao
      cartRAO.setSubTotal(newExtendedDaysSubtotal);
      cartRAO.setTotal(newExtendedDaysSubtotal);
      cartRAO.setTotalIncludingCharges(newExtendedDaysSubtotal);

      final DiscountRAO discount = this.getRuleEngineCalculationService().addOrderLevelDiscount(cartRAO, true, finalDiscount);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Discount calculated for free extended rental days is : {}" ,finalDiscount);

      final RuleEngineResultRAO result = context.getRuleEngineResultRao();
      result.getActions().add(discount);
      this.setRAOMetaData(context, discount);
      context.scheduleForUpdate(cartRAO, discount);
      context.insertFacts(discount);

      return true;
    }

    return  false;
  }


  /**
   * Get Updated end date with verification that end date
   * does not lie in the blackout dates or SAT/SUN
   * @param cartRao
   * @param freeRentalDays
   * @return
   */
  private Date getUpdatedEndDate(final CartRAO cartRao, final Integer freeRentalDays) {
    final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
    Date updatedRentalToDate = addFreeRentalDays(freeRentalDays.longValue(), BlDateTimeUtils.convertDateToStringDate(cartRao.getRentalToDate(),BlCoreConstants.DATE_FORMAT));
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Updated Rental To Date without checking it is blackout date or Weekend: {} ",updatedRentalToDate);
    if(BlDateTimeUtils.isDateFallsOnBlackOutDate(updatedRentalToDate,blackOutDates)){
      updatedRentalToDate = BlDateTimeUtils.addDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,BlDateTimeUtils.convertDateToStringDate(updatedRentalToDate,BlCoreConstants.DATE_FORMAT), blackOutDates);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Updated Rental To Date After checking it is blackout date or Weekend: {} ", updatedRentalToDate);
    }
    return updatedRentalToDate;
  }

  /**
   * Get Promotion Duration Price for Additional Rental days to
   * calculate discount
   * @param cartRao
   * @param context
   * @param rentalDays
   * @return
   */
  private BigDecimal getPromotionRentalDurationPrice(final CartRAO cartRao, final RuleActionContext context, final Integer rentalDays) {
    BigDecimal totalRentalPrice = BigDecimal.ZERO;
    for (final OrderEntryRAO entry : cartRao.getEntries()) {
      if (Objects.nonNull(entry.getPrice()) && rentalDays > 0) {
        final BlProductModel blProduct = (BlProductModel) this.findProduct(entry.getProductCode(), context);
        if(blProduct != null) {
          final Double basePrice = CollectionUtils.isNotEmpty(blProduct.getEurope1Prices()) ? blProduct
              .getEurope1Prices().iterator().next().getPrice() : 0.0D;
          BigDecimal updatedEntryRentalPrice = getBlCommercePriceService().getDynamicPriceDataForProduct(blProduct.getConstrained(), basePrice,rentalDays.longValue()).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
          updatedEntryRentalPrice = updatedEntryRentalPrice.multiply(new BigDecimal(entry.getAvailableQuantity())).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
          totalRentalPrice = totalRentalPrice.add(updatedEntryRentalPrice).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
          entry.setPrice(updatedEntryRentalPrice);
          entry.setBasePrice(updatedEntryRentalPrice);

          BlLogger.logFormatMessageInfo(LOG, Level.DEBUG," extended day cart entry price for : {} rental days for product {} is : {}",rentalDays,entry.getProductCode(), updatedEntryRentalPrice);
        }
      }
    }
    return totalRentalPrice;

  }




  /**
   * Find product by code
   * @param productCode
   * @param context
   * @return
   */
  protected ProductModel findProduct(final String productCode, final RuleActionContext context) {
    final ProductModel product = null;
    try {
     return this.getProductService().getProductForCode(productCode);
    } catch (final Exception var5) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "no product found for code {} in rule {} cannot apply rule action.", productCode, this.getRuleCode(context), var5 );

    }
    return product;
  }

  /**
   *  Add Free Rental days which includes weekends and blackout dates
   * @param numberOfDaysToAdd
   * @param rentalToDate
   * @return
   */
  public Date addFreeRentalDays(final long numberOfDaysToAdd, final String rentalToDate)
  {
    try
    {
      LocalDate localDate = BlDateTimeUtils.convertStringDateToLocalDate(rentalToDate, BlCoreConstants.DATE_FORMAT);
      if (Objects.nonNull(localDate))
      {
        int addedDays = 0;
        while (addedDays < numberOfDaysToAdd) {
          localDate = localDate.plusDays(1);
          addedDays++;
        }
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Added {} free rental days in end date {}",numberOfDaysToAdd,localDate);

        return Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
      }
      return BlDateTimeUtils.getDate(rentalToDate, BlCoreConstants.DATE_FORMAT);
    }
    catch (final DateTimeParseException e)
    {
      return BlDateTimeUtils.getDate(rentalToDate, BlCoreConstants.DATE_FORMAT);
    }
  }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(final BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }



  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(final ProductService productService) {
    this.productService = productService;
  }


  public BlCartService getCartService() {
    return cartService;
  }

  public void setCartService(final BlCartService cartService) {
    this.cartService = cartService;
  }

  public BlCommercePriceService getBlCommercePriceService() {
    return blCommercePriceService;
  }

  public void setBlCommercePriceService(
      final BlCommercePriceService blCommercePriceService) {
    this.blCommercePriceService = blCommercePriceService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }
}


