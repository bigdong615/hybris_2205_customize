package com.bl.core.order;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.util.PriceValue;


/**
 * Bl Calculation service to calculate custom attributes by adding custom calculation logic
 *
 * @author Ravikumar
 *
 */
public interface BlCalculationService extends CalculationService
{

 /**
  * this method created to calculate extend order
  */
 void recalculateForExtendOrder(final AbstractOrderModel orderModel , final int defaultAddedTimeForExtendRental) throws CalculationException;

 /**
  * This method created to calculate entries for extend rent
  */
 void calculateEntriesForExtendOrder(final AbstractOrderModel order, final boolean forceRecalculate , final int defaultAddedTimeForExtendRental) throws CalculationException;

  /**
  * This method created to reset entry level changes for extend rent
  */
  void resetAllValuesForExtendOrder(final AbstractOrderEntryModel entry , final int defaultAddedTimeForExtendRental) throws CalculationException;

 /**
  * This method created to get dynamic price based on dates selected for extend rent
  */
 PriceValue getDynamicBasePriceForRentalExtendOrderSku(final PriceValue basePrice, final ProductModel product , final int defaultAddedTimeForExtendRental);


 /**
  * This method created to recalculate order from cscockpit
  */
 void recalculateOrderForTax(final AbstractOrderModel order) throws CalculationException;


 /**
  * This method created to recalculate order entries from cscockpit
  */
 void calculateEntriesForTax(final AbstractOrderModel order, final boolean forceRecalculate)
    throws CalculationException;

 /**
  * This method created to reset the values while recalculating tax from cscockpit
  */
 void resetAllValuesForTax(final AbstractOrderEntryModel entry) throws CalculationException;

 /**
  * This method created to get base price for product which is open or past order while calculating tax from cscockit
  */
 PriceValue getDynamicBasePriceForTax(final PriceValue basePrice, final ProductModel product , final AbstractOrderModel abstractOrder);
}
