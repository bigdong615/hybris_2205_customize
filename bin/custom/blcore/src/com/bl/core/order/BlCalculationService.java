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
	// TO-DO Add custom calculation logic

 void recalculateForExtendOrder(final AbstractOrderModel orderModel , int defaultAddedTimeForExtendRental) throws CalculationException;

 void calculateEntriesForExtendOrder(final AbstractOrderModel order, final boolean forceRecalculate , int defaultAddedTimeForExtendRental) throws CalculationException;

  void resetAllValuesForExtendOrder(final AbstractOrderEntryModel entry , int defaultAddedTimeForExtendRental) throws CalculationException;

 PriceValue getDynamicBasePriceForRentalExtendOrderSku(final PriceValue basePrice, final ProductModel product , int defaultAddedTimeForExtendRental);
}
