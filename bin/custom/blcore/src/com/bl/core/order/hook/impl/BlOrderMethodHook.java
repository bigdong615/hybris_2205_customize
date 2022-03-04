package com.bl.core.order.hook.impl;

import de.hybris.platform.commerceservices.order.hook.CommercePlaceOrderMethodHook;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.Objects;

import org.apache.log4j.Logger;

import com.bl.core.utils.BlDateTimeUtils;


/**
 * It populates the fields on order as required
 *
 * @author Ravikumar
 *
 */
public class BlOrderMethodHook implements CommercePlaceOrderMethodHook
{
	private static final Logger LOG = Logger.getLogger(BlOrderMethodHook.class);
	private ModelService modelService;

	@Override
	public void afterPlaceOrder(final CommerceCheckoutParameter checkoutParameter, final CommerceOrderResult commerceOrderResult)
			throws InvalidCartException
	{
		final OrderModel order = commerceOrderResult.getOrder();
		if (Objects.nonNull(order))
		{
			getModelService().refresh(order);
			setValuesForRunTAttributes(order);
		}
	}

	/**
	 * Sets the values for run T attributes.
	 *
	 * @param order
	 *           the new values for run T attributes
	 */
	private void setValuesForRunTAttributes(final OrderModel order)
	{
		order.setRunTot_grandTotal(getGrandTotalFromOrder(order));
		order.setRunTot_subtotal(order.getSubtotal());
		order.setRunTot_totalOptionsCost(order.getTotalOptionsCost());
		order.setRunTot_totalPrice(order.getTotalPrice());
		order.setRunTot_totalTax(order.getTotalTax());
		order.setRunTot_daysRented(
				Long.valueOf(BlDateTimeUtils.getDaysBetweenDates(order.getRentalStartDate(), order.getRentalEndDate())).intValue());
		getModelService().save(order);
	}

	/**
	 * Gets the grand total from order.
	 *
	 * @param order
	 *           the order
	 * @return the grand total from order
	 */
	private Double getGrandTotalFromOrder(final OrderModel order)
	{
		if (Objects.isNull(order.getGrandTotal()) || order.getGrandTotal().compareTo(Double.valueOf(0.0d)) <= 0)
		{
			return order.getTotalPrice();
		}
		return order.getGrandTotal();
	}

	@Override
	public void beforePlaceOrder(final CommerceCheckoutParameter checkoutParameter) throws InvalidCartException
	{
		// Not implemented

	}

	@Override
	public void beforeSubmitOrder(final CommerceCheckoutParameter checkoutParameter, final CommerceOrderResult commerceOrderResult)
			throws InvalidCartException
	{
		// Not implemented

	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

}
