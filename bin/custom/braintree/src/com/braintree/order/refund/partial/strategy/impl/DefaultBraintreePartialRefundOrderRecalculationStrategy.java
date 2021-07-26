package com.braintree.order.refund.partial.strategy.impl;


import com.braintree.order.refund.partial.strategy.BraintreePartialRefundOrderRecalculationStrategy;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.OrderService;
import de.hybris.platform.servicelayer.model.ModelService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;


public class DefaultBraintreePartialRefundOrderRecalculationStrategy implements BraintreePartialRefundOrderRecalculationStrategy
{

	private ModelService modelService;
	private OrderService orderService;

	@Override
	public OrderModel recalculateOrder(final OrderModel order, final BigDecimal amountToSubtract)
	{

		BigDecimal sumSubstractValue = BigDecimal.ZERO;

		final BigDecimal orderSubTotal = BigDecimal.valueOf(order.getSubtotal());
		List<RecalculationEntry> recalEntries = new ArrayList<RecalculationEntry>();
		if (orderSubTotal.compareTo(amountToSubtract) >= 0)
		{
			for (AbstractOrderEntryModel entry : order.getEntries())
			{
				RecalculationEntry re = new RecalculationEntry();
				re.setOrderEntry(entry);
				re.setPercenatge(BigDecimal.valueOf(entry.getTotalPrice())
						.divide(BigDecimal.valueOf(order.getSubtotal()), 2, BigDecimal.ROUND_HALF_UP));
				recalEntries.add(re);
			}
			for (RecalculationEntry recalEntry : recalEntries)
			{
				final BigDecimal substractValue = amountToSubtract.multiply(recalEntry.getPercenatge());
				sumSubstractValue = sumSubstractValue.add(substractValue);
				if (sumSubstractValue.compareTo(orderSubTotal) < 0)
				{
					BigDecimal totalPrice = BigDecimal.valueOf(recalEntry.getOrderEntry().getTotalPrice());
					totalPrice = totalPrice.subtract(substractValue);
					if (totalPrice.compareTo(BigDecimal.ZERO) < 0)
					{
						totalPrice = BigDecimal.ZERO;
					}
					recalEntry.getOrderEntry().setTotalPrice(Double.valueOf(totalPrice.doubleValue()));
				}
				else
				{
					recalEntry.getOrderEntry().setTotalPrice(BigDecimal.ZERO.doubleValue());
				}
			}
			order.setCalculated(false);
			getOrderService().calculateOrder(order);
			getModelService().save(order);
		}
		return order;
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(ModelService modelService)
	{
		this.modelService = modelService;
	}

	public OrderService getOrderService()
	{
		return orderService;
	}

	public void setOrderService(OrderService orderService)
	{
		this.orderService = orderService;
	}

	private class RecalculationEntry
	{
		private AbstractOrderEntryModel orderEntry;
		private BigDecimal percenatge;

		public AbstractOrderEntryModel getOrderEntry()
		{
			return orderEntry;
		}

		public void setOrderEntry(AbstractOrderEntryModel orderEntry)
		{
			this.orderEntry = orderEntry;
		}

		public BigDecimal getPercenatge()
		{
			return percenatge;
		}

		public void setPercenatge(BigDecimal percenatge)
		{
			this.percenatge = percenatge;
		}
	}
}
