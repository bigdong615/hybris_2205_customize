package com.bl.core.order.populators;


import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.ReturnOrderData;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.text.SimpleDateFormat;
import java.util.List;

public class BlReturnOrderPopulator implements Populator<List<OrderModel>, List<ReturnOrderData>>
{
	@Override
	public void populate(final List<OrderModel> source, final List<ReturnOrderData> target) throws ConversionException
	{
		final SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
		for(final OrderModel order : source)
		{
			final ReturnOrderData orderData = new ReturnOrderData();
			orderData.setCustomerName(order.getUser().getName());
			orderData.setEmail(order.getUser().getUid());
			orderData.setOrderNumber(order.getOrderID());
			if(order.getCreationtime()!=null) {
				orderData.setOrderPlacedDate(formatter.format(order.getCreationtime()));
			}
			if(order.getRentalEndDate()!=null) {
				orderData.setReturnOrderDate(formatter.format(order.getRentalEndDate()));
			}
			if(order.getActualRentalEndDate()!=null)
			{
				orderData.setActualReturnOrderDate(formatter.format(order.getActualRentalEndDate()));
			}
			target.add(orderData);
		}
	}

	/**
	 * @param string
	 * @return
	 */
	private Object SimpleDateFormat(final String string)
	{
		// XXX Auto-generated method stub
		return null;
	}

}
