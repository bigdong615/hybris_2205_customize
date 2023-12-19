/**
 *
 */
package com.bl.core.order.populators;

import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.text.SimpleDateFormat;
import java.util.List;


/**
 * @author kumar
 *
 */
public class BlCompletedOrderPopulator implements Populator<List<OrderModel>, List<OrderData>>
{
	final SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");


	@Override
	public void populate(final List<OrderModel> source, final List<OrderData> target) throws ConversionException
	{
		for (final OrderModel order : source)
		{
			for (final AbstractOrderEntryModel orderentry : order.getEntries())
			{
				final OrderData orderData = new OrderData();

				orderData.setCode(order.getCode());
				orderData.setPage_id(orderentry.getProduct().getCode());
				orderData.setOrder_id(order.getCode());
				if (order.getPaymentAddress() != null)
				{
					orderData.setFirst_name(order.getPaymentAddress().getFirstname());
					orderData.setLast_name(order.getPaymentAddress().getLastname());
				}
				orderData.setEmail(order.getUser().getUid());
				orderData.setOrder_date(formatter.format(order.getCreationtime()));
				target.add(orderData);

			}
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
