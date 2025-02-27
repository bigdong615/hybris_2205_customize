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

import org.apache.log4j.Logger;


/**
 * @author kumar
 *
 */
public class BlCompletedOrderPopulator implements Populator<List<OrderModel>, List<OrderData>>
{
	final SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd");
	public static final String LOCALE_CONSTANT = "en_US";
	private static final Logger LOG = Logger.getLogger(BlCompletedOrderPopulator.class);


	@Override
	public void populate(final List<OrderModel> source, final List<OrderData> target) throws ConversionException
	{
		for (final OrderModel order : source)
		{
			try
			{
				if (order.getPaymentAddress() != null)
				{
					if (order.getUser().getName() != null || order.getPaymentAddress().getFirstname() != null)
					{
						for (final AbstractOrderEntryModel orderentry : order.getEntries())
						{
							if (orderentry.getProduct() != null)
							{
								final OrderData orderData = new OrderData();
								orderData.setCode(order.getCode());
								if (orderentry.getProduct() != null)
								{
									orderData.setPage_id(orderentry.getProduct().getCode());
								}
								orderData.setOrder_id(order.getCode());
								if (order.getPaymentAddress() != null)
								{
									if (order.getPaymentAddress().getFirstname() != null)
									{
										orderData.setFirst_name(order.getPaymentAddress().getFirstname());
									}
									else
									{
										orderData.setFirst_name(order.getUser().getName());
									}
									if (order.getPaymentAddress().getLastname() != null)
									{
										orderData.setLast_name(order.getPaymentAddress().getLastname());
									}
									else
									{
										orderData.setLast_name(order.getUser().getName());
									}
								}
								orderData.setEmail(order.getUser().getUid());
								orderData.setOrder_date(formatter.format(order.getCreationtime()));
								orderData.setLocale(LOCALE_CONSTANT);
								target.add(orderData);
							}
						}
					}
				}
			}
			catch (final Exception exception)
			{
				LOG.debug("Error while completed order export for order number " + order.getCode());
				exception.printStackTrace();
			}
		}
	}
}
