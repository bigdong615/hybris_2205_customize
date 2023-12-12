/**
 *
 */
package com.bl.core.order.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.order.UpsScrapeOrderData;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Objects;

public class BlUpsScrapeOrderPopulator implements Populator<List<AbstractOrderModel>, List<UpsScrapeOrderData>>
{
	 @Override
	 public void populate(final List<AbstractOrderModel> source, final List<UpsScrapeOrderData> target) throws ConversionException
	 {
		 final SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
		 for (final AbstractOrderModel order : source)
		 {
			final UpsScrapeOrderData orderData = new UpsScrapeOrderData();
			orderData.setSubscriberId(order.getUser().getUid());
			orderData.setEmailAddress(order.getUser().getUid());
			orderData.setOrderNumber(order.getCode());
			if (order.getUser().getName().equals("THE UPS STORE"))
			{
			    orderData.setCustomerName("BorrowLenses Customer");
			}else {
				orderData.setCustomerName(order.getUser().getName());
			}
			if (Objects.nonNull(order.getDeliveryMode())) {
				final ZoneDeliveryModeModel zoneDeliveryModeModel = ((ZoneDeliveryModeModel) order
						.getDeliveryMode());
				zoneDeliveryModeModel.getShippingGroup().getName();
				orderData.setShippingMethod(zoneDeliveryModeModel.getCode());
			}
			orderData.setRentalStartDate(formatter.format(order.getRentalStartDate()));
			orderData.setRentalEndDate(formatter.format(order.getRentalEndDate()));
			target.add(orderData);
		}
	}
 }
