package com.bl.core.order.populators;


import com.bl.core.utils.BlDateTimeUtils;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.order.LateOrderData;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.List;
import java.util.Objects;

public class BlLateOrderPopulator implements Populator<List<AbstractOrderModel>, List<LateOrderData>>
{
	@Override
	public void populate(final List<AbstractOrderModel> source, final List<LateOrderData> target) throws ConversionException {
		final SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy  HH:mm:ss");
		for (final AbstractOrderModel order : source) {
			final LateOrderData orderData = new LateOrderData();
			orderData.setSubscriberId(order.getUser().getUid());
            if(order.getUser().getName().equals("THE UPS STORE")){
			    orderData.setCustomerName("BorrowLenses Customer");
			}else {
				orderData.setCustomerName(order.getUser().getName());
			}
			orderData.setEmailAddress(order.getUser().getUid());
			orderData.setOrderNumber(order.getCode());
			orderData.setRentalStartDate(formatter.format(order.getRentalStartDate()));
			orderData.setRentalEndDate(formatter.format(order.getRentalEndDate()));
			if (Objects.nonNull(order.getDeliveryMode())) {
				final ZoneDeliveryModeModel zoneDeliveryModeModel = ((ZoneDeliveryModeModel) order
						.getDeliveryMode());
				orderData.setShippingMethodType(
						zoneDeliveryModeModel.getShippingGroup().getName());
				orderData.setShippingMethod(zoneDeliveryModeModel.getCode());
			}
			for (final ConsignmentModel consignment : order.getConsignments()) {
				Date optimizedEndDate = consignment.getOptimizedShippingEndDate();
				orderData.setOptimizedReturnDate(formatter.format(consignment.getOptimizedShippingEndDate()));
				orderData.setDaysLate((int) BlDateTimeUtils
						.getDaysBetweenDates( optimizedEndDate,new Date()));

			}
			target.add(orderData);

		}
	}

}
