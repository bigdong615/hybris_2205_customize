/*
 * [y] hybris Platform
 *
 * Copyright (c) 2018 SAP SE or an SAP affiliate company.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of SAP
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with SAP.
 *
 */
package com.bl.Ordermanagement.actions.order;

import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.storelocator.GPS;
import de.hybris.platform.storelocator.GeoWebServiceWrapper;
import de.hybris.platform.storelocator.data.AddressData;
import de.hybris.platform.storelocator.exception.GeoServiceWrapperException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Required;

/**
 * Used to get an address from order.deliveryAddress and find the closest store populating back the
 * delivery latitude/longitude.
 */
public class GeocodeShippingAddressAction extends AbstractProceduralAction<OrderProcessModel>
{
	private static final Logger LOG = LoggerFactory.getLogger(GeocodeShippingAddressAction.class);

	private GeoWebServiceWrapper geoWebServiceWrapper;
	private Converter<AddressModel, AddressData> addressConverter;

	@Override
	public void executeAction(final OrderProcessModel orderProcessModel)
	{
		LOG.info("Process: {} in step {}", orderProcessModel.getCode(), getClass().getSimpleName());

		final OrderModel order = orderProcessModel.getOrder();
		try
		{
			if (!(order.getDeliveryMode() instanceof BlPickUpZoneDeliveryModeModel))
			{
				LOG.debug("Getting GPS from delivery address...");
				final GPS gps = getGeoWebServiceWrapper().geocodeAddress(
						getAddressConverter().convert(order.getDeliveryAddress()));

				if (LOG.isInfoEnabled())
				{
					LOG.info(String.format("Setting up latitude: %1$,.6f and longitude: %2$,.6f to the order",
							gps.getDecimalLatitude(), gps.getDecimalLongitude()));

				}
				final AddressModel deliveryAddress = order.getDeliveryAddress();

				if (deliveryAddress != null)
				{
					deliveryAddress.setLatitude(gps.getDecimalLatitude());
					deliveryAddress.setLongitude(gps.getDecimalLongitude());
				}
				getModelService().save(deliveryAddress);

			}
		}
		catch (final ConversionException | GeoServiceWrapperException e)  //NOSONAR
		{
			LOG.info("Fail to obtain geocode from order.deliveryAddress, error message: " + e.getMessage()); //NOSONAR
		}
	}

	@Required
	public void setGeoWebServiceWrapper(final GeoWebServiceWrapper geoWebServiceWrapper)
	{
		this.geoWebServiceWrapper = geoWebServiceWrapper;
	}

	@Required
	public void setAddressConverter(final Converter<AddressModel, AddressData> addressConverter)
	{
		this.addressConverter = addressConverter;
	}

	protected GeoWebServiceWrapper getGeoWebServiceWrapper()
	{
		return geoWebServiceWrapper;
	}

	protected Converter<AddressModel, AddressData> getAddressConverter()
	{
		return addressConverter;
	}
}
