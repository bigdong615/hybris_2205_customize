package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.ConsignmentPopulator;
import de.hybris.platform.commercefacades.order.data.ConsignmentData;
import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import org.apache.log4j.Logger;


public class BlConsignmentPopulator extends ConsignmentPopulator
{
	private static final Logger LOG = Logger.getLogger(BlConsignmentPopulator.class);
	private Converter<DeliveryModeModel, DeliveryModeData> deliveryModeConverter;

	/**
	 * @return the deliveryModeConverter
	 */
	public Converter<DeliveryModeModel, DeliveryModeData> getDeliveryModeConverter()
	{
		return deliveryModeConverter;
	}

	/**
	 * @param deliveryModeConverter
	 *           the deliveryModeConverter to set
	 */
	public void setDeliveryModeConverter(final Converter<DeliveryModeModel, DeliveryModeData> deliveryModeConverter)
	{
		this.deliveryModeConverter = deliveryModeConverter;
	}

	@Override
	public void populate(final ConsignmentModel source, final ConsignmentData target) throws ConversionException
	{
		super.populate(source, target);
		if (source.getDeliveryMode() != null)
		{
			target.setDeliveryMode(getDeliveryModeConverter().convert(source.getDeliveryMode()));
		}
		target.setNameddeliverydate(source.getNamedDeliveryDate());
		target.setCarrier(source.getCarrier());
		if (source.getWarehouse() != null)
		{
			target.setWarehouseCode(source.getWarehouse().getCode());
		}
		if (source.getOrder() != null)
		{
			target.setOrderCode(source.getOrder().getCode());
		}
		target.setStatusDisplay(source.getStatusDisplay());
		//target.setCarrierDetails(source.getCarrierDetails());
		//target.setPackagingInfo(source.getPackagingInfo());
		target.setTaskAssignmentWorkflow(source.getTaskAssignmentWorkflow());
		target.setOptimizedShippingStartDate(source.getOptimizedShippingStartDate());
		target.setOptimizedshippingenddate(source.getOptimizedShippingEndDate());
		if (source.getOptimizedShippingType() != null)
		{
			target.setOptimizedshippingtype(source.getOptimizedShippingType().getCode());
		}
		if (source.getOptimizedShippingMethodType() != null)
		{
			target.setOptimizedShippingMethodType(source.getOptimizedShippingMethodType().getCode());
		}
		target.setActualShippingDateToCustomer(source.getActualShippingDateToCustomer());
		target.setOptimizedRentalEndDateChangedByJob(source.isOptimizedRentalEndDateChangedByJob());
		target.setOrdertransferconsignment(source.isOrderTransferConsignment());
		target.setCleanCompleteConsignment(source.isCleanCompleteConsignment());
		target.setThreeDayGroundAvailability(source.isThreeDayGroundAvailability());
		target.setInternalTransferConsignment(source.isInternalTransferConsignment());
		target.setShipmentBlShippedStatusDate(source.getShipmentBlShippedStatusDate());
		target.setRentalStartDate(source.getRentalStartDate());
		target.setRentalEndDate(source.getRentalEndDate());
		//target.setOrderNotes(source.getOrderNotes());
		target.setPrimaryKey(source.getPk().toString());
	}
}
