/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.commercefacades.order.data.ConsignmentData;
import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import de.hybris.platform.commercefacades.storelocator.data.PointOfServiceData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.storelocator.model.PointOfServiceModel;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.util.Assert;

import com.bl.logging.BlLogger;


/**
 * @author kumar
 *
 */
public class BlDomoConsignmentPopulator implements Populator<ConsignmentModel, ConsignmentData>
{
	private static final Logger LOG = Logger.getLogger(BlConsignmentPopulator.class);
	private Converter<DeliveryModeModel, DeliveryModeData> deliveryModeConverter;
	private Converter<PointOfServiceModel, PointOfServiceData> pointOfServiceConverter;
	private Converter<AddressModel, AddressData> addressConverter;

	@Override
	public void populate(final ConsignmentModel source, final ConsignmentData target) throws ConversionException
	{
		Assert.notNull(source, "Parameter source cannot be null.");
		Assert.notNull(target, "Parameter target cannot be null.");
		try
		{
		target.setCode(source.getCode());
		target.setTrackingID(source.getTrackingID());
		target.setStatus(source.getStatus());
		if (ConsignmentStatus.SHIPPED.equals(source.getStatus()) || ConsignmentStatus.READY_FOR_PICKUP.equals(source.getStatus()))
		{
			target.setStatusDate(source.getShippingDate());
		}
		if (source.getDeliveryPointOfService() != null)
		{
			target.setDeliveryPointOfService(getPointOfServiceConverter().convert(source.getDeliveryPointOfService()));
		}
		if (source.getShippingAddress() != null)
		{
			target.setShippingAddress(getAddressConverter().convert(source.getShippingAddress()));
		}
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
		target.setPrimaryKey(source.getPk().toString());
		target.setPicker(source.getPicker());
	}
	catch (final Exception exception)
	{
		LOG.info("Error while getting Consignment for PK " + source.getPk().toString());
		BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting Consignment", exception);
		exception.printStackTrace();

	}
		//		String entriesPk = null;
		//		if (source.getConsignmentEntries() != null)
		//		{
		//			for (final ConsignmentEntryModel consignmentEntryModel : source.getConsignmentEntries())
		//			{
		//				entriesPk += consignmentEntryModel.getPk().toString();
		//			}
		//		}
		//		target.setEntriesPk(entriesPk);
	}

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

	protected Converter<PointOfServiceModel, PointOfServiceData> getPointOfServiceConverter()
	{
		return pointOfServiceConverter;
	}

	@Required
	public void setPointOfServiceConverter(final Converter<PointOfServiceModel, PointOfServiceData> pointOfServiceConverter)
	{
		this.pointOfServiceConverter = pointOfServiceConverter;
	}

	protected Converter<AddressModel, AddressData> getAddressConverter()
	{
		return addressConverter;
	}

	@Required
	public void setAddressConverter(final Converter<AddressModel, AddressData> addressConverter)
	{
		this.addressConverter = addressConverter;
	}

}
