package com.bl.core.services.strategy.impl;

import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationStatus;
import de.hybris.platform.commerceservices.strategies.impl.DefaultCartValidationStrategy;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.services.strategy.BlCartValidationStrategy;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;


/**
 * This class is use to validate cart before placing the order from review page.
 *
 * @author Ravikumar
 *
 */
public class DefaultBlCartValidationStrategy extends DefaultCartValidationStrategy implements BlCartValidationStrategy
{
	private static final Logger LOG = Logger.getLogger(DefaultBlCartValidationStrategy.class);
	private BlDatePickerService blDatePickerService;
	private BlCommerceStockService blCommerceStockService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected CommerceCartModification validateCartEntry(final CartModel cartModel, final CartEntryModel cartEntryModel)
	{
		// First verify that the product exists
		try
		{
			getProductService().getProductForCode(cartEntryModel.getProduct().getCode());
		}
		catch (final UnknownIdentifierException e)
		{
			final CommerceCartModification modification = new CommerceCartModification();
			modification.setStatusCode(CommerceCartModificationStatus.UNAVAILABLE);
			modification.setQuantityAdded(0);
			modification.setQuantity(0);

			final CartEntryModel entry = new CartEntryModel()
			{
				@Override
				public Double getBasePrice()
				{
					return null;
				}

				@Override
				public Double getTotalPrice()
				{
					return null;
				}
			};
			entry.setProduct(cartEntryModel.getProduct());

			modification.setEntry(entry);

			getModelService().remove(cartEntryModel);
			getModelService().refresh(cartModel);

			return modification;
		}

		// Stock quantity for this cartEntry
		final long cartEntryLevel = cartEntryModel.getQuantity().longValue();

		//Added condition for used gear product

		 if (BooleanUtils.isFalse(cartModel.getIsRentalCart()) || cartModel.isGiftCardOrder())
		{
			return returnSuccessModification(cartEntryModel, cartEntryLevel);
		}

		// Overall availability of this product
		final Long stockLevel = getStockLevel(cartEntryModel);

		// Overall stock quantity in the cart
		final long cartLevel = getCartLevel(cartEntryModel, cartModel);

		if (Objects.isNull(stockLevel) || stockLevel.intValue() < cartLevel)
		{
			final CommerceCartModification modification = new CommerceCartModification();
			modification.setStatusCode(CommerceCartModificationStatus.NO_STOCK);
			modification.setQuantityAdded(cartLevel);
			modification.setQuantity(cartEntryModel.getQuantity().longValue());
			modification.setEntry(cartEntryModel);
			return modification;
		}

		return returnSuccessModification(cartEntryModel, cartEntryLevel);

	}

	private CommerceCartModification returnSuccessModification(final CartEntryModel cartEntryModel, final long cartEntryLevel)
	{
		final CommerceCartModification modification = new CommerceCartModification();
		modification.setStatusCode(CommerceCartModificationStatus.SUCCESS);
		modification.setQuantityAdded(cartEntryLevel);
		modification.setQuantity(cartEntryLevel);
		modification.setEntry(cartEntryModel);

		return modification;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Long getStockLevel(final CartEntryModel cartEntryModel)
	{
		final RentalDateDto rentalDateDto = getBlDatePickerService().getRentalDatesFromSession();
		try
		{
			final DeliveryModeModel deliveryMode = cartEntryModel.getOrder().getDeliveryMode();
			if (Objects.nonNull(rentalDateDto) && deliveryMode instanceof ZoneDeliveryModeModel)
			{
				final List<WarehouseModel> listOfWarehouses = getListOfWarehouses(((ZoneDeliveryModeModel) deliveryMode));
				if (CollectionUtils.isEmpty(listOfWarehouses))
				{
					return Long.valueOf(0);
				}
				final Date startDay = BlDateTimeUtils.convertStringDateToDate(rentalDateDto.getSelectedFromDate(),
						BlCoreConstants.DATE_FORMAT);
				final Date endDay = BlDateTimeUtils.convertStringDateToDate(rentalDateDto.getSelectedToDate(),
						BlCoreConstants.DATE_FORMAT);
				return getBlCommerceStockService().getAvailableCount(cartEntryModel.getProduct().getCode(), listOfWarehouses,
						startDay, endDay);
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while getting stock availability for cart {} with product {}", cartEntryModel.getOrder().getCode(),
					cartEntryModel.getProduct().getCode());
		}
		return Long.valueOf(0);
	}

	@Override
	protected void validateDelivery(final CartModel cartModel) {
		if (cartModel.getDeliveryAddress() != null)
		{
			if (!isGuestUserCart(cartModel) && !getUserService().getCurrentUser().equals(cartModel.getUser()))
			{
				cartModel.setDeliveryAddress(null);
				getModelService().save(cartModel);
			}
		}
	}
	/**
	 * Gets the list of warehouses from the delivery mode or else from Base Store.
	 *
	 * @param deliveryMode
	 *           the delivery mode
	 * @return the list of warehouses
	 */
	private List<WarehouseModel> getListOfWarehouses(final ZoneDeliveryModeModel deliveryMode)
	{
		final List<WarehouseModel> listOfWarehouses = Lists.newArrayList();
		final WarehouseModel warehouse = deliveryMode.getWarehouse();
		if (Objects.nonNull(warehouse))
		{
			listOfWarehouses.add(warehouse);
		}
		else if (Objects.nonNull(getBaseStoreService().getCurrentBaseStore()))
		{
			listOfWarehouses.addAll(getBaseStoreService().getCurrentBaseStore().getWarehouses());
		}
		return listOfWarehouses;
	}

	/**
	 * This method added for keep delivery address for pick up
	 */
	@Override
	protected void validateDelivery(final CartModel cartModel)
	{
			if (Objects.nonNull(cartModel.getDeliveryAddress())
					&& !isGuestUserCart(cartModel) && !getUserService().getCurrentUser().equals(cartModel.getUser()))
			{
				cartModel.setDeliveryAddress(null);
				getModelService().save(cartModel);
			}
	}


	/**
	 * @return the blCommerceStockService
	 */
	public BlCommerceStockService getBlCommerceStockService()
	{
		return blCommerceStockService;
	}

	/**
	 * @param blCommerceStockService
	 *           the blCommerceStockService to set
	 */
	public void setBlCommerceStockService(final BlCommerceStockService blCommerceStockService)
	{
		this.blCommerceStockService = blCommerceStockService;
	}

	/**
	 * @return the blDatePickerService
	 */
	public BlDatePickerService getBlDatePickerService()
	{
		return blDatePickerService;
	}

	/**
	 * @param blDatePickerService
	 *           the blDatePickerService to set
	 */
	public void setBlDatePickerService(final BlDatePickerService blDatePickerService)
	{
		this.blDatePickerService = blDatePickerService;
	}


}
