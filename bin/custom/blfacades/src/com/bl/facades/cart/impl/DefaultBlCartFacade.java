package com.bl.facades.cart.impl;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.AvailabilityMessage;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;

import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.impl.DefaultCartFacade;
import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Default implementation of the {@link BlCartFacade}.Delivers functionality for cart.
 *
 * @author Neeraj Singh
 */
public class DefaultBlCartFacade extends DefaultCartFacade implements BlCartFacade {

  private static final Logger LOGGER = Logger.getLogger(DefaultBlCartFacade.class);
  private BlCartService blCartService;
  
  private BlDatePickerService blDatePickerService;
  
  private BaseStoreService baseStoreService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeCartEntries() {

    getBlCartService().clearCartEntries();
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void resetCartCalculationFlag()
  {
	  getBlCartService().resetCartCalculationFlag();
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void recalculateCartIfRequired()
  {
	  getBlCartService().recalculateCartIfRequired();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateCartEntryDamageWaiver(final long entryNumber, final String damageWaiverType)
  {
	  getBlCartService().updateCartEntryDamageWaiver(entryNumber, damageWaiverType);
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void setRentalDatesOnCart(final Date rentalStartDate, final Date rentalEndDate)
  {
	  getBlCartService().setRentalDatesOnCart(rentalStartDate, rentalEndDate);
  }

  /**
   * {@inheritDoc}
   */
  public CartModificationData addToCart(final String productCode, final long quantity,
      final String serialCode)
      throws CommerceCartModificationException {

    BlSerialProductModel blSerialProductModel = null;
    BlProductModel blProductModel = (BlProductModel) getProductService()
        .getProductForCode(productCode);

    CartModel cartModel = blCartService.getSessionCart();
    final CommerceCartParameter parameter = new CommerceCartParameter();

    try {
      //For used gear product
      if (StringUtils.isNotEmpty(serialCode) && !StringUtils
          .equalsIgnoreCase(serialCode, BlFacadesConstants.SERIAL_CODE_MISSING) && CollectionUtils
          .isNotEmpty(blProductModel.getSerialProducts())) {
        for (final BlSerialProductModel blSerialProduct : blProductModel.getSerialProducts()) {
          if (blSerialProduct.getProductId().equals(serialCode)) {
            blSerialProductModel = blSerialProduct;
            parameter.setProduct(blSerialProductModel);
            parameter.setUnit(blSerialProductModel.getUnit());
            parameter.setCreateNewEntry(false);
            break;
          }
        }
      } else {
        //For rental product
        parameter.setProduct(blProductModel);
        parameter.setUnit(blProductModel.getUnit());
        parameter.setCreateNewEntry(true);
      }
    } catch (Exception exception) {
      BlLogger.logMessage(LOGGER, Level.ERROR,
          "Unable to set product model, unit and new entry to CommerceCartParameter", exception);
    }

    parameter.setEnableHooks(true);
    parameter.setCart(cartModel);
    parameter.setQuantity(quantity);

    final CommerceCartModification commerceCartModification = getCommerceCartService()
        .addToCart(parameter);
    setCartType(blSerialProductModel, cartModel, commerceCartModification);

    return getCartModificationConverter().convert(commerceCartModification);
  }

  /**
   * Set cart type rental or used gear based on product added to cart.
   * @param blSerialProductModel
   * @param cartModel
   * @param commerceCartModification
   */
  private void setCartType(final BlSerialProductModel blSerialProductModel,
      final CartModel cartModel,
      final CommerceCartModification commerceCartModification) {
    if (commerceCartModification != null && commerceCartModification.getStatusCode()
        .equals(BlFacadesConstants.SUCCESS)) {
      if (blSerialProductModel == null) {
        cartModel.setIsRentalCart(Boolean.TRUE);
      } else {
        cartModel.setIsRentalCart(Boolean.FALSE);
      }
    }
    getModelService().save(cartModel);
  }

    /**
   * {@inheritDoc}
   */
  @Override
  public boolean isRentalProductAddedToCartInUsedGearCart(final String productCode, final String serialCode) {

    boolean isAddToCartNotAllowed = false;
    CartModel cartModel = blCartService.getSessionCart();
    BlProductModel blProductModel = (BlProductModel) getProductService()
        .getProductForCode(productCode);
    BlSerialProductModel blSerialProductModel = null;

    if (StringUtils.isNotEmpty(serialCode) && !StringUtils
        .equalsIgnoreCase(serialCode, BlFacadesConstants.SERIAL_CODE_MISSING) && CollectionUtils
        .isNotEmpty(blProductModel.getSerialProducts())) {
      for (final BlSerialProductModel blSerialProduct : blProductModel.getSerialProducts()) {
        if (blSerialProduct.getProductId().equals(serialCode)) {
          blSerialProductModel = blSerialProduct;
          break;
        }
      }
    }

    if (cartModel != null && CollectionUtils.isNotEmpty(cartModel.getEntries())) {
      //It prevents user to add product to cart, if current cart is rental cart and user tries to add used gear product.
      if (Boolean.TRUE.equals(cartModel.getIsRentalCart())
          && blSerialProductModel != null) {
        isAddToCartNotAllowed = true;
      }

      //It prevents user to add product to cart, if current cart is used gear cart and user tries to add rental product.
      if (Boolean.FALSE.equals(cartModel.getIsRentalCart())
          && blSerialProductModel == null) {
        isAddToCartNotAllowed = true;
      }
    }
    return isAddToCartNotAllowed;
  }
  
  /**
	 * {@inheritDoc}
	 */
	@Override
	public boolean checkAvailabilityForRentalCart(final CartData cartData)
	{
		final AtomicBoolean enableContinueToCheckout = new AtomicBoolean(Boolean.TRUE);
		final RentalDateDto rentalDatesFromSession = getBlDatePickerService().getRentalDatesFromSession();
		if (BooleanUtils.isTrue(cartData.getIsRentalCart()) && Objects.nonNull(rentalDatesFromSession)
				&& CollectionUtils.isNotEmpty(cartData.getEntries()))
		{
			final List<WarehouseModel> warehouses = getBaseStoreService().getCurrentBaseStore().getWarehouses();
			final Map<String, Long> availabilityForRentalCart = getBlCartService().checkAvailabilityForRentalCart(cartData,
					warehouses, rentalDatesFromSession);
			cartData.getEntries().forEach(entry -> {
				final int cartEntryQty = entry.getQuantity().intValue();
				final String productCode = entry.getProduct().getCode();
				final int availableQty = availabilityForRentalCart.get(productCode).intValue();
				if (availableQty == 0)
				{
					final String nextAvailabilityDate = getBlCartService().getNextAvailabilityDate(productCode, rentalDatesFromSession,
							warehouses, cartEntryQty);
					if (StringUtils.isNotBlank(nextAvailabilityDate))
					{
						entry.setAvailabilityMessage(cartEntryQty > 1
								? getMessage("cart.entry.item.availability.qty.no.stock.available",
										Arrays.asList(String.valueOf(cartEntryQty), nextAvailabilityDate))
								: getMessage("cart.entry.item.availability.no.stock.available.till",
										Arrays.asList(nextAvailabilityDate)));
					}
					enableContinueToCheckout.set(Boolean.FALSE);
				}
				else if (BooleanUtils.negate(availableQty >= cartEntryQty))
				{
					entry.setAvailabilityMessage(
							getMessage("cart.entry.item.availability.low.stock.available", Arrays.asList(String.valueOf(availableQty))));
					enableContinueToCheckout.set(Boolean.FALSE);
				}
			});
		}
		return enableContinueToCheckout.get();
	}

	/**
	 * Gets the message object.
	 *
	 * @param messageCode
	 *           the message code
	 * @param arguments
	 *           the arguments
	 * @return the message
	 */
	private AvailabilityMessage getMessage(final String messageCode, final List<String> arguments)
	{
		final AvailabilityMessage message = new AvailabilityMessage();
		message.setMessageCode(messageCode);
		message.setArguments(arguments);
		return message;
	}

  /**
   * Gets the bl cart service.
   *
   * @return the bl cart service
   */
  public BlCartService getBlCartService() 
  {
    return blCartService;
  }

  /**
   * Sets the bl cart service.
   *
   * @param blCartService the new bl cart service
   */
  public void setBlCartService(final BlCartService blCartService) 
  {
    this.blCartService = blCartService;
  }

/**
 * @return the blDatePickerService
 */
public BlDatePickerService getBlDatePickerService()
{
	return blDatePickerService;
}

/**
 * @param blDatePickerService the blDatePickerService to set
 */
public void setBlDatePickerService(BlDatePickerService blDatePickerService)
{
	this.blDatePickerService = blDatePickerService;
}

/**
 * @return the baseStoreService
 */
public BaseStoreService getBaseStoreService()
{
	return baseStoreService;
}

/**
 * @param baseStoreService the baseStoreService to set
 */
public void setBaseStoreService(BaseStoreService baseStoreService)
{
	this.baseStoreService = baseStoreService;
}

}