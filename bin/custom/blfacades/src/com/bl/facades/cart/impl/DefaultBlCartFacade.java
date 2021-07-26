package com.bl.facades.cart.impl;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.AvailabilityMessage;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.data.AddToCartParams;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.order.impl.DefaultCartFacade;
import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Lists;

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
  
  private BlCommerceStockService blCommerceStockService;

	private Converter<AddToCartParams, CommerceCartParameter> commerceCartParameterConverter;

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
	 * It performs add to cart operation based on product type.
	 * @param productCode the product code
	 * @param quantity the quantity
	 * @param serialCode the serial code
	 * @return CartModificationData
	 * @throws CommerceCartModificationException the exception
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
				final Optional<BlSerialProductModel> blSerialProductModelOptional = blProductModel
						.getSerialProducts().stream()
						.filter(blSerialProduct -> blSerialProduct.getProductId().equals(serialCode))
						.findFirst();
				if (blSerialProductModelOptional.isPresent()) {
					blSerialProductModel = blSerialProductModelOptional.get();
					parameter.setProduct(blSerialProductModel);
					parameter.setIsNoDamageWaiverSelected(Boolean.TRUE);
					parameter.setIsDamageWaiverProSelected(Boolean.FALSE);
					parameter.setIsDamageWaiverSelected(Boolean.FALSE);
					parameter.setUnit(blSerialProductModel.getUnit());
					parameter.setCreateNewEntry(false);
				}
			} else {
        //For rental product
        parameter.setProduct(blProductModel);
        parameter.setUnit(blProductModel.getUnit());
        parameter.setCreateNewEntry(false);
      }
    } catch (final RuntimeException exception) {
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
          //Added code for serial status changes
		  blSerialProductModel.setSerialStatus(SerialStatusEnum.ADDED_TO_CART);
		  getModelService().save(blSerialProductModel);
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
    BlSerialProductModel blSerialProductModel = getBlSerialProductModel(serialCode, blProductModel);

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
	 * It initialize blSerialProductModel is case of used gear product added to cart.
	 *
	 * @param serialCode     the serial code
	 * @param blProductModel the BlProductModel
	 * @return BlSerialProductModel
	 */
	private BlSerialProductModel getBlSerialProductModel(final String serialCode,
			final BlProductModel blProductModel) {
		if (StringUtils.isNotEmpty(serialCode) && !StringUtils
				.equalsIgnoreCase(serialCode, BlFacadesConstants.SERIAL_CODE_MISSING) && CollectionUtils
				.isNotEmpty(blProductModel.getSerialProducts())) {
			final Optional<BlSerialProductModel> blSerialProductModelOptional = blProductModel
					.getSerialProducts().stream()
					.filter(blSerialProduct -> blSerialProduct.getProductId().equals(serialCode)).findFirst();
			if (blSerialProductModelOptional.isPresent()) {
				return blSerialProductModelOptional.get();
			}
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void checkAvailabilityForRentalCart(final CartData cartData)
	{
		try
		{
			final RentalDateDto rentalDatesFromSession = getBlDatePickerService().getRentalDatesFromSession();
			if (BooleanUtils.isTrue(cartData.getIsRentalCart()) && Objects.nonNull(rentalDatesFromSession)
					&& CollectionUtils.isNotEmpty(cartData.getEntries()))
			{
				final List<WarehouseModel> warehouses = getBaseStoreService().getCurrentBaseStore().getWarehouses();
				final Map<String, Long> availabilityForRentalCart = getBlCartService().getAvailabilityForRentalCart(cartData,
						warehouses, rentalDatesFromSession);
				cartData.getEntries().forEach(entry -> {
					final int cartEntryQty = entry.getQuantity().intValue();
					final String productCode = entry.getProduct().getCode();
					final int availableQty = availabilityForRentalCart.get(productCode).intValue();
					if (availableQty == 0)
					{
						final String nextAvailabilityDate = getBlCommerceStockService().getNextAvailabilityDateInCheckout(productCode,
								rentalDatesFromSession, warehouses, cartEntryQty);
						setNextAvailableDateToCartEntry(entry, cartEntryQty, nextAvailabilityDate);
					}
					else if (BooleanUtils.negate(availableQty >= cartEntryQty))
					{
						entry.setAvailabilityMessage(getMessage("cart.entry.item.availability.low.stock.available",
								Arrays.asList(String.valueOf(availableQty))));
					}
				});
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOGGER, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while checking next availability for cart - {}", cartData.getCode());
			final AvailabilityMessage productUnavailableMessage = getMessage("text.stock.not.available", Lists.emptyList());
			cartData.getEntries().forEach(cartEntry -> cartEntry.setAvailabilityMessage(productUnavailableMessage));
		}
	}

	/**
	 * Sets the next available date with message to cart entry.
	 *
	 * @param entry
	 *           the entry
	 * @param cartEntryQty
	 *           the cart entry qty
	 * @param nextAvailabilityDate
	 *           the next availability date
	 */
	private void setNextAvailableDateToCartEntry(final OrderEntryData entry, final int cartEntryQty,
			final String nextAvailabilityDate)
	{
		if (StringUtils.isNotBlank(nextAvailabilityDate))
		{
			entry.setAvailabilityMessage(cartEntryQty > 1
					? getMessage("cart.entry.item.availability.qty.no.stock.available",
							Arrays.asList(String.valueOf(cartEntryQty), nextAvailabilityDate))
					: getMessage("cart.entry.item.availability.no.stock.available.till", Arrays.asList(nextAvailabilityDate)));
		}
	}

	/**
	 * Gets the availability message object by setting message and list of arguments.
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
	 * {@inheritDoc}
	 */
	@Override
	public CartModificationData updateCartEntryFromPopup(final long entryNumber, final long quantity)
			throws CommerceCartModificationException
	{
		final AddToCartParams dto = new AddToCartParams();
		dto.setQuantity(quantity);
		final CommerceCartParameter parameter = getCommerceCartParameterConverter().convert(dto);
		parameter.setEnableHooks(true);
		parameter.setEntryNumber(entryNumber);
		parameter.setIsFromAddToCartPopup(true);

		final CommerceCartModification modification = getCommerceCartService().updateQuantityForCartEntry(parameter);

		return getCartModificationConverter().convert(modification);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean checkAvailabilityOnCartContinue(final RentalDateDto sessionRentalDate)
	{
		final AtomicBoolean isAvailable = new AtomicBoolean(Boolean.TRUE);
		final CartModel cartModel = getBlCartService().getSessionCart();
		if (Objects.nonNull(sessionRentalDate) && Objects.nonNull(cartModel) && CollectionUtils.isNotEmpty(cartModel.getEntries()))
		{
			final Date startDay = BlDateTimeUtils.getDate(sessionRentalDate.getSelectedFromDate(), BlFacadesConstants.DATE_FORMAT);
			final Date endDay = BlDateTimeUtils.getDate(sessionRentalDate.getSelectedToDate(), BlFacadesConstants.DATE_FORMAT);
			final List<String> listOfProductCodes = cartModel.getEntries().stream()
					.map(cartEntry -> cartEntry.getProduct().getCode()).collect(Collectors.toList());
			final Map<String, Long> groupByProductsAvailability = getBlCommerceStockService().groupByProductsAvailability(startDay,
					endDay, listOfProductCodes, getBaseStoreService().getCurrentBaseStore().getWarehouses());
			cartModel.getEntries().forEach(cartEntry -> {
				final int cartQuantity = cartEntry.getQuantity().intValue();
				final int availableStockQuantity = groupByProductsAvailability.get(cartEntry.getProduct().getCode()).intValue();
				if (availableStockQuantity < cartQuantity)
				{
					isAvailable.set(Boolean.FALSE);
					return;
				}
			});
			return isAvailable.get();
		}
		return isAvailable.get();
	}

	/**
	 * @{InheritDoc }
	 */
	@Override
	public String identifyCartType() {
		final CartModel cartModel = blCartService.getSessionCart();
		if (CollectionUtils
				.isNotEmpty(cartModel.getEntries()) && Boolean.TRUE.equals(cartModel.getIsRentalCart())) {
			return BlFacadesConstants.RENTAL_CART;
		} else if (CollectionUtils
				.isNotEmpty(cartModel.getEntries()) && Boolean.FALSE.equals(cartModel.getIsRentalCart())) {
			return BlFacadesConstants.USED_GEAR_CART;
		} else if (CollectionUtils
				.isEmpty(cartModel.getEntries())) {
			return BlFacadesConstants.RENTAL_OR_USED_GEAR_PRODUCT_ALLOWED;
		}
		return null;
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

/**
 * @return the blCommerceStockService
 */
public BlCommerceStockService getBlCommerceStockService()
{
	return blCommerceStockService;
}

/**
 * @param blCommerceStockService the blCommerceStockService to set
 */
public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService)
{
	this.blCommerceStockService = blCommerceStockService;
}

	/**
	 * This method is used for remove discontinue product from cart.
	 * @param cartModel
	 * @param isCartPage
	 */
  @Override
  public String removeDiscontinueProductFromCart(final CartModel cartModel,final boolean isCartPage) {
    StringBuilder removedEntry = new StringBuilder();
    List<Integer> entryList = getDiscontinueEntryList(cartModel,removedEntry);
    if (CollectionUtils.isNotEmpty(entryList)) {
      Collections.reverse(entryList);
      entryList.forEach(entryNumber -> {
        try {
          if (isCartPage) {
            updateCartEntry(entryNumber, 0);
          } else {
            updateCartEntry(entryNumber, 0, cartModel);
          }
        } catch (CommerceCartModificationException ex) {
          BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR,
              "Couldn't update product with the entry number: {0} . {1}", entryNumber, ex);
        }
      });
    }
    String removedEntries = removedEntry.toString();
		if(StringUtils.isNotEmpty(removedEntries)) {
			removedEntries = removedEntries.substring(1);
		}
   return removedEntries;
  }

	/**
	 * This method used for pre-populating saved card data before removing its discontinue entry.
	 * @param entryNumber
	 * @param quantity
	 * @param cartModel
	 * @return
	 * @throws CommerceCartModificationException
	 */
	@Override
	public CartModificationData updateCartEntry(final long entryNumber, final long quantity,final CartModel cartModel)
			throws CommerceCartModificationException
	{
		final CommerceCartParameter parameter = new CommerceCartParameter();
		parameter.setCart(cartModel);
		parameter.setQuantity(quantity);
		parameter.setCreateNewEntry(false);
		parameter.setEnableHooks(true);
		parameter.setEnableHooks(true);
		parameter.setEntryNumber(entryNumber);
		final CommerceCartModification modification = getCommerceCartService().updateQuantityForCartEntry(parameter);
		return getCartModificationConverter().convert(modification);
	}

	@Resource(name = "i18nService")
	private I18NService i18nService;
	/**
	 *  This method used for collecting discontinue entries number form cart.
	 * @param cartModel
	 * @param removedEntry
	 */
	@Override
	public List<Integer> getDiscontinueEntryList(final CartModel cartModel, StringBuilder removedEntry){
		List<Integer> entryList = new ArrayList<>();
		cartModel.getEntries().forEach(entry -> {
			if (entry.getProduct() != null) {
				final BlProductModel blProductModel = (BlProductModel) entry.getProduct();
				if (org.apache.commons.lang.BooleanUtils.isTrue(blProductModel.getDiscontinued())) {
					entryList.add(entry.getEntryNumber());
					removedEntry.append(BlFacadesConstants.COMMA_SEPERATER).append(blProductModel.getName(
							i18nService.getCurrentLocale()));
				}
			}
		});
		return entryList;
	}

	/**
	 *{@inheritDoc}
	 */
	@Override
	public void savePoPaymentDetails(final String poNumber, final String poNotes) {
    blCartService.savePoPaymentDetails(poNumber,poNotes);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removePoNumber() {
		CartModel cartModel = blCartService.getSessionCart();
		if (cartModel != null) {
			try {
				cartModel.setPoNumber(null);
				getModelService().save(cartModel);
				getModelService().refresh(cartModel);
			} catch (final ModelSavingException exception) {
				BlLogger
						.logMessage(LOGGER, Level.ERROR, "Error occurred while updating po number", exception);
			}
		}
	}

	@Override
	public Converter<AddToCartParams, CommerceCartParameter> getCommerceCartParameterConverter() {
		return commerceCartParameterConverter;
	}

	@Override
	public void setCommerceCartParameterConverter(
			Converter<AddToCartParams, CommerceCartParameter> commerceCartParameterConverter) {
		this.commerceCartParameterConverter = commerceCartParameterConverter;
	}
}