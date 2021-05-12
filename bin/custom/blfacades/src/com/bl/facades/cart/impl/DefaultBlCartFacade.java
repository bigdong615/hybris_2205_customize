package com.bl.facades.cart.impl;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.facades.cart.BlCartFacade;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.impl.DefaultCartFacade;
import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;
import org.apache.commons.collections4.CollectionUtils;
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
  public static final String SERIAL_CODE_MISSING = "serialCodeNotPresent";

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
  public CartModificationData addToCart(final String productCode, final long quantity,
      final String serialCode)
      throws CommerceCartModificationException {

    BlSerialProductModel blSerialProductModel = null;
    BlProductModel blProductModel = (BlProductModel) getProductService()
        .getProductForCode(productCode);

    CartModel cartModel = blCartService.getSessionCart();
    final CommerceCartParameter parameter = new CommerceCartParameter();

    try {
      if (StringUtils.isNotEmpty(serialCode) && !StringUtils
          .equalsIgnoreCase(serialCode, SERIAL_CODE_MISSING) && CollectionUtils
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
        parameter.setProduct(blProductModel);
        parameter.setUnit(blProductModel.getUnit());
        parameter.setCreateNewEntry(true);
      }
    } catch (Exception exception) {
      BlLogger.logMessage(LOGGER, Level.DEBUG,
          "Unable to set product model, unit and new entry to CommerceCartParameter",exception);
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
        .equals("success") && blSerialProductModel == null) {
      cartModel.setIsRentalCart(Boolean.TRUE);
    }
    if (commerceCartModification != null && commerceCartModification.getStatusCode()
        .equals("success") && blSerialProductModel != null) {
      cartModel.setIsRentalCart(Boolean.FALSE);
    }
    getModelService().save(cartModel);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isRentalAndUsedProduct(final String productCode, final String serialCode) {

    boolean isAddToCartNotAllowed = false;
    CartModel cartModel = blCartService.getSessionCart();
    BlProductModel blProductModel = (BlProductModel) getProductService()
        .getProductForCode(productCode);
    BlSerialProductModel blSerialProductModel = null;

    if (StringUtils.isNotEmpty(serialCode) && !StringUtils
        .equalsIgnoreCase(serialCode, SERIAL_CODE_MISSING) && CollectionUtils
        .isNotEmpty(blProductModel.getSerialProducts())) {
      for (final BlSerialProductModel blSerialProduct : blProductModel.getSerialProducts()) {
        if (blSerialProduct.getProductId().equals(serialCode)) {
          blSerialProductModel = blSerialProduct;
          break;
        }
      }
    }

    if (cartModel != null && CollectionUtils.isNotEmpty(cartModel.getEntries())) {
      if (Boolean.TRUE.equals(cartModel.getIsRentalCart())
          && blSerialProductModel != null) {
        isAddToCartNotAllowed = true;
      }

      if (Boolean.FALSE.equals(cartModel.getIsRentalCart())
          && blSerialProductModel == null) {
        isAddToCartNotAllowed = true;
      }
    }
    return isAddToCartNotAllowed;
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

}