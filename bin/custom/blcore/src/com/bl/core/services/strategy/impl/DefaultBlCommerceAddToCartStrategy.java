package com.bl.core.services.strategy.impl;

import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.order.CommerceCartModificationStatus;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceAddToCartStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.storelocator.model.PointOfServiceModel;
import org.apache.commons.lang.BooleanUtils;

/**
 * This class overrides the OOB add to cart implementation to override the OOB availability logic
 * @author Neeraj Singh
 */
public class DefaultBlCommerceAddToCartStrategy extends
    DefaultCommerceAddToCartStrategy {

  private static final long DEFAULT_STOCK_QUANTITY = 1L;

  /**
   * {@inheritDoc}
   */
  @Override
  protected CommerceCartModification doAddToCart(final CommerceCartParameter parameter)
      throws CommerceCartModificationException {
    CommerceCartModification modification;

    final CartModel cartModel = parameter.getCart();
    final ProductModel productModel = parameter.getProduct();
    final long quantityToAdd = parameter.getQuantity();
    final PointOfServiceModel deliveryPointOfService = parameter.getPointOfService();

    this.beforeAddToCart(parameter);
    validateAddToCart(parameter);

    if (isProductForCode(parameter).booleanValue()) {
      // So now work out what the maximum allowed to be added is (note that this may be negative!)
      final long actualAllowedQuantityChange = getAllowedQuantityChange(cartModel, productModel,
          quantityToAdd, deliveryPointOfService);
      final Integer maxOrderQuantity = productModel.getMaxOrderQuantity();
      final long cartLevel = checkCartLevel(productModel, cartModel, deliveryPointOfService);
      final long cartLevelAfterQuantityChange = actualAllowedQuantityChange + cartLevel;

      if (actualAllowedQuantityChange > 0) {
        // We are allowed to add items to the cart
        final CartEntryModel entryModel = addCartEntry(parameter, actualAllowedQuantityChange);

        // To update the Damage waiver same as existing order for extend rental
        if(BooleanUtils.isTrue(parameter.getIsFromRentAgainPage())) {
          entryModel.setGearGuardProFullWaiverSelected(parameter.getIsDamageWaiverProSelected());
          entryModel.setGearGuardWaiverSelected(parameter.getIsDamageWaiverSelected());
          entryModel.setNoDamageWaiverSelected(parameter.getIsNODamageWaiverSelected());
        }
        getModelService().save(entryModel);

        final String statusCode = getStatusCodeAllowedQuantityChange(actualAllowedQuantityChange,
            maxOrderQuantity,
            quantityToAdd, cartLevelAfterQuantityChange);

        modification = createAddToCartResp(parameter, statusCode, entryModel,
            actualAllowedQuantityChange);
      } else {
        // Not allowed to add any quantity, or maybe even asked to reduce the quantity
        // Do nothing!
        //This code can be needed for used gear add to cart implementation.
        final String status = getStatusCodeForNotAllowedQuantityChange(maxOrderQuantity,
            maxOrderQuantity);

        modification = createAddToCartResp(parameter, status, createEmptyCartEntry(parameter), 0);

      }
    } else {
      modification = createAddToCartResp(parameter, CommerceCartModificationStatus.UNAVAILABLE,
          createEmptyCartEntry(parameter), 0);
    }

    return modification;
  }

  /**
   * Not checking product stock  for add to cart. Availability will be checked while redirecting to
   * cart page.
   *
   * @param cartModel
   * @param productModel
   * @param quantityToAdd
   * @param deliveryPointOfService
   * @return stock quantity
   */
  private long getAllowedQuantityChange(final CartModel cartModel, final ProductModel productModel,
      final long quantityToAdd, final PointOfServiceModel deliveryPointOfService) {

    //based on used gear add to cart below commented code need to update/remove.
    /*if(productModel instanceof BlSerialProductModel) { //NOSONAR
      return getAllowedCartAdjustmentForProduct(cartModel, productModel, quantityToAdd,
          deliveryPointOfService);
    }*/ //NOSONAR

    return DEFAULT_STOCK_QUANTITY;
  }

}
