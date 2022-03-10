package com.bl.core.services.strategy.impl;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.order.CommerceCartModificationStatus;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceAddToCartStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.storelocator.model.PointOfServiceModel;
import java.util.Objects;
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
          quantityToAdd, deliveryPointOfService,parameter);
      final Integer maxOrderQuantity = productModel.getMaxOrderQuantity();
      final long cartLevel = checkCartLevel(productModel, cartModel, deliveryPointOfService);
      final long cartLevelAfterQuantityChange = actualAllowedQuantityChange + cartLevel;

      if (actualAllowedQuantityChange > 0) {
        // We are allowed to add items to the cart
        final CartEntryModel entryModel = addCartEntry(parameter, actualAllowedQuantityChange);
        // To update the Damage waiver same as existing order for extend rental
        // For used gear damage waiver don't need to set.
        if (Objects.nonNull(entryModel) && (entryModel.getProduct() instanceof BlSerialProductModel
            || BooleanUtils.isTrue(parameter.getIsFromRentAgainPage()))) {
          setDamageWaiverOptions(entryModel, parameter.getIsDamageWaiverProSelected(),
              parameter.getIsDamageWaiverSelected(), parameter.getIsNoDamageWaiverSelected());
        }
        // Set gift card purchase values to entry model
        if(Objects.nonNull(entryModel) && Objects.nonNull(parameter.getGiftCardAmount())){
      	  entryModel.setRecipientEmail(parameter.getRecipientEmail());
      	  entryModel.setRecipientName(parameter.getRecipientName());
      	  entryModel.setRecipientMessage(parameter.getRecipientMessage());
        }
        if(BooleanUtils.isTrue(parameter.getRetailGear())){
          entryModel.getOrder().setIsRetailGearOrder(true);
          entryModel.getOrder().setIsRentalOrder(Boolean.FALSE);
          entryModel.getOrder().setRentalStartDate(null);
          entryModel.getOrder().setRentalEndDate(null);
        }
        entryModel.setBundleMainEntry(parameter.isBundleMainEntry());
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
   * Setting damage waiver options in entry model.
   *
   * @param entryModel the CartEntryModel
   * @param isGearGuardProFullWaiverSelected the isGearGuardProFullWaiverSelected
   * @param isGearGuardWaiverSelected the isGearGuardWaiverSelected
   * @param isNoDamageWaiverSelected the isNoDamageWaiverSelected
   */
  private void setDamageWaiverOptions(final CartEntryModel entryModel,
      final boolean isGearGuardProFullWaiverSelected,
      final boolean isGearGuardWaiverSelected, final boolean isNoDamageWaiverSelected) {
    entryModel.setGearGuardProFullWaiverSelected(isGearGuardProFullWaiverSelected);
    entryModel.setGearGuardWaiverSelected(isGearGuardWaiverSelected);
    entryModel.setNoDamageWaiverSelected(isNoDamageWaiverSelected);
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
      final long quantityToAdd, final PointOfServiceModel deliveryPointOfService,final CommerceCartParameter parameter) {

    //based on used gear add to cart below commented code need to update/remove.
    /*if(productModel instanceof BlSerialProductModel) { //NOSONAR
      return getAllowedCartAdjustmentForProduct(cartModel, productModel, quantityToAdd,
          deliveryPointOfService);
    }*/ //NOSONAR
 if(BooleanUtils.isTrue(parameter.getIsFromRentAgainPage())){
   return quantityToAdd;
 }
    return DEFAULT_STOCK_QUANTITY;
  }

}
