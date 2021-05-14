package com.bl.core.services.strategy.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationStatus;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceUpdateCartEntryStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.storelocator.model.PointOfServiceModel;
import java.util.Date;
import java.util.List;

/**
 * @author Neeraj Singh
 */
public class DefaultBlCommerceUpdateCartEntryStrategy extends
    DefaultCommerceUpdateCartEntryStrategy {

  private BlDatePickerService blDatePickerService;
  private BlCommerceStockService blCommerceStockService;

  /**
   * {@inheritDoc}
   */
  @Override
  protected CommerceCartModification modifyEntry(final CartModel cartModel,
      final AbstractOrderEntryModel entryToUpdate,
      final long actualAllowedQuantityChange, final long newQuantity,
      final Integer maxOrderQuantity) {
    // Now work out how many that leaves us with on this entry
    final long entryNewQuantity =
        entryToUpdate.getQuantity().longValue() + actualAllowedQuantityChange;

    final ModelService modelService = getModelService();

    if (entryNewQuantity >= 0) {
      // Adjust the entry quantity to the new value
      entryToUpdate.setQuantity(Long.valueOf(entryNewQuantity));
      modelService.save(entryToUpdate);
      modelService.refresh(cartModel);
      final CommerceCartParameter parameter = new CommerceCartParameter();
      parameter.setEnableHooks(true);
      parameter.setCart(cartModel);
      getCommerceCartCalculationStrategy().calculateCart(parameter);
      modelService.refresh(entryToUpdate);

      // Return the modification data
      final CommerceCartModification modification = new CommerceCartModification();
      modification.setQuantityAdded(actualAllowedQuantityChange);
      modification.setEntry(entryToUpdate);
      modification.setQuantity(entryNewQuantity);

      //isMaxOrderQuantitySet functionality is not applicable for BL.
      /*if (isMaxOrderQuantitySet(maxOrderQuantity) && entryNewQuantity == maxOrderQuantity.longValue())
      {
        modification.setStatusCode(CommerceCartModificationStatus.MAX_ORDER_QUANTITY_EXCEEDED);
      }*/
      if (newQuantity == entryNewQuantity) {
        modification.setStatusCode(CommerceCartModificationStatus.SUCCESS);
      } else {
        modification.setStatusCode(CommerceCartModificationStatus.LOW_STOCK);
      }

      return modification;
    }

    return new CommerceCartModification();
  }

  /**
   * {@InheritDoc}
   */
  @Override
  protected long getAllowedCartAdjustmentForProduct(final CartModel cartModel,
      final ProductModel productModel,
      final long quantityToAdd, final PointOfServiceModel pointOfServiceModel) {

    long stockLevel;
    final long newTotalQuantityAfterStockLimit;
    final RentalDateDto rentalDateDto = getBlDatePickerService().getRentalDatesFromSession();

    final List<WarehouseModel> warehouseModelList = getBaseStoreService().getCurrentBaseStore()
        .getWarehouses();

    final long cartLevel = checkCartLevel(productModel, cartModel, pointOfServiceModel);

    // How many will we have in our cart if we add quantity
    final long newTotalQuantity = cartLevel + quantityToAdd;

    if (rentalDateDto != null) {
      final Date startDay = BlDateTimeUtils
          .convertStringDateToDate(rentalDateDto.getSelectedFromDate(),
              BlCoreConstants.DATE_FORMAT);
      final Date endDay = BlDateTimeUtils
          .convertStringDateToDate(rentalDateDto.getSelectedToDate(), BlCoreConstants.DATE_FORMAT);

      stockLevel = getBlCommerceStockService()
          .getAvailableCount(productModel.getCode(), warehouseModelList, startDay, endDay);

      // Now limit that to the total available in stock
      newTotalQuantityAfterStockLimit = Math.min(newTotalQuantity, stockLevel);
    } else {
      newTotalQuantityAfterStockLimit = newTotalQuantity;
    }

    // So now work out what the maximum allowed to be added is (note that
    // this may be negative! We can remove this commented code later on, if maxOrderQuantity not required in BL)
    //final Integer maxOrderQuantity = productModel.getMaxOrderQuantity();

    /*if (isMaxOrderQuantitySet(maxOrderQuantity))
    {
      final long newTotalQuantityAfterProductMaxOrder = Math
          .min(newTotalQuantityAfterStockLimit, maxOrderQuantity.longValue());
      return newTotalQuantityAfterProductMaxOrder - cartLevel;
    }*/
    return newTotalQuantityAfterStockLimit - cartLevel;
  }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }

  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }
}
