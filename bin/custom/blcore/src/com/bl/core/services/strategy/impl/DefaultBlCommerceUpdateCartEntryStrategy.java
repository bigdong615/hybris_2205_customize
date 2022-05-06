package com.bl.core.services.strategy.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.commerceservices.order.CommerceCartModification;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceUpdateCartEntryStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.storelocator.model.PointOfServiceModel;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;

/** This class is used to get available stock and modify cart entry.
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
	public CommerceCartModification updateQuantityForCartEntry(final CommerceCartParameter parameters)
			throws CommerceCartModificationException
	{
		beforeUpdateCartEntry(parameters);
		final CartModel cartModel = parameters.getCart();
		final long newQuantity = parameters.getQuantity();
		final long entryNumber = parameters.getEntryNumber();
		final boolean isFromAddToCartPopup = BooleanUtils.toBoolean(parameters.getIsFromAddToCartPopup());

		validateParameterNotNull(cartModel, "Cart model cannot be null");
		CommerceCartModification modification;

		final AbstractOrderEntryModel entryToUpdate = getEntryForNumber(cartModel, (int) entryNumber);
		validateEntryBeforeModification(newQuantity, entryToUpdate);
		final Integer maxOrderQuantity = entryToUpdate.getProduct().getMaxOrderQuantity();
		// Work out how many we want to add (could be negative if we are
		// removing items)
		final long quantityToAdd = newQuantity - entryToUpdate.getQuantity().longValue();

		// So now work out what the maximum allowed to be added is (note that
		// this may be negative!)
		final long actualAllowedQuantityChange = isFromAddToCartPopup 
				? getAllowedCartAdjustmentForProductFromPopup(cartModel, entryToUpdate.getProduct(), 
						quantityToAdd, entryToUpdate.getDeliveryPointOfService()) 
				: getAllowedCartAdjustmentForProduct(cartModel, entryToUpdate.getProduct(),
				quantityToAdd, entryToUpdate.getDeliveryPointOfService());
		//Now do the actual cartModification
		modification = modifyEntry(cartModel, entryToUpdate, actualAllowedQuantityChange, newQuantity, maxOrderQuantity);
		afterUpdateCartEntry(parameters, modification);
		return modification;

	}
  
  /**
	 * Gets the quantity for product from add to cart popup without checking stock availability
	 *
	 * @param cartModel
	 *           the cart model
	 * @param productModel
	 *           the product model
	 * @param quantityToAdd
	 *           the quantity to add
	 * @param pointOfServiceModel
	 *           the point of service model
	 * @return the allowed cart adjustment for product from popup
	 */
	private long getAllowedCartAdjustmentForProductFromPopup(final CartModel cartModel, final ProductModel productModel,
			final long quantityToAdd, final PointOfServiceModel pointOfServiceModel)
	{
		final long cartLevel = checkCartLevel(productModel, cartModel, pointOfServiceModel);

		// How many will we have in our cart if we add quantity
		final long newTotalQuantity = cartLevel + quantityToAdd;

		return newTotalQuantity - cartLevel;
	}

  /**
   * {@inheritDoc}
   */
  @Override
  protected CommerceCartModification modifyEntry(final CartModel cartModel,
      final AbstractOrderEntryModel entryToUpdate,
      final long actualAllowedQuantityChange, final long newQuantity,
      final Integer maxOrderQuantity) {

		CommerceCartModification modifyEntry = super.modifyEntry(cartModel, entryToUpdate, actualAllowedQuantityChange, newQuantity,
        maxOrderQuantity);
		updateOptionEntry(modifyEntry.getEntry());
		return modifyEntry;
  }
	/**
	 * Update options on entry 
	 * @param AbstractOrderEntryModel
	 *           the orderEntry
	 */
private void updateOptionEntry(final AbstractOrderEntryModel orderEntry){
  	if(CollectionUtils.isNotEmpty(orderEntry.getOptions())){
			final BlOptionsModel optionsModel = orderEntry.getOptions().iterator().next();
			final Integer quantity = Integer.parseInt(orderEntry.getQuantity().toString());
			List<BlOptionsModel> selectOptionList = new ArrayList<BlOptionsModel>(quantity);
			for(int i = 0 ; i < quantity ; i++){
				selectOptionList.add(optionsModel);
			}
			orderEntry.setOptions(selectOptionList);
			getModelService().save(orderEntry);
			getModelService().refresh(orderEntry);
		}
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
    final long cartLevel = checkCartLevel(productModel, cartModel, pointOfServiceModel);

    // How many will we have in our cart if we add quantity
    final long newTotalQuantity = cartLevel + quantityToAdd;
    newTotalQuantityAfterStockLimit = newTotalQuantity;

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
