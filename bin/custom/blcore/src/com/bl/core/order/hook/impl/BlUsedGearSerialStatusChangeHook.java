package com.bl.core.order.hook.impl;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.cart.BlCartService;
import de.hybris.platform.commerceservices.order.hook.CommercePlaceOrderMethodHook;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.servicelayer.model.ModelService;


/**
 * It is a custom implementation of OOTB class {@link CommercePlaceOrderMethodHook} to do change serial product status
 * when order has been placed.
 *
 * @author Gaurav
 *
 */
public class BlUsedGearSerialStatusChangeHook implements CommercePlaceOrderMethodHook
{

	private ModelService modelService;
	private BlCartService blCartService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void afterPlaceOrder(final CommerceCheckoutParameter parameter, final CommerceOrderResult orderModel)
			throws InvalidCartException
	{

		final OrderModel order = orderModel.getOrder();
		getModelService().refresh(order);

		for (final AbstractOrderEntryModel entry : order.getEntries())
		{
			if (entry.getProduct() instanceof BlSerialProductModel)
			{
				final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) entry.getProduct();
				if (SerialStatusEnum.ADDED_TO_CART.equals(blSerialProductModel.getSerialStatus()))
				{
					blSerialProductModel.setSerialStatus(SerialStatusEnum.SOLD);
					getBlCartService().changeSerialStatusInStagedVersion(blSerialProductModel.getCode(), SerialStatusEnum.SOLD);
					blSerialProductModel.setHardAssigned(true);
					blSerialProductModel.setIsBufferedInventory(Boolean.FALSE);
					getModelService().save(blSerialProductModel);
					getModelService().refresh(order);
				}
			}
		}
	}



	/**
	 * {@inheritDoc}
	 */
	@Override
	public void beforePlaceOrder(final CommerceCheckoutParameter parameter) throws InvalidCartException
	{
		// not implemented
	}


	/**
	 * {@inheritDoc}
	 */
	@Override
	public void beforeSubmitOrder(final CommerceCheckoutParameter parameter, final CommerceOrderResult result)
			throws InvalidCartException
	{
		// not implemented
	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	public BlCartService getBlCartService() {
		return blCartService;
	}

	public void setBlCartService(BlCartService blCartService) {
		this.blCartService = blCartService;
	}

}
