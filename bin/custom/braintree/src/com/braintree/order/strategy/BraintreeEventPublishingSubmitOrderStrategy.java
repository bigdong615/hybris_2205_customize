package com.braintree.order.strategy;


import com.bl.core.enums.ReplacementRequestStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.strategies.SubmitOrderStrategy;
import de.hybris.platform.order.strategies.impl.EventPublishingSubmitOrderStrategy;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import org.apache.commons.lang.BooleanUtils;


public class BraintreeEventPublishingSubmitOrderStrategy extends EventPublishingSubmitOrderStrategy implements SubmitOrderStrategy
{

	private static final String RETURN_REQUEST = "returnRequest";
	private ModelService modelService;
	private SessionService sessionService;

	@Override
	public void submitOrder(OrderModel order)
	{
		if (null != getSessionService().getAttribute(RETURN_REQUEST)) {
			final ReturnRequestModel returnRequestModel = getSessionService()
					.getAttribute(RETURN_REQUEST);
			returnRequestModel.setOrderForReplacement(order);
			returnRequestModel.setReplacementRequestStatus(ReplacementRequestStatus.COMPLETED);
			getModelService().save(returnRequestModel);
			getModelService().refresh(returnRequestModel);
		}

		// Stopping the order process in case of gift card order and new gear order.
		if (!order.isGiftCardOrder() && BooleanUtils.isFalse(order.getIsRetailGearOrder()))
		{	
		  super.submitOrder(order);
		}
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(ModelService modelService)
	{
		this.modelService = modelService;
	}


	public SessionService getSessionService() {
		return sessionService;
	}

	public void setSessionService(SessionService sessionService) {
		this.sessionService = sessionService;
	}



}
