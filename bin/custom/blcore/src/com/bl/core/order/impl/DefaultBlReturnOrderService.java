package com.bl.core.order.impl;


import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ReplacementRequestStatus;
import com.bl.core.model.BlReturnEntryModel;
import com.bl.core.order.BlReturnOrderService;
import de.hybris.platform.basecommerce.enums.ReturnAction;
import de.hybris.platform.basecommerce.enums.ReturnStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.returns.impl.DefaultReturnService;
import de.hybris.platform.returns.model.ReturnEntryModel;
import de.hybris.platform.returns.model.ReturnProcessModel;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.session.SessionService;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;


public class DefaultBlReturnOrderService extends DefaultReturnService implements BlReturnOrderService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlReturnOrderService.class);

	@Resource(name = "sessionService")
	private SessionService sessionService;

	@Resource
	private BusinessProcessService businessProcessService;

	@Override
	public ReturnRequestModel createReturnRequest(OrderModel orderModel, List<String> productReturnInfo) {
		ReturnRequestModel returnRequest = createReturnRequest(orderModel);
		createRMA(returnRequest);
		returnRequest.setIsReplacementOrder(true);
		returnRequest.setReplacementRequestStatus(ReplacementRequestStatus.PROCESSING);

		List<ReturnEntryModel> returnEntries = new ArrayList();
		for(String productData : productReturnInfo) {
			// set returnEntry for every prod - qty combination
			setReturnEntry(orderModel, productData, returnEntries, returnRequest);
		}

		getModelService().save(returnRequest);

		// add return request in session
		addReturnRequestInSession(returnRequest);

		// TBD: when to start the business process
		startReturnProcess(returnRequest);
		return returnRequest;
	}

	private void setReturnEntry(AbstractOrderModel order, String productData, List<ReturnEntryModel> returnEntries, ReturnRequestModel returnRequestModel)
	{
		String[] prod = productData.split(":");
		String prodCode = (String) Array.get(prod, 0);
		String prodQty = (String) Array.get(prod, 1);

		for (AbstractOrderEntryModel entry : order.getEntries()) {
			if(entry.getProduct().getCode().equalsIgnoreCase(prodCode))
			{
				BlReturnEntryModel blReturnEntry = getModelService().create(BlReturnEntryModel.class);
				blReturnEntry.setOrderEntry(entry);
				blReturnEntry.setAction(ReturnAction.IMMEDIATE); // TBD: Mandatory field value
				blReturnEntry.setStatus(ReturnStatus.APPROVAL_PENDING); // TBD: Mandatory field value
				blReturnEntry.setExpectedQuantity(Long.parseLong(prodQty)); // TBD for return qty attribute
				getModelService().save(blReturnEntry);

				returnEntries.add(blReturnEntry);

			}
		}
		returnRequestModel.setReturnEntries(returnEntries);
	}

	private void addReturnRequestInSession(ReturnRequestModel returnRequest) {
		if(sessionService.hasCurrentSession())
		{
			sessionService.getCurrentSession();
			sessionService.setAttribute(BlCoreConstants.RETURN_REQUEST, returnRequest);
		}
		else {
			sessionService.createNewSession();
			sessionService.getCurrentSession();
			sessionService.setAttribute(BlCoreConstants.RETURN_REQUEST, returnRequest);
		}
	}

	private void startReturnProcess(ReturnRequestModel returnRequest) {

		final String fulfilmentProcessDefinitionName = "return-process";
		final String processCode = fulfilmentProcessDefinitionName + "-" + returnRequest.getCode() + "-" + System.currentTimeMillis();
		final ReturnProcessModel businessProcessModel = businessProcessService.createProcess(processCode, fulfilmentProcessDefinitionName);
		businessProcessModel.setReturnRequest(returnRequest);
		getModelService().save(businessProcessModel);

		businessProcessService.startProcess(businessProcessModel);
		if (LOG.isInfoEnabled())
		{
			LOG.info(String.format("Started the process %s", processCode));
		}
	}
}
