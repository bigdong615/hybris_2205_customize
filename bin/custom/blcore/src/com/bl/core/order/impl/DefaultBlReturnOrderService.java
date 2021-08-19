package com.bl.core.order.impl;


import com.bl.logging.BlLogger;
import de.hybris.platform.basecommerce.enums.ReturnAction;
import de.hybris.platform.basecommerce.enums.ReturnStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.returns.impl.DefaultReturnService;
import de.hybris.platform.returns.model.ReturnEntryModel;
import de.hybris.platform.returns.model.ReturnProcessModel;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.session.SessionService;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ReplacementRequestStatus;
import com.bl.core.model.BlReturnEntryModel;
import com.bl.core.order.BlReturnOrderService;

/**
 * Return order class
 */
public class DefaultBlReturnOrderService extends DefaultReturnService implements BlReturnOrderService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlReturnOrderService.class);

	@Resource(name = "sessionService")
	private SessionService sessionService;

	@Resource
	private BusinessProcessService businessProcessService;

	@Override
	public ReturnRequestModel createReturnRequest(final OrderModel orderModel, final String productList) {
		final ReturnRequestModel returnRequest = createReturnRequest(orderModel);
		createRMA(returnRequest);
		returnRequest.setIsReplacementOrder(true);
		returnRequest.setReplacementRequestStatus(ReplacementRequestStatus.PROCESSING);

		final List<String> products = new ArrayList();
		setProducts(products, productList);

		final List<ReturnEntryModel> returnEntries = new ArrayList();
		for(final String productData : products) {
			setReturnEntry(orderModel, productData, returnEntries, returnRequest);
		}
		getModelService().save(returnRequest);
		addReturnRequestInSession(returnRequest);
		startReturnProcess(returnRequest);
		return returnRequest;
	}

	/*
	 * Set the required products after validation
	 * */
	private void setProducts(List<String> products, String productList) {
		productList = StringUtils.chop(productList);
		productList = productList.substring(1);
		final String[] productInfo = productList.split("% ");

		for (final String productData : productInfo) {
			String[] product = productData.split(":");
			String qty = (String) Array.get(product, 1);
			if(!qty.equalsIgnoreCase("0"))	{
				products.add(productData);
			}
		}
	}

	/**
	 * Set returnEntry for every product - quantity combination
	 */
	private void setReturnEntry(final AbstractOrderModel order, final String productData, final List<ReturnEntryModel> returnEntries, final ReturnRequestModel returnRequestModel)
	{
		final String[] prod = productData.split(":");
		final String prodCode = (String) Array.get(prod, 0);
		final String prodQty = (String) Array.get(prod, 1);

		for (final AbstractOrderEntryModel entry : order.getEntries()) {
			if(entry.getProduct().getCode().equalsIgnoreCase(prodCode))
			{
				final BlReturnEntryModel blReturnEntry = getModelService().create(BlReturnEntryModel.class);
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

	/*
	 *  Add return request in session
	 * */
	private void addReturnRequestInSession(final ReturnRequestModel returnRequest) {
		if(sessionService.hasCurrentSession())
		{
			sessionService.getCurrentSession();
			sessionService.setAttribute(BlCoreConstants.RETURN_REQUEST, returnRequest);
			BlLogger.logMessage(LOG, Level.DEBUG, "Return request added in session");
		}
		else {
			sessionService.createNewSession();
			sessionService.getCurrentSession();
			sessionService.setAttribute(BlCoreConstants.RETURN_REQUEST, returnRequest);
			BlLogger.logMessage(LOG, Level.DEBUG, "Return request added in session");
		}
	}

	/*
	 * Start return process
	 * */
	private void startReturnProcess(final ReturnRequestModel returnRequest) {
		final String fulfilmentProcessDefinitionName = "return-process";
		final String processCode = fulfilmentProcessDefinitionName + "-" + returnRequest.getCode() + "-" + System.currentTimeMillis();
		final ReturnProcessModel businessProcessModel = businessProcessService.createProcess(processCode, fulfilmentProcessDefinitionName);
		businessProcessModel.setReturnRequest(returnRequest);
		getModelService().save(businessProcessModel);
		businessProcessService.startProcess(businessProcessModel);
		if (LOG.isInfoEnabled()){
			LOG.info(String.format("Started the process %s", processCode));
		}
	}
}
