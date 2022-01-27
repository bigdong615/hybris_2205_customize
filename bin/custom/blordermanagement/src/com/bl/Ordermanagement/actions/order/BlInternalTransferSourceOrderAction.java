/*
 * [y] hybris Platform
 *
 * Copyright (c) 2018 SAP SE or an SAP affiliate company.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of SAP
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with SAP.
 *
 */
package com.bl.Ordermanagement.actions.order;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.constants.WarehousingConstants;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Action node responsible for sourcing the order and allocating the consignments. After the consignments are created,
 * the consignment sub-process is started for every consignment.
 */
public class BlInternalTransferSourceOrderAction extends AbstractProceduralAction<OrderProcessModel>
{

	private static final Logger LOG = Logger.getLogger(BlInternalTransferSourceOrderAction.class);

	private AllocationService allocationService;
	private BusinessProcessService businessProcessService;

	@Override
	public void executeAction(final OrderProcessModel process) throws RetryLaterException, Exception
	{

		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Process: {} in step {}",
				process.getCode(), getClass().getSimpleName());

		final OrderModel order = process.getOrder();

		final SourcingResults	results = createSourcingResult(order);

		if (results != null) {

			results.getResults().forEach(this::logSourcingInfo);
			final Collection<ConsignmentModel> consignments = getAllocationService()
							.createConsignments(process.getOrder(), "cons" + process.getOrder().getCode(),
									results);

			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"Number of consignments created during internal order transfer allocation: {}",
					consignments.size());

			startConsignmentSubProcess(consignments, process);

			order.setStatus(OrderStatus.PENDING);

			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Internal Transfer Order with code : {} was successfully sourced", order.getCode());
		}

		getModelService().save(order);
	}

	private SourcingResults createSourcingResult(final OrderModel order) {

	if (BooleanUtils.isTrue(order.getInternalTransferOrder())) {

			final SourcingResults sourcingResults =  new SourcingResults();
			final Set<SourcingResult> sourcingResultSet = new HashSet<>();
			final SourcingResult sourcingResult = new SourcingResult();
			sourcingResult.setWarehouse(order.getInternalTransferOrderWarehouse());
			final Map<AbstractOrderEntryModel,Long> sourcingMap = new HashMap<>();

			if (CollectionUtils.isNotEmpty(order.getEntries()))	{

				order.getEntries().stream().forEach(abstractOrderEntryModel ->
						sourcingMap.put(abstractOrderEntryModel, abstractOrderEntryModel.getQuantity())
				);

				sourcingResult.setAllocation(sourcingMap);
				sourcingResultSet.add(sourcingResult);
				sourcingResults.setResults(sourcingResultSet);
				sourcingResults.setComplete(true);

				return sourcingResults;
			}

		}

		BlLogger.logFormatMessageInfo(LOG, Level.INFO,
				"Order can't be sourced. Order code : {} is not an internal transfer order.",
				order.getCode());

		return null;

	}
	/**
	 * Create and start a consignment process for each consignment in the collection.
	 *
	 * @param consignments
	 * 		- list of consignments; never <tt>null</tt>
	 * @param process
	 * 		- order process model
	 */
	protected void startConsignmentSubProcess(final Collection<ConsignmentModel> consignments, final OrderProcessModel process)
	{
		for (final ConsignmentModel consignment : consignments)
		{
			final ConsignmentProcessModel subProcess = getBusinessProcessService()
					.createProcess(consignment.getCode() + WarehousingConstants.CONSIGNMENT_PROCESS_CODE_SUFFIX,
							BlOrdermanagementConstants.CONSIGNMENT_SUBPROCESS_NAME);
			subProcess.setParentProcess(process);
			subProcess.setConsignment(consignment);
			save(subProcess);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Start Consignment sub-process: '{}'", subProcess.getCode());

			getBusinessProcessService().startProcess(subProcess);
		}
	}

	protected void logSourcingInfo(final SourcingResult result)
	{
		if (result != null)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Sourcing from Location: '{}'", result.getWarehouse().getCode());

			result.getAllocation().forEach((product, qty) -> BlLogger
					.logFormatMessageInfo(LOG, Level.DEBUG, "Product {} :  {} : Quantity: {}",
							product.getProduct().getCode(), product.getProduct().getName(getSessionLocale()),
							qty));
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,"The sourcing result is null");
		}
	}

	protected Locale getSessionLocale()
	{
		return JaloSession.getCurrentSession().getSessionContext().getLocale();
	}

	protected BusinessProcessService getBusinessProcessService()
	{
		return businessProcessService;
	}

	public void setBusinessProcessService(final BusinessProcessService businessProcessService)
	{
		this.businessProcessService = businessProcessService;
	}

	public AllocationService getAllocationService() {
		return allocationService;
	}

	public void setAllocationService(
			final AllocationService allocationService) {
		this.allocationService = allocationService;
	}
}
