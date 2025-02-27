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
package com.bl.Ordermanagement.actions.order.fraudcheck;

import de.hybris.platform.basecommerce.enums.FraudStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.fraud.FraudService;
import de.hybris.platform.fraud.impl.FraudServiceResponse;
import de.hybris.platform.fraud.model.FraudReportModel;
import de.hybris.platform.orderhistory.model.OrderHistoryEntryModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.util.Config;
import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Required;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


/**
 * Check for fraudulent order.<br/>
 * If a fraud is detected, the process transitions to FRAUD and the order will be cancelled.<br/>
 * If no fraud is detected, the process transitions to OK and the order will be sourced.<br/>
 * If the order is deemed potentially fraudulent, the process transitions to POTENTIAL and the order must be
 * approved/declined by a customer support agent.
 */
public class FraudCheckOrderAction extends AbstractFraudCheckAction<OrderProcessModel>
{
	private static final Logger LOG = LoggerFactory.getLogger(FraudCheckOrderAction.class);
	private FraudService fraudService;
	private String providerName;
	private Double scoreLimit;
	private Double scoreTolerance;

	@Override
	public Transition executeAction(final OrderProcessModel process)
	{
		validateParameterNotNullStandardMessage("process", process);
		validateParameterNotNullStandardMessage("order", process.getOrder());

		LOG.info("Process: {} in step {}", process.getCode(), getClass().getSimpleName());
		//get the fraud-detection configuration
		scoreLimit = getScoreLimit();
		scoreTolerance = getScoreTolerance();

		final OrderModel order = process.getOrder();
		final FraudServiceResponse response = getFraudService().recognizeOrderSymptoms(getProviderName(), order);

		final double score = response.getScore();
		LOG.info("Order: {} has a score of {}", order.getCode(), score);
		if (score < scoreLimit)
		{
			final FraudReportModel fraudReport = createFraudReport(providerName, response, order, FraudStatus.OK);
			final OrderHistoryEntryModel historyEntry = createHistoryLog(providerName, order, FraudStatus.OK, null);
			order.setFraudulent(Boolean.FALSE);
			order.setPotentiallyFraudulent(Boolean.FALSE);
			order.setStatus(OrderStatus.FRAUD_CHECKED);
			modelService.save(fraudReport);
			modelService.save(historyEntry);
			modelService.save(order);
			return Transition.OK;
		}
		else
		{
			LOG.info("Order: {} has a fraud score of {}", order.getCode(), score);
			final FraudReportModel fraudReport = createFraudReport(providerName, response, order, FraudStatus.CHECK);
			final OrderHistoryEntryModel historyEntry = createHistoryLog(providerName, order, FraudStatus.CHECK,
					fraudReport.getCode());
			order.setFraudulent(Boolean.FALSE);
			order.setPotentiallyFraudulent(Boolean.TRUE);
			order.setStatus(OrderStatus.FRAUD_CHECKED);
			modelService.save(fraudReport);
			modelService.save(historyEntry);
			modelService.save(order);
			return Transition.POTENTIAL;
		}
	/*	else
		{
			final FraudReportModel fraudReport = createFraudReport(providerName, response, order, FraudStatus.FRAUD);
			final OrderHistoryEntryModel historyEntry = createHistoryLog(providerName, order, FraudStatus.FRAUD,
					fraudReport.getCode());
			order.setFraudulent(Boolean.TRUE);
			order.setPotentiallyFraudulent(Boolean.FALSE);
			order.setStatus(OrderStatus.FRAUD_CHECKED);
			modelService.save(fraudReport);
			modelService.save(historyEntry);
			modelService.save(order);
			return Transition.FRAUD;
		} */
	}

	public Double getConfig(final String property)
	{
		final String value = Config.getParameter(BlOrdermanagementConstants.EXTENSIONNAME + ".fraud." + property);
		return Double.valueOf(value);
	}

	protected FraudService getFraudService()
	{
		return fraudService;
	}

	@Required
	public void setFraudService(final FraudService fraudService)
	{
		this.fraudService = fraudService;
	}

	protected String getProviderName()
	{
		return providerName;
	}

	@Required
	public void setProviderName(final String providerName)
	{
		this.providerName = providerName;
	}

	protected Double getScoreLimit()
	{
		if (scoreLimit == null)
		{
			scoreLimit = getConfig("scoreLimit");
		}
		return scoreLimit;
	}

	public void setScoreLimit(final Double scoreLimit)
	{
		this.scoreLimit = scoreLimit;
	}

	protected Double getScoreTolerance()
	{
		if (scoreTolerance == null)
		{
			scoreTolerance = getConfig("scoreTolerance");
		}
		return scoreTolerance;
	}

	public void setScoreTolerance(final Double scoreTolerance)
	{
		this.scoreTolerance = scoreTolerance;
	}

}
