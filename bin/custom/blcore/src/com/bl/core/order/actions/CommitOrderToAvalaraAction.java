package com.bl.core.order.actions;

import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.task.RetryLaterException;

import java.util.Objects;

import javax.annotation.Resource;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.OrderToAvalaraProcessModel;
import com.bl.logging.BlLogger;
import com.bl.tax.ResponseData;
import com.bl.tax.service.BlTaxService;


/**
 * @author Ravikumar
 *
 */
public class CommitOrderToAvalaraAction extends AbstractSimpleDecisionAction<OrderToAvalaraProcessModel>
{

	private static final Logger LOG = Logger.getLogger(CommitOrderToAvalaraAction.class);

	@Resource(name = "defaultBlAvalaraTaxService")
	private BlTaxService<AbstractOrderModel, ExternalTaxDocument> defaultBlAvalaraTaxService;

	@Override
	public Transition executeAction(final OrderToAvalaraProcessModel orderToAvalaraProcessModel)
			throws RetryLaterException, Exception
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Order to Avalara Process Start : {} in class : {}",
				orderToAvalaraProcessModel.getCode(), getClass().getSimpleName());

		if (Objects.isNull(orderToAvalaraProcessModel.getOrder()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Order to Avalara Process Failed : {} in class : {} due to order object is null",
					orderToAvalaraProcessModel.getCode(), getClass().getSimpleName());
			return Transition.NOK;
		}
		if (Objects.isNull(orderToAvalaraProcessModel.getOrder().getStatus()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Order to Avalara Process Failed : {} in class : {} due to No Order status found on order : {}",
					orderToAvalaraProcessModel.getCode(), getClass().getSimpleName(), orderToAvalaraProcessModel.getOrder().getCode());
			return Transition.NOK;
		}
		if (BooleanUtils
				.isFalse(orderToAvalaraProcessModel.getOrder().getStatus().getCode().equals(OrderStatus.COMPLETED.getCode())))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Order to Avalara Process Failed : {} in class : {} due to Order : {} is not completed, Order has status : {}",
					orderToAvalaraProcessModel.getCode(), getClass().getSimpleName(), orderToAvalaraProcessModel.getOrder().getCode(),
					orderToAvalaraProcessModel.getOrder().getStatus().getCode());
			return Transition.NOK;
		}
		final boolean isOrderCommittedToAvalara = commitOrderToAvalara(orderToAvalaraProcessModel.getOrder());
		return isOrderCommittedToAvalara ? Transition.OK : Transition.NOK;
	}

	/**
	 * Commit order to avalara.
	 *
	 * @param order
	 *           the order
	 */
	private boolean commitOrderToAvalara(final AbstractOrderModel order)
	{
		try
		{
			final ResponseData responseData = getDefaultBlAvalaraTaxService().commitOrderToAvalara(order);
			if (Objects.isNull(responseData))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
						"CommitOrderToAvalaraAction :: commitOrderToAvalara :: ResponseData is null for Order with code : {}",
						order.getCode());
				return false;
			}
			if (BooleanUtils.isFalse("201".equalsIgnoreCase(responseData.getStatusCode())))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
						"CommitOrderToAvalaraAction :: commitOrderToAvalara :: Failed to Commit Order with code : {} to avalara. Received Status code : {} and Status Message from avalara :: {}",
						order.getCode(), responseData.getStatusCode(), responseData.getStatusMessage());
				return false;
			}
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"CommitOrderToAvalaraAction :: commitOrderToAvalara ::  Order with code : {} is committed to avalara",
					order.getCode());
			order.setIsOrderCommittedToAvalara(Boolean.TRUE);
			getModelService().save(order);
			getModelService().refresh(order);
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Order with code : {} is failed to commit to avalara", order.getCode());
			return false;
		}
		return true;
	}

	/**
	 * @return the defaultBlAvalaraTaxService
	 */
	public BlTaxService<AbstractOrderModel, ExternalTaxDocument> getDefaultBlAvalaraTaxService()
	{
		return defaultBlAvalaraTaxService;
	}

	/**
	 * @param defaultBlAvalaraTaxService
	 *           the defaultBlAvalaraTaxService to set
	 */
	public void setDefaultBlAvalaraTaxService(
			final BlTaxService<AbstractOrderModel, ExternalTaxDocument> defaultBlAvalaraTaxService)
	{
		this.defaultBlAvalaraTaxService = defaultBlAvalaraTaxService;
	}

}
