package com.braintree.transaction.service.impl;

import com.braintree.constants.BraintreeConstants;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.braintree.order.capture.partial.process.OrderCompleteProcessService;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import com.braintree.exceptions.BraintreeErrorException;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import org.apache.commons.collections.CollectionUtils;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;


public class BrainTreePaymentTransactionServiceImpl implements BrainTreePaymentTransactionService
{
	private final String ORDER_PROCESS_PREFIX = "order-process";
	private final String ORDER_PROCESS_SUFFIX = "_SubmitForSettlementEvent";
	private static final String ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE = "orderModel can not be null";

	private FlexibleSearchService flexibleSearchService;
	private BusinessProcessService businessProcessService;
	private OrderCompleteProcessService orderCompleteProcessService;
	private ModelService modelService;

	@Override
	public List<PaymentTransactionModel> getTransactionsByRequestIdAndPaymentProvider(String requestId, String paymentProvider)
	{
		final PaymentTransactionModel transactionModel = new PaymentTransactionModel();

		transactionModel.setRequestId(requestId);
		transactionModel.setPaymentProvider(paymentProvider);

		return flexibleSearchService.getModelsByExample(transactionModel);
	}

	@Override
	public PaymentTransactionModel getTransactionByRequestIdAndPaymentProvider(final String requestId,
			final String paymentProvider)
	{
		final List<PaymentTransactionModel> paymentTransactionModels = getTransactionsByRequestIdAndPaymentProvider(requestId,
				paymentProvider);
		return paymentTransactionModels == null || paymentTransactionModels.isEmpty() ? null : paymentTransactionModels.get(0);
	}

	@Override
	public void continueSubmitOrder(BrainTreeTransactionDetailModel currentTransaction, BigDecimal amount)
	{
		PaymentTransactionModel paymentTransactionModel = getTransactionByRequestIdAndPaymentProvider(currentTransaction.getId(),
				BraintreeConstants.BRAINTREE_PROVIDER_NAME);

		if (paymentTransactionModel != null)
		{
			resumeOrderProcess(paymentTransactionModel, currentTransaction, amount);
		}
	}

	@Override
	public void continueOrderProcess(OrderModel orderModel)
	{
		orderCompleteProcessService.startOrderCompletionProcess(orderModel);
	}

	@Override
	public boolean isValidTransactionId(final OrderModel orderModel, final String transactionId)
	{
		validateParameterNotNull(orderModel, ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE);
		boolean result = false;

		for (final PaymentTransactionModel paymentTransactionModel : orderModel.getPaymentTransactions())
		{
			for (final PaymentTransactionEntryModel paymentTransactionEntryModel : paymentTransactionModel.getEntries())
			{
				if ((paymentTransactionEntryModel.getType() == PaymentTransactionType.AUTHORIZATION
						&& TransactionStatus.ACCEPTED.name().equals(paymentTransactionEntryModel.getTransactionStatus()))
						&& paymentTransactionEntryModel.getRequestId().equals(transactionId))
				{
					result = true;
				}
			}
		}
		return result;
	}

	@Override
	public boolean isOrderFullyCaptured(AbstractOrderModel orderModel)
	{
		final List<PaymentTransactionModel> paymentTransactions = orderModel.getPaymentTransactions();
		if (CollectionUtils.isNotEmpty(paymentTransactions))
		{
			BigDecimal capturedAmount = paymentTransactions.stream()
					.flatMap(transaction -> transaction.getEntries()
							.stream())
					.filter(entries -> entries.getType().equals(PaymentTransactionType.CAPTURE))
					.map(PaymentTransactionEntryModel::getAmount)
					.reduce(BigDecimal.ZERO, BigDecimal::add);
			return capturedAmount.doubleValue() >= orderModel.getTotalPrice();
		}
		return Boolean.FALSE;
	}

	@Override
	public boolean canPerformDelayedCapture(OrderModel orderModel, BigDecimal amountForCapture)
			throws BraintreeErrorException
	{
		final List<PaymentTransactionModel> paymentTransactions = orderModel.getPaymentTransactions();
		boolean result = Boolean.FALSE;
		if (CollectionUtils.isNotEmpty(paymentTransactions))
		{
			BigDecimal capturedAmount = paymentTransactions.stream()
					.flatMap(transaction -> transaction.getEntries()
							.stream())
					.filter(entries -> entries.getType().equals(PaymentTransactionType.CAPTURE))
					.map(PaymentTransactionEntryModel::getAmount)
					.reduce(BigDecimal.ZERO, BigDecimal::add);
			result = capturedAmount.add(amountForCapture).doubleValue() <= orderModel.getTotalPrice();
			if (!result)
			{
				throw new BraintreeErrorException(getLocalizedString(BraintreeConstants.EXCEEDED_CAPTURE_AMOUNT));
			}
		}
		return result;
	}

	@Override
	public void setOrderStatus(final AbstractOrderModel orderModel, final OrderStatus orderStatus)
	{
		orderModel.setStatus(orderStatus);
		getModelService().save(orderModel);
		getModelService().refresh(orderModel);
	}


	private void resumeOrderProcess(PaymentTransactionModel paymentTransactionModel,
			BrainTreeTransactionDetailModel currentTransaction, BigDecimal amount)
	{
		OrderModel order = (OrderModel) paymentTransactionModel.getOrder();
		setAmount(paymentTransactionModel, amount);

		OrderProcessModel orderProcess = getOrderProcess(order);

		if (orderProcess != null)
		{
			getBusinessProcessService().triggerEvent(orderProcess.getCode() + ORDER_PROCESS_SUFFIX);
		}

	}

	@Override
	public void resumeOrderProcess(OrderModel order)
	{
		if (order != null)
		{
			OrderProcessModel orderProcess = getOrderProcess(order);

			if (orderProcess != null)
			{
				getBusinessProcessService().triggerEvent(orderProcess.getCode() + ORDER_PROCESS_SUFFIX);
			}
		}
	}

	private OrderProcessModel getOrderProcess(OrderModel order)
	{
		Collection<OrderProcessModel> orderProcess = order.getOrderProcess();
		for (OrderProcessModel processModel : orderProcess)
		{
			if (ORDER_PROCESS_PREFIX.equals(processModel.getProcessDefinitionName()))
			{
				return processModel;
			}
		}
		return null;
	}

	private void setAmount(PaymentTransactionModel paymentTransactionModel, BigDecimal amount)
	{
		for (PaymentTransactionEntryModel entry : paymentTransactionModel.getEntries())
		{
			if (PaymentTransactionType.AUTHORIZATION.equals(entry.getType()))
			{
				entry.setSubmittedForSettlementAmount(amount);
				modelService.save(entry);
			}
		}
	}

	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
	{
		this.flexibleSearchService = flexibleSearchService;
	}

	public BusinessProcessService getBusinessProcessService()
	{
		return businessProcessService;
	}

	public void setBusinessProcessService(BusinessProcessService businessProcessService)
	{
		this.businessProcessService = businessProcessService;
	}

	public OrderCompleteProcessService getOrderCompleteProcessService()
	{
		return orderCompleteProcessService;
	}

	public void setOrderCompleteProcessService(
			OrderCompleteProcessService orderCompleteProcessService)
	{
		this.orderCompleteProcessService = orderCompleteProcessService;
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(ModelService modelService)
	{
		this.modelService = modelService;
	}
}
