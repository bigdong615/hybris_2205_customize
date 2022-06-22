package com.braintree.order.submitForSettlement.service.impl;

import com.bl.logging.BlLogger;
import com.braintree.command.request.BrainTreeSubmitForSettlementTransactionRequest;
import com.braintree.command.result.BrainTreeSubmitForSettlementTransactionResult;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.order.capture.partial.services.BraintreePartialCaptureService;
import com.braintree.order.submitForSettlement.service.BraintreeSubmitForSettlementService;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import java.math.BigDecimal;
import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;


public class BraintreeSubmitForSettlementServiceImpl implements BraintreeSubmitForSettlementService
{

	private static final Logger LOG = Logger.getLogger(BraintreeSubmitForSettlementServiceImpl.class);

	private static final String ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE = "orderModel can not be null";

	private BrainTreePaymentService brainTreePaymentService;
	private BrainTreePaymentTransactionService brainTreePaymentTransactionService;
	private BrainTreeTransactionService brainTreeTransactionService;
	private ModelService modelService;

	@Override
	public boolean submitForSettlement(OrderModel orderModel, BigDecimal amount, String authorizeTransactionID)
			throws BraintreeErrorException
	{
		validateParameterNotNull(orderModel, ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE);

		BrainTreeSubmitForSettlementTransactionRequest request =
				new BrainTreeSubmitForSettlementTransactionRequest(orderModel.getUser().getUid());
		request.setAmount(amount);
		request.setTransactionId(authorizeTransactionID);

		BrainTreeSubmitForSettlementTransactionResult brainTreeSubmitForSettlementTransactionResult = getBrainTreePaymentService()
				.submitForSettlementTransaction(request);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Payment capture response {} for the order {}",
				brainTreeSubmitForSettlementTransactionResult, orderModel.getCode());
		if (brainTreeSubmitForSettlementTransactionResult.isSuccess())
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Payment capture is successful for the order {}", orderModel.getCode());
			createTransaction(orderModel, brainTreeSubmitForSettlementTransactionResult);
				/*if (getBrainTreePaymentTransactionService().isOrderFullyCaptured(orderModel))
				{*/
				getBrainTreePaymentTransactionService().setOrderStatus(orderModel, OrderStatus.PAYMENT_CAPTURED);
				//getBrainTreePaymentTransactionService().continueOrderProcess(orderModel);
			/*}
			else
			{
				getBrainTreePaymentTransactionService().setOrderStatus(orderModel, OrderStatus.PARTIAL_CAPTURE);
			}*/
			return Boolean.TRUE;
		}
		throw new BraintreeErrorException(brainTreeSubmitForSettlementTransactionResult.getErrorMessage(),
				brainTreeSubmitForSettlementTransactionResult.getTransactionId());
	}

	@Override
	public boolean isSubmitForSettlementAvailable(OrderModel orderModel)
	{
		getModelService().refresh(orderModel);
		final List<PaymentTransactionModel> paymentTransactions = orderModel.getPaymentTransactions();
		if (CollectionUtils.isNotEmpty(paymentTransactions))
		{
			return paymentTransactions.stream()
					.flatMap(transaction -> transaction.getEntries()
							.stream())
					.noneMatch(entries -> entries.getType().equals(PaymentTransactionType.CAPTURE));
		}
		return Boolean.FALSE;
	}

	private void createTransaction(OrderModel orderModel, BrainTreeSubmitForSettlementTransactionResult brainTreeSaleTransactionResult)
	{
		final List<PaymentTransactionModel> paymentTransactions = orderModel.getPaymentTransactions();
		if (CollectionUtils.isNotEmpty(paymentTransactions))
		{
			PaymentTransactionModel transactionModel = paymentTransactions.iterator().next();
			getBrainTreeTransactionService().createSubmitForSettlementTransaction(transactionModel, brainTreeSaleTransactionResult);
		}
	}

	public BrainTreePaymentService getBrainTreePaymentService()
	{
		return brainTreePaymentService;
	}

	public void setBrainTreePaymentService(BrainTreePaymentService brainTreePaymentService)
	{
		this.brainTreePaymentService = brainTreePaymentService;
	}

	public BrainTreeTransactionService getBrainTreeTransactionService()
	{
		return brainTreeTransactionService;
	}

	public void setBrainTreeTransactionService(BrainTreeTransactionService brainTreeTransactionService)
	{
		this.brainTreeTransactionService = brainTreeTransactionService;
	}

	public BrainTreePaymentTransactionService getBrainTreePaymentTransactionService()
	{
		return brainTreePaymentTransactionService;
	}

	public void setBrainTreePaymentTransactionService(
			BrainTreePaymentTransactionService brainTreePaymentTransactionService)
	{
		this.brainTreePaymentTransactionService = brainTreePaymentTransactionService;
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
