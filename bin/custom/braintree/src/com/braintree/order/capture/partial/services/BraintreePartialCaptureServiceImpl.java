package com.braintree.order.capture.partial.services;

import com.braintree.command.request.BrainTreeSaleTransactionRequest;
import com.braintree.command.result.BrainTreeSaleTransactionResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.order.capture.partial.process.OrderCompleteProcessService;
import com.braintree.order.capture.partial.strategy.BraintreePartialOrderCalculationStrategy;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.google.common.base.Preconditions;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.payment.dto.CardInfo;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Logger;

import java.math.BigDecimal;
import java.util.List;


public class BraintreePartialCaptureServiceImpl implements BraintreePartialCaptureService
{
	private static final Logger LOG = Logger.getLogger(BraintreePartialCaptureServiceImpl.class);

	private static final String ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE = "orderModel can not be null";
	private static final String PAYMENT_PROVIDER_CAN_NOT_BE_NULL_MESSAGE = "payment provider can not be null";
	private static final String APPROVED = "APPROVED";
	private BraintreePartialOrderCalculationStrategy partialOrderCalculationStrategy;
	private BrainTreeConfigService brainTreeConfigService;
	private BrainTreePaymentService brainTreePaymentService;
	private BrainTreePaymentTransactionService brainTreePaymentTransactionService;
	private BrainTreeTransactionService brainTreeTransactionService;
	private OrderCompleteProcessService orderCompleteProcessService;
	private ModelService modelService;

	@Override
	public boolean partialCapture(OrderModel orderModel, BigDecimal captureAmount) throws BraintreeErrorException
	{
		validateParameterNotNull(orderModel, ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE);
		LOG.error("isPartialCapturePossible: " + isPartialCapturePossible(orderModel));
			BigDecimal possibleAmountForCapture = getPossibleAmountForCapture(orderModel);
			LOG.error("possibleAmountForCapture: " + possibleAmountForCapture);

			if (possibleAmountForCapture.compareTo(BigDecimal.ZERO) >= 0 && captureAmount.compareTo(possibleAmountForCapture) <= 0)
			{
				BrainTreeSaleTransactionResult brainTreeSaleTransactionResult = brainTreePaymentService
						.partialCaptureTransaction(createRequest(orderModel, captureAmount));

				if (brainTreeSaleTransactionResult.isSuccess())
				{
					createTransaction(orderModel, brainTreeSaleTransactionResult);
					if (isOrderComplete(orderModel))
					{
						getBrainTreePaymentTransactionService().setOrderStatus(orderModel, OrderStatus.PAYMENT_CAPTURED);
						getBrainTreePaymentTransactionService().continueOrderProcess(orderModel);
					}
					else
					{
						getBrainTreePaymentTransactionService().setOrderStatus(orderModel, OrderStatus.PARTIAL_CAPTURE);
					}
					return Boolean.TRUE;
				}
				throw new BraintreeErrorException(brainTreeSaleTransactionResult.getErrorMessage(),
						brainTreeSaleTransactionResult.getTransactionId());
			}
		return Boolean.FALSE;
	}

	@Override
	public boolean partialCapture(OrderModel orderModel, BigDecimal captureAmount, final String authorizeTransactionID)
			throws BraintreeErrorException
	{
		validateParameterNotNull(orderModel, ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE);

		BrainTreeSaleTransactionResult brainTreeSaleTransactionResult = brainTreePaymentService
				.partialCaptureTransaction(createRequest(orderModel,
						captureAmount, authorizeTransactionID));

		if (brainTreeSaleTransactionResult.isSuccess())
		{
			createTransaction(orderModel, brainTreeSaleTransactionResult);
			if (isOrderComplete(orderModel))
			{
				getBrainTreePaymentTransactionService().setOrderStatus(orderModel, OrderStatus.PAYMENT_CAPTURED);
				getBrainTreePaymentTransactionService().continueOrderProcess(orderModel);
			}
			else
			{
				getBrainTreePaymentTransactionService().setOrderStatus(orderModel, OrderStatus.PARTIAL_CAPTURE);
			}
			return Boolean.TRUE;
		}
		throw new BraintreeErrorException(brainTreeSaleTransactionResult.getErrorMessage(),
				brainTreeSaleTransactionResult.getTransactionId());
	}

	private boolean isOrderComplete(OrderModel orderModel)
	{
		BigDecimal amountForCapture = getPossibleAmountForCapture(orderModel);
		return BigDecimal.ZERO.compareTo(amountForCapture) >= 0;
	}

	private void createTransaction(OrderModel orderModel, BrainTreeSaleTransactionResult brainTreeSaleTransactionResult)
	{
		final List<PaymentTransactionModel> paymentTransactions = orderModel.getPaymentTransactions();
		if (CollectionUtils.isNotEmpty(paymentTransactions))
		{
			PaymentTransactionModel transactionModel = paymentTransactions.iterator().next();
			brainTreeTransactionService.createPartialCaptureTransaction(transactionModel, brainTreeSaleTransactionResult);
		}
	}

	private BrainTreeSaleTransactionRequest createRequest(OrderModel orderModel, BigDecimal captureAmount)
	{
		CustomerModel customer = (CustomerModel) orderModel.getUser();

		BrainTreeSaleTransactionRequest request = new BrainTreeSaleTransactionRequest(customer.getUid(),
				createCard(orderModel.getPaymentAddress()), null, captureAmount, createBillingInfo(orderModel.getDeliveryAddress()));

		final BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) orderModel.getPaymentInfo();

		request.setUsePaymentMethodToken(Boolean.TRUE);
		request.setPaymentMethodToken(brainTreePaymentInfoModel.getPaymentMethodToken());
		request.setRequestId(getRequestId(orderModel));
		return request;
	}

	private BrainTreeSaleTransactionRequest createRequest(OrderModel orderModel, BigDecimal captureAmount,
			String authorizeTransactionID)
	{
		CustomerModel customer = (CustomerModel) orderModel.getUser();

		BrainTreeSaleTransactionRequest request = new BrainTreeSaleTransactionRequest(customer.getUid(),
				createCard(orderModel.getPaymentAddress()), null, captureAmount, createBillingInfo(orderModel.getDeliveryAddress()));

		final BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) orderModel.getPaymentInfo();

		request.setUsePaymentMethodToken(Boolean.TRUE);
		request.setPaymentMethodToken(brainTreePaymentInfoModel.getPaymentMethodToken());
		request.setRequestId(authorizeTransactionID);
		return request;
	}

	private String getRequestId(final OrderModel orderModel)
	{
		String requestId = "";
		for (final PaymentTransactionModel paymentTransaction : orderModel.getPaymentTransactions())
		{
			if (paymentTransaction.getRequestId() != null && !paymentTransaction.getRequestId().isEmpty())
			{
				requestId = paymentTransaction.getRequestId();
			}
		}
		return requestId;
	}

	private BillingInfo createBillingInfo(final AddressModel addressModel)
	{
		final BillingInfo shippingInfo = new BillingInfo();
		shippingInfo.setFirstName(addressModel.getFirstname());
		shippingInfo.setLastName(addressModel.getLastname());
		shippingInfo.setEmail(addressModel.getEmail());
		shippingInfo.setPostalCode(addressModel.getPostalcode());
		shippingInfo.setStreet1(addressModel.getLine1());
		shippingInfo.setStreet2(addressModel.getLine2());
		return shippingInfo;
	}

	private CardInfo createCard(final AddressModel addressModel)
	{
		final CardInfo cardInfo = new CardInfo();
		cardInfo.setBillingInfo(createBillingInfo(addressModel));
		return cardInfo;
	}

	@Override
	public BigDecimal getPossibleAmountForCapture(OrderModel orderModel)
	{
		validateParameterNotNull(orderModel, ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE);
		modelService.refresh(orderModel);
		return partialOrderCalculationStrategy.calculateCaptureAmount(orderModel);
	}

	@Override
	public BigDecimal getPossibleAmountForCaptureForAuthorizedTransaction
			(PaymentTransactionModel paymentTransactionModel, BigDecimal totalAmount)
	{
		return partialOrderCalculationStrategy.calculateTransaction(paymentTransactionModel, totalAmount.doubleValue());
	}

	@Override
	public boolean isPartialCapturePossible(OrderModel orderModel)
	{
		validateParameterNotNull(orderModel, ORDER_MODEL_CAN_NOT_BE_NULL_MESSAGE);
		boolean result = brainTreeConfigService.getMultiCaptureEnabled();
		LOG.error("isPartialCapturePossible, result: " + result);
		return result;
	}

	protected void validateParameterNotNull(Object parameter, String nullMessage)
	{
		Preconditions.checkArgument(parameter != null, nullMessage);
	}

	public BraintreePartialOrderCalculationStrategy getPartialOrderCalculationStrategy()
	{
		return partialOrderCalculationStrategy;
	}

	public void setPartialOrderCalculationStrategy(BraintreePartialOrderCalculationStrategy partialOrderCalculationStrategy)
	{
		this.partialOrderCalculationStrategy = partialOrderCalculationStrategy;
	}

	public BrainTreeConfigService getBrainTreeConfigService()
	{
		return brainTreeConfigService;
	}

	public void setBrainTreeConfigService(BrainTreeConfigService brainTreeConfigService)
	{
		this.brainTreeConfigService = brainTreeConfigService;
	}

	public BrainTreePaymentService getBrainTreePaymentService()
	{
		return brainTreePaymentService;
	}

	public void setBrainTreePaymentService(BrainTreePaymentService brainTreePaymentService)
	{
		this.brainTreePaymentService = brainTreePaymentService;
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

	public BrainTreeTransactionService getBrainTreeTransactionService()
	{
		return brainTreeTransactionService;
	}

	public void setBrainTreeTransactionService(BrainTreeTransactionService brainTreeTransactionService)
	{
		this.brainTreeTransactionService = brainTreeTransactionService;
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(ModelService modelService)
	{
		this.modelService = modelService;
	}

	public OrderCompleteProcessService getOrderCompleteProcessService()
	{
		return orderCompleteProcessService;
	}

	public void setOrderCompleteProcessService(OrderCompleteProcessService orderCompleteProcessService)
	{
		this.orderCompleteProcessService = orderCompleteProcessService;
	}
}
