package com.braintree.actions.order;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.payment.CreditCardPaymentInfoModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.payment.PaymentService;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.processengine.action.AbstractAction;
import de.hybris.platform.task.RetryLaterException;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;

import java.util.HashSet;
import java.util.Set;


public class BrainTreeTakePaymentAction extends AbstractAction<OrderProcessModel>
{
	private static final Logger LOG = Logger.getLogger(BrainTreeTakePaymentAction.class);

	public enum Transition
	{
		OK, NOK, UNDEFINED;

		public static Set<String> getStringValues()
		{
			final Set<String> res = new HashSet<String>();
			for (final Transition transitions : Transition.values())
			{
				res.add(transitions.name());
			}
			return res;
		}
	}


	private PaymentService paymentService;

	private BrainTreeConfigService configService;

	@Override
	public Set<String> getTransitions()
	{
		return OrderManualCheckedAction.Transition.getStringValues();
	}

	@Override
	public final String execute(final OrderProcessModel process) throws RetryLaterException, Exception
	{
		return executeAction(process).toString();
	}

	protected Transition executeAction(final OrderProcessModel process)
	{
		final OrderModel order = process.getOrder();

		LOG.error("isCaptureRequired(order): " + isCaptureRequired(order));

		if (isCaptureRequired(order))
		{
			for (final PaymentTransactionModel txn : order.getPaymentTransactions())
			{
				final PaymentTransactionEntryModel txnEntry = getPaymentService().capture(txn);
				if (TransactionStatus.ACCEPTED.name().equals(txnEntry.getTransactionStatus()))
				{
					if (LOG.isDebugEnabled())
					{
						LOG.debug("The payment transaction has been captured. Order: " + order.getCode() + ". Txn: " + txn.getCode());
					}
					setOrderStatus(order, OrderStatus.PAYMENT_CAPTURED);
					break;
				}
				else if (TransactionStatus.REVIEW.name().equals(txnEntry.getTransactionStatus()))
				{
					setOrderStatus(order, OrderStatus.PARTIAL_CAPTURE);
					LOG.debug("The payment transaction should to be captured. Order: " + order.getCode() + ". Txn: " + txn.getCode());
					return Transition.UNDEFINED;
				}
				else if (checkForInvalidCaptureTransactions(txnEntry))
				{
					LOG.error("The payment transaction capture has failed. Order: " + order.getCode() + ". Txn: " + txn.getCode() + ". Waiting for submitForSettlement action.");
					return Transition.UNDEFINED;
				}
				else
				{
					LOG.error("The payment transaction capture has failed. Order: " + order.getCode() + ". Txn: " + txn.getCode());
					setOrderStatus(order, OrderStatus.PAYMENT_NOT_CAPTURED);
					return Transition.NOK;
				}
			}
		}
		else {
			LOG.info("Return result as: " + Transition.UNDEFINED);
			return Transition.UNDEFINED;
		}
		LOG.info("Return result as: " + Transition.OK);
		return Transition.OK;
	}

	private boolean checkForInvalidCaptureTransactions(PaymentTransactionEntryModel transactionEntry)
	{
		return TransactionStatus.REJECTED.name().equals(transactionEntry.getTransactionStatus())
				&& !TransactionStatusDetails.BANK_DECLINE.name().equals(transactionEntry.getTransactionStatusDetails());
	}

	private final boolean isCaptureRequired(final OrderModel order) {

		if (order.getPaymentInfo() instanceof BrainTreePaymentInfoModel) {
			String paymentProvider = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider();
			if (BraintreeConstants.BRAINTREE_PAYMENT.equals(paymentProvider) || BraintreeConstants.VENMO_CHECKOUT
					.equals(paymentProvider) || BraintreeConstants.ANDROID_PAY_CARD.equals(paymentProvider)) {
				return true;
			}
			String intent = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPayPalIntent();
			if (BraintreeConstants.PAYPAL_INTENT_ORDER.equalsIgnoreCase(intent)) {
				return false;
			}
			if (BraintreeConstants.PAYPAL_INTENT_SALE.equalsIgnoreCase(intent)) {
				return true;
			}

		}
		return order.getPaymentInfo() instanceof CreditCardPaymentInfoModel
				|| order.getPaymentInfo() instanceof BrainTreePaymentInfoModel;
	}


	protected PaymentService getPaymentService()
	{
		return paymentService;
	}

	@Required
	public void setPaymentService(final PaymentService paymentService)
	{
		this.paymentService = paymentService;
	}

	public BrainTreeConfigService getConfigService() {
		return configService;
	}

	public void setConfigService(BrainTreeConfigService configService) {
		this.configService = configService;
	}

}