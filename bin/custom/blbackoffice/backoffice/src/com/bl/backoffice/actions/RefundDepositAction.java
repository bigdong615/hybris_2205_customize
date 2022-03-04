package com.bl.backoffice.actions;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;
import org.zkoss.zul.Messagebox;


/**
 * This class is used to refund the deposit amount
 * @author Moumita
 */
public class RefundDepositAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<OrderModel, OrderModel>
{

	@Resource(name = "modelService")
	private ModelService modelService;
	@Resource
	private BrainTreePaymentService brainTreePaymentService;
	@Resource
	private BrainTreeTransactionService brainTreeTransactionService;

	protected static final String SOCKET_OUT_CONTEXT = "blRefundDepositContext";
	private static final String REFUND_ALREADY_PROCESSED = "Refund has already been processed";
	private static final String SUCCESS_MESSAGE = "Deposit Released";
	private static final String ERROR_MESSAGE = "There was an unexpected error, Contact your supervisor";

	/**
	 * The button will be enabled if deposit amount has been charged for the order
	 *
	 * @param actionContext
	 *           the action context
	 * @return the boolean
	 */

	@Override
	public boolean canPerform(final ActionContext<OrderModel> actionContext)
	{
		final OrderModel order = actionContext.getData();

		return (order != null && CollectionUtils.isNotEmpty(order.getDepositPaymentTransactions()) && !order.isNonRefundableDeposit());
	}

	/**
	 * This method is responsible to refund the deposit amount
	 *
	 * @param actionContext
	 *           the action context
	 * @return the action result
	 */
	public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext)
{
		final OrderModel order = actionContext.getData();
		final List<PaymentTransactionModel> paymentTransactions = order.getDepositPaymentTransactions();
		final boolean isRefundPossible = checkRefundPossibility(paymentTransactions);
		final List<AtomicBoolean> refundSuccessful = new ArrayList<>();
		if(isRefundPossible) {
			paymentTransactions.forEach(transaction -> {
				final BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) transaction
						.getInfo();
				if(transaction.isLegacyTransaction()) {
					final Result<Transaction> result = brainTreeTransactionService.issueBlindCredit(transaction.getEntries().get(0), BigDecimal
							.valueOf(paymentInfo.getDepositAmount()));
					if (!result.isSuccess()) {brainTreeTransactionService
						refundSuccessful.add(new AtomicBoolean(Boolean.FALSE));
					}
				} else {
					final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(
							transaction
									.getRequestId());
					request.setAmount(BigDecimal.valueOf(paymentInfo.getDepositAmount())
							.setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN));
					request.setOrderId(order.getCode());
					request.setTransactionId(transaction.getRequestId());
					final BrainTreeRefundTransactionResult brainTreeRefundTransactionResult = brainTreePaymentService
							.refundTransaction(request);
					if (brainTreeRefundTransactionResult.isSuccess()) {
						brainTreeTransactionService.createRefundTransaction(transaction,
								brainTreeRefundTransactionResult);
					} else {
						refundSuccessful.add(new AtomicBoolean(Boolean.FALSE));
					}
				}
			});
			if(refundSuccessful.stream()
					.allMatch(AtomicBoolean::get)) {
				this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
				Messagebox.show(SUCCESS_MESSAGE);
				return new ActionResult(ActionResult.SUCCESS);
			} else {
				Messagebox.show(ERROR_MESSAGE);
				return new ActionResult(ActionResult.ERROR);
			}
		}
			Messagebox.show(REFUND_ALREADY_PROCESSED);
			return new ActionResult(ActionResult.ERROR);
	}

	/**
	 * It checks whether refund has already been processed or not
	 * @param paymentTransactions list of payment transaction
	 * @return true if refund is not processed yet
	 */
	private boolean checkRefundPossibility(final List<PaymentTransactionModel> paymentTransactions) {
		for(PaymentTransactionModel transactionModel : paymentTransactions) {
			for(PaymentTransactionEntryModel transactionEntryModel : transactionModel.getEntries()) {
				if(transactionEntryModel.getType().equals((PaymentTransactionType.REFUND_STANDALONE))) {
					return false;
				}
			}
		}
		return true;
	}

}
