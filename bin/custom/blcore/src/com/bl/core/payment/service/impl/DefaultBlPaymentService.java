package com.bl.core.payment.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.payment.service.BlPaymentService;
import com.bl.logging.BlLogger;
import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import com.braintreegateway.Transaction;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.math.BigDecimal;
import java.util.*;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;


/**
 * This is used to create auth and capture transaction
 * @author Moumita
 */
public class DefaultBlPaymentService implements BlPaymentService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlPaymentService.class);
	private BlOrderDao orderDao;
	private BrainTreeTransactionService brainTreeTransactionService;
	private ModelService modelService;
	private BrainTreePaymentService brainTreePaymentService;


	/**
	 * {@inheritDoc}
	 */
	@Override
	public void authorizePaymentForOrders()
	{
		final List<AbstractOrderModel> ordersToAuthorizePayment = getOrderDao().getOrdersForAuthorization();
		final Set<AbstractOrderModel> ordersToAuthPayment = new HashSet<>(ordersToAuthorizePayment);
		ordersToAuthPayment.forEach(order -> {
			if(order.getTotalPrice() > 0) {
				final boolean isSuccessAuth = getBrainTreeTransactionService().createAuthorizationTransactionOfOrder(order,
						BigDecimal.valueOf(order.getTotalPrice()), Boolean.FALSE, null);
				if (isSuccessAuth) {
					order.setIsAuthorised(Boolean.TRUE);
					order.setIsAuthorizationAttempted(true);
					getModelService().save(order);
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Auth is successful for the order {}", order.getCode());
				} else {
					order.setIsAuthorizationAttempted(true);
					order.setStatus(OrderStatus.PAYMENT_NOT_AUTHORIZED);
					modelService.save(order);
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Auth is not successful for the order {}", order.getCode());
				}
			} else {
				setIsAuthorizedFlagForGiftCard(order);
			}
		});
	}

	/**
	 * {@inheritDoc}
	 * @return
	 */
	@Override
	public boolean capturePaymentForOrder(final OrderModel order) {
		try {
			final PaymentTransactionEntryModel authEntry = getAUthEntry(order);
			if(CollectionUtils.isNotEmpty(order.getGiftCard()) && Double.compare(order.getTotalPrice(), 0.0) == 0) {
				BlLogger.logMessage(LOG, Level.INFO, "The total amount is 0 on this order {} as gift card has been applied", order.getCode());
				return Boolean.TRUE;
			}
			else if(authEntry != null && authEntry.getAmount().intValue() > BlInventoryScanLoggingConstants.ONE) {
				BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Before payment capture order total is {} and auth amount is {} for the order {} ", String.valueOf(order.getTotalPrice()),authEntry.getAmount().intValue(),order.getCode());
				      if(Double.compare(authEntry.getAmount().doubleValue(),order.getTotalPrice())==0) {
					     BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Authorized amount and order total are same for the order {}",order.getCode());
					       return getBrainTreeTransactionService().captureAuthorizationTransaction(
							  order, authEntry.getAmount(), authEntry.getRequestId());
				        }
				      else {
						  BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Before voiding the authorized amount for the order {}",order.getCode());
					        //Voiding the previous authorized amount which is not matching with current order total.
						    final VoidRequest voidRequest = new VoidRequest(order.getUser().getUid(), authEntry.getRequestId(), StringUtils.EMPTY, StringUtils.EMPTY);
						    final BrainTreeVoidResult voidResult = getBrainTreePaymentService().voidTransaction(voidRequest);
						    if (voidResult.isSuccess()) {
							authEntry.setTransactionStatus(Transaction.Status.VOIDED.name());
							modelService.save(authEntry);
							} else {
								LOG.error("Error, message: " + voidResult.getErrorMessage());
								throw new BraintreeErrorException(voidResult.getErrorMessage(), voidResult.getTransactionId());
							}
							//Creating new authorization transaction with the current order total.
						    return createNewAuthorizationAndCapture(order);

				            }
			}
			else if( authEntry ==null && CollectionUtils.isNotEmpty(order.getGiftCard()))
			{
				BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Creating authorization entry when gift card applied order is modified for the order {}",order.getCode());
				//Creating new authorization transaction with the current order total for gift card applied order.
				return createNewAuthorizationAndCapture(order);
			}
			else
			{
				BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Authorization entry is present only for $1 for the order {}",order.getCode());
				if(order.getTotalPrice() > BlInventoryScanLoggingConstants.ZERO) {
					return getBrainTreeTransactionService().createAuthorizationTransactionOfOrder(
							order, BigDecimal.valueOf(order.getTotalPrice()), Boolean.TRUE, null);
				}
			}
		} catch(final BraintreeErrorException ex) {
			order.setStatus(OrderStatus.RECEIVED_PAYMENT_DECLINED);
			modelService.save(order);
			BlLogger.logMessage(LOG, Level.ERROR, "BraintreeErrorException occurred while capturing "
					+ "the payment for order {} ", order.getCode(), ex);
		} catch(final Exception ex) {
			BlLogger.logMessage(LOG, Level.ERROR, "Exception occurred while capturing "
					+ "the payment for order {} ", order.getCode(), ex);
		}
		return false;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void voidAuthTransaction() {
		final List<OrderModel> orders = getOrderDao().getOrdersToVoidTransactions();
		orders.stream().forEach(order ->
				getBrainTreeTransactionService().voidAuthTransaction(order));
	}

	/**
	 *
	 * @param order
	 * @return
	 * @throws BraintreeErrorException
	 */
	public boolean createNewAuthorizationAndCapture(OrderModel order) throws BraintreeErrorException {

		BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Creating new authorization for capture payment for the order {}",order.getCode());
		getBrainTreeTransactionService().createAuthorizationTransactionOfOrder(order,
				BigDecimal.valueOf(order.getTotalPrice()),
				Boolean.FALSE, null);

		final PaymentTransactionEntryModel newAuthEntry = getAUthEntry(order);
		BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Capturing new authorized amount {} for the order {}",newAuthEntry.getAmount(),order.getCode());
		return getBrainTreeTransactionService().captureAuthorizationTransaction(
				order, newAuthEntry.getAmount(), newAuthEntry.getRequestId());
	}

	/**
	 * It gets the authorization entry of the order
	 * @param order
	 */
	private PaymentTransactionEntryModel getAUthEntry(final OrderModel order) {
		final List<PaymentTransactionModel> transactions = order.getPaymentTransactions();
		if(CollectionUtils.isNotEmpty(transactions)) {
			final List<PaymentTransactionEntryModel> transactionEntries = transactions.get(0).getEntries();
			final Optional<PaymentTransactionEntryModel> authEntry = transactionEntries.stream()
					.filter(transactionEntry ->
							transactionEntry.getType().equals(PaymentTransactionType.AUTHORIZATION))
					.reduce((first, second) -> second);
			if (authEntry.isPresent() && TransactionStatus.ACCEPTED.name()
					.equals(authEntry.get().getTransactionStatus())) {
				return authEntry.get();
			}
		}
		return null;
	}

	/**
	 * It sets isAuthorized flag as true when order total price is 0
	 * @param order
	 */
	private void setIsAuthorizedFlagForGiftCard(final AbstractOrderModel order) {
		if(CollectionUtils.isNotEmpty(order.getGiftCard()) && (BigDecimal.valueOf(order
				.getTotalPrice())).compareTo(BigDecimal.ZERO) == 0) {
			order.setIsAuthorised(Boolean.TRUE);
			getModelService().save(order);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "The order {} has been paid fully via Gift card", order.getCode());
		}
	}

	/**
	 * @return the blOrderDao
	 */
	public BlOrderDao getOrderDao()
	{
		return orderDao;
	}

	/**
	 * @param orderDao
	 *           the blOrderDao to set
	 */
	public void setOrderDao(final BlOrderDao orderDao)
	{
		this.orderDao = orderDao;
	}

	/**
	 * @return the brainTreeTransactionService
	 */
	public BrainTreeTransactionService getBrainTreeTransactionService()
	{
		return brainTreeTransactionService;
	}

	/**
	 * @param brainTreeTransactionService
	 *           the brainTreeTransactionService to set
	 */
	public void setBrainTreeTransactionService(final BrainTreeTransactionService brainTreeTransactionService)
	{
		this.brainTreeTransactionService = brainTreeTransactionService;
	}

	public ModelService getModelService() {
		return modelService;
	}

	public void setModelService(final ModelService modelService) {
		this.modelService = modelService;
	}

	public BrainTreePaymentService getBrainTreePaymentService() {
		return brainTreePaymentService;
	}

	public void setBrainTreePaymentService(BrainTreePaymentService brainTreePaymentService) {
		this.brainTreePaymentService = brainTreePaymentService;
	}


}
