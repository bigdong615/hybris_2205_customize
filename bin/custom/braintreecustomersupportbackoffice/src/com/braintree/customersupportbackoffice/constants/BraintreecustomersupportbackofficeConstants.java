/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved
 */
package com.braintree.customersupportbackoffice.constants;

/**
 * Global class for all Ybackoffice constants. You can add global constants for your extension into this class.
 */
public final class BraintreecustomersupportbackofficeConstants extends GeneratedBraintreecustomersupportbackofficeConstants
{
	public static final String EXTENSIONNAME = "braintreecustomersupportbackoffice";

	private BraintreecustomersupportbackofficeConstants()
	{
		//empty to avoid instantiating this constant class
	}

	public interface OrderManagementActions {
		String PARTIAL_CAPTURE_TITLE = "bt.customersupport.widget.order.capture.partial.title";
		String PARTIAL_CAPTURE_SUCCESS = "bt.customersupport.widget.order.capture.partial.message.success";
		String PARTIAL_CAPTURE_ERROR = "bt.customersupport.widget.order.capture.partial.message.error";
		String PARTIAL_CAPTURE_INCORRECT_AMOUNT_ERROR = "bt.customersupport.widget.order.capture.partial.message.error.amount.incorrect";
		String PARTIAL_CAPTURE_AMOUNT_NOT_MATH_ERROR = "bt.customersupport.widget.order.capture.partial.message.error.amount.not.match";
	}

	public interface TransactionManagementActions {
		String WIDGET_MESSAGE_TRANSACTION_SFS_SUCCESS = "bt.customersupport.widget.transaction.transactionmanagement.sfsTransaction.message.success";
		String WIDGET_MESSAGE_TRANSACTION_SFS_ERROR = "bt.customersupport.widget.transaction.transactionmanagement.sfsTransaction.message.error";
		String WIDGET_TRANSACTION_SFS_TITLE = "bt.customersupport.widget.transaction.transactionmanagement.sfsTransaction.title";
		String WIDGET_MESSAGE_TRANSACTION_SFS_REFRESH = "bt.customersupport.widget.transaction.transactionmanagement.sfsTransaction.message.refresh";

		String WIDGET_REFUND_TITLE = "bt.customersupport.widget.transaction.transactionmanagement.refundTransaction.title";
		String WIDGET_MESSAGE_REFUND_SUCCESS = "bt.customersupport.widget.transaction.transactionmanagement.refundTransaction.message.success";
		String WIDGET_MESSAGE_REFUND_ERROR = "bt.customersupport.widget.transaction.transactionmanagement.refundTransaction.message.error";
		String WIDGET_MESSAGE_REFUND_ERROR_GO_TO_ORDER = "bt.customersupport.widget.transaction.transactionmanagement.refundTransaction.message.error.goToOrder";

		String WIDGET_MESSAGE_TRANSACTION_CREATE_SUCCESS_POSTFIX = "bt.customersupport.widget.transaction.transactionmanagement.newTransaction.message.success.postfix";

		String WIDGET_VOID_ASK_TITLE = "bt.customersupport.widget.transaction.transactionmanagement.voidTransaction.ask.title";
		String WIDGET_VOID_ASK_MESSAGE = "bt.customersupport.widget.transaction.transactionmanagement.voidTransaction.ask.message";

		String WIDGET_CLONE_TITLE = "bt.customersupport.widget.transaction.transactionmanagement.cloneTransaction.title";
		String WIDGET_MESSAGE_CLONE_SUCCESS = "bt.customersupport.widget.transaction.transactionmanagement.cloneTransaction.message.success";
		String WIDGET_MESSAGE_CLONE_ERROR = "bt.customersupport.widget.transaction.transactionmanagement.cloneTransaction.message.error";
	}

	public interface PartialRefundOrderAction {
		String WIDGET_PARTIAL_REFUND_TITLE = "braintree.backoffice.partialRefundTransaction.title.status";
		String WIDGET_MESSAGE_PARTIAL_REFUND_ERROR = "braintree.backoffice.partialRefundTransaction.title.message.error";
		String WIDGET_MESSAGE_PARTIAL_REFUND_SUCCESS = "braintree.backoffice.partialRefundTransaction.message.success";
		String WIDGET_MESSAGE_PARTIAL_REFUND_CREATE_SUCCESS_POSTFIX = "braintree.backoffice.partialRefundTransaction.message.postfix";
		String WIDGET_MESSAGE_AMOUNT_EMPTY="braintree.backoffice.partialRefundTransaction.amount.error.empty";
		String WIDGET_MESSAGE_AMOUNT_ZERO="braintree.backoffice.partialRefundTransaction.amount.error.zero";
		String WIDGET_MESSAGE_AMOUNT_NUMBER_FORMAT="braintree.backoffice.partialRefundTransaction.amount.error.number.format";
	}

	public interface BraintreeVoidAction{
		String WIDGET_VOID_TRANSACTION_TITLE = "braintree.backoffice.voidTransaction.title.status";
	}
}
