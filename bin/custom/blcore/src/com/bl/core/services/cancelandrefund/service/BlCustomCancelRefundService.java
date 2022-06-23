package com.bl.core.services.cancelandrefund.service;

import com.braintree.command.result.BrainTreeRefundTransactionResult;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.returns.model.RefundEntryModel;
import de.hybris.platform.returns.model.ReturnRequestModel;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;

/**
 * Custom cancel and refund service interface
 *
 * @author Namrata Lohar
 */
public interface BlCustomCancelRefundService {

    /**
     * This method will fetch all refund records associated to order entry from dao
     *
     * @param abstractOrderEntryCode entry code
     * @param originalInstance versioned/extended order entry or not
     * @param orderNumber order code
     * @return list of refund entries transacted for particular order entry
     */
    Collection<RefundEntryModel> getAllRefundEntriesForOrderEntry(final String abstractOrderEntryCode, final String orderNumber,
                                                                  final boolean originalInstance);

    /**
     * This method will fetch all refund records associated to order entry from dao
     *
     * @param originalInstance versioned/extended order entry or not
     * @param orderNumber order model
     * @return map of refund entries transacted for all order entries in order
     */
    Map<String, Collection<RefundEntryModel>> getAllRefundEntriesForOrder(final OrderModel orderNumber, final boolean originalInstance);

    /**
     * This method will create a request object and return result
     *
     * @param transactionId id
     * @param amount amount
     * @param orderId order
     * @return result
     */
    BrainTreeRefundTransactionResult createBrainTreeRefundTransactionRequest(final String transactionId, final BigDecimal amount,
                                                                             final String orderId);

    /**
     * This method will check if payment is captured or not in transactions
     *
     * @param orderModel order
     * @return capture payment transaction
     */
    Optional<PaymentTransactionEntryModel> getCapturedPaymentTransaction(final OrderModel orderModel);

    /**
     * This method will calculate total refunded amount on each entry
     *
     * @param refundEntryModels refunded entries
     * @return total refunded amount on each entry
     */
    double getTotalRefundedAmountOnOrderEntry(final Collection<RefundEntryModel> refundEntryModels);

    /**
     * This method will calculate total refunded amount on order
     *
     * @param orderModel order
     * @return total refunded amount on order
     */
    double getTotalRefundedAmountOnOrder(final OrderModel orderModel);

    /**
     * This method will create PaymentTransactionEntryModel and return
     *  @param transaction transaction
     * @param result refund result
     * @param transactionType refund or partial refund
     * @param orderModel
     */
    void createRefundTransaction(final PaymentTransactionModel transaction, final BrainTreeRefundTransactionResult result,
                                 final PaymentTransactionType transactionType, OrderModel orderModel);

    /**
     * This method will create refund and payment transaction entry
     *
     * @param result refund result
     * @param returnRequestModel request
     * @param orderEntry entry
     * @param qty quantity
     * @param totalPrice amount
     * @param orderModel order
     * @param refundStandalone transaction type
     * @param notes notes
     */
    void createRefundAndPaymentEntry(final BrainTreeRefundTransactionResult result, final ReturnRequestModel returnRequestModel,
                                     final AbstractOrderEntryModel orderEntry, final Long qty, final Double totalPrice,
                                     final OrderModel orderModel, final PaymentTransactionType refundStandalone, final String notes);

    /**
     * This method will calculate amount on checkbox status on popup on order
     *
     * @param tax amount
     * @param waiver cost
     * @param shipping cost
     * @param subTotal order
     * @return amount on checkbox status
     */
    double calculateAmountOnCheckboxStatusFull(final double subTotal, final double tax, final double waiver, final double shipping,
                                               final double amount, final boolean isForGetRefundAmountClick);

    /**
     * This method will calculate amount on checkbox status on popup on entry
     *
     * @param orderModel order
     * @param order entry
     * @param collectionMap checkbox map
     * @param qtyToCancel quantity
     * @return amount on checkbox status
     */
    double calculateAmountOnCheckboxStatusPartial(final OrderModel orderModel, final AbstractOrderEntryModel order,
                                                  final Map<String, Object> collectionMap, final Long qtyToCancel);

    /**
     * This method will create map on checkbox status
     *
     * @param tax amount
     * @param waiver cost
     * @param shipping cost
     * @param amount amount
     * @return values in map
     */
    Map<String, Object> collectSelectionCheckboxAndCreateMap(final boolean tax, final boolean waiver, final Boolean shipping, final Double amount);

    /**
     * This will calculate amount on input and it will return
     *
     * @param cancelQty qty
     * @param cancellableQty available qty
     * @param productPrice basePrice
     * @param taxLabel tax value
     * @param waiverLabel waiver value
     * @return calculated amount
     */
    double getTotalAmountPerEntry(int cancelQty, final int cancellableQty, final double productPrice, final double taxLabel, final double waiverLabel);
}
