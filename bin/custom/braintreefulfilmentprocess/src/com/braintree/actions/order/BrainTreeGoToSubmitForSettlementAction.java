package com.braintree.actions.order;


import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.enums.BrainTreePaymentMethod;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.task.RetryLaterException;
import org.apache.log4j.Logger;

public class BrainTreeGoToSubmitForSettlementAction extends AbstractSimpleDecisionAction<OrderProcessModel> {

    private static final Logger LOG = Logger.getLogger(BrainTreeGoToSubmitForSettlementAction.class);

    private BrainTreeConfigService brainTreeConfigService;

    @Override
    public Transition executeAction(OrderProcessModel orderProcessModel) throws RetryLaterException, Exception {
        OrderModel order = orderProcessModel.getOrder();

        if (order.getPaymentInfo() != null && order.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
        {
            if (canCreateCaptureTransactionForOrder(order))
            {
                LOG.info("[BRAINTREE ORDER PROCESS] Order process (orderProcess.code " + orderProcessModel.getCode() + ") will be captured. order.code: " + order.getCode());
                return Transition.OK;
            }
            LOG.info("[BRAINTREE ORDER PROCESS] Order process (code " + orderProcessModel.getCode() + ") is waiting for submitForSettlement event.");
            return Transition.NOK;
        }

        LOG.warn("[BRAINTREE ORDER PROCESS] PayPal intent not found for this order. Order process is stopped and waiting for submitForSettlement event, order.code: " + order.getCode());
        return Transition.OK;
    }

    private boolean canCreateCaptureTransactionForOrder(OrderModel order)
    {
        String paymentProvider = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider();
        String intent = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPayPalIntent();
        if (BraintreeConstants.APPLE_PAY_PAYMENT.equals(paymentProvider))
        {
            return getBrainTreeConfigService().getSettlementConfigParameter();
        }
        if (BraintreeConstants.ANDROID_PAY_CARD.equals(paymentProvider))
        {
            return getBrainTreeConfigService().getSettlementConfigParameter();
        }
        if (BraintreeConstants.BRAINTREE_PAYMENT.equals(paymentProvider))
        {
            return getBrainTreeConfigService().getSettlementConfigParameter();
        }
        if (BraintreeConstants.VENMO_CHECKOUT.equals(paymentProvider))
        {
	         return getBrainTreeConfigService().getSettlementConfigParameter();
        }
        if (BraintreeConstants.PAYPAL_PAYMENT.equals(paymentProvider) || BrainTreePaymentMethod.PAYPAL.getCode()
              .equals(paymentProvider) || BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT.equals(paymentProvider))
        {
            LOG.error("intent: " + intent);
            return BraintreeConstants.PAYPAL_INTENT_SALE.equalsIgnoreCase(intent) || BraintreeConstants.PAYPAL_INTENT_ORDER
                  .equalsIgnoreCase(intent);
        }
        if (BraintreeConstants.LOCAL_PAYMENT.equals(paymentProvider))
        {
            return BraintreeConstants.PAYPAL_INTENT_SALE.equalsIgnoreCase(intent);
        }
        LOG.warn("[BRAINTREE ORDER PROCESS] Unknown payment provider: " + paymentProvider);
        return false;
    }

    public BrainTreeConfigService getBrainTreeConfigService() {
        return brainTreeConfigService;
    }

    public void setBrainTreeConfigService(BrainTreeConfigService brainTreeConfigService) {
        this.brainTreeConfigService = brainTreeConfigService;
    }
}

