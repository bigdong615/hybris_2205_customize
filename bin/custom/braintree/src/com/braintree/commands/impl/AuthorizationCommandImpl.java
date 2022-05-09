package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeAuthorizationRequest;
import com.braintree.command.request.beans.BrainTreeLineItemBean;
import com.braintree.command.result.BrainTreeAuthorizationResult;
import com.braintree.constants.BraintreeConstants;
import com.braintree.enums.BrainTreePaymentMethod;

import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.braintreegateway.TransactionDescriptorRequest;
import com.braintreegateway.TransactionLineItem;
import com.braintreegateway.TransactionLineItemRequest;
import com.braintreegateway.TransactionOptionsRequest;
import com.braintreegateway.TransactionRequest;
import com.braintreegateway.ValidationError;
import de.hybris.platform.order.CartService;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.commands.AuthorizationCommand;
import de.hybris.platform.payment.commands.request.AuthorizationRequest;
import de.hybris.platform.payment.commands.result.AuthorizationResult;
import de.hybris.platform.payment.dto.AvsStatus;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.payment.dto.CvnStatus;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Currency;
import java.util.Date;
import java.util.List;


public class AuthorizationCommandImpl extends AbstractCommand<AuthorizationRequest, AuthorizationResult>
		implements AuthorizationCommand
{
	private final static Logger LOG = Logger.getLogger(AuthorizationCommandImpl.class);

	private static final String AUTHORIZATION_TRANSACTION = "[AUTHORIZATION TRANSACTION] ";
	private static final String PROPERTY_LEVEL2_LEVEL3 = "braintree.enable.level2.level3.data";

	private CartService cartService;
	private ConfigurationService configurationService;


	@Override
	public AuthorizationResult perform(final AuthorizationRequest authorizationRequest)
	{
		LOG.info("configured intent: " + getBrainTreeConfigService().getIntent());

		final TransactionRequest transactionRequest = translateRequest(authorizationRequest);

		final Result<Transaction> braintreeReply = getBraintreeGateway().transaction().sale(transactionRequest);

		LOG.info("braintreeReply: " + braintreeReply);

		final AuthorizationResult authorizationResult = translateResponse(braintreeReply);
		return authorizationResult;
	}

	private TransactionRequest translateRequest(final AuthorizationRequest authorizationRequest)
	{
		TransactionRequest request = null;

		if (authorizationRequest instanceof BrainTreeAuthorizationRequest)
		{
			final BrainTreeAuthorizationRequest brainTreeAuthorizationRequest = (BrainTreeAuthorizationRequest) authorizationRequest;

			request = new TransactionRequest().customerId(brainTreeAuthorizationRequest.getCustomerId())
					.amount(authorizationRequest.getTotalAmount());

			setAdditionalParameters(brainTreeAuthorizationRequest, request);

			if (configurationService.getConfiguration().getBoolean(PROPERTY_LEVEL2_LEVEL3, Boolean.FALSE))
			{
				//                validate Level2 fields, if valid then apply to request
				if (isLevel2Applicable(brainTreeAuthorizationRequest))
				{
					//					LOG.info("Apply Level2 data, order: " + brainTreeAuthorizationRequest.getPurchaseOrderNumber());
					//					request.purchaseOrderNumber(brainTreeAuthorizationRequest.getPurchaseOrderNumber());
					request.taxAmount(roundNumberToTwoDecimalPlaces(brainTreeAuthorizationRequest.getTaxAmountAuthorize()));

					//                    validate Level3 fields, if valid then apply to request
					if (isLevel3Applicable(brainTreeAuthorizationRequest))
					{
						LOG.info("Apply Level3 data, order: " + brainTreeAuthorizationRequest.getPurchaseOrderNumber());

						request.shippingAmount(roundNumberToTwoDecimalPlaces(brainTreeAuthorizationRequest.getShippingAmount()));
						request.discountAmount(roundNumberToTwoDecimalPlaces(brainTreeAuthorizationRequest.getDiscountAmount()));
						request.shipsFromPostalCode(brainTreeAuthorizationRequest.getShipsFromPostalCode());

						request.shippingAddress().postalCode(brainTreeAuthorizationRequest.getShippingPostalCode())
								.countryCodeAlpha3(brainTreeAuthorizationRequest.getShippingCountryCodeAlpha3())
								.done();

						for (BrainTreeLineItemBean bean : brainTreeAuthorizationRequest.getLineItems())
						{
							TransactionLineItemRequest t = request.lineItem();

							t.name(bean.getName()).kind(TransactionLineItem.Kind.DEBIT).quantity(bean.getQuantity())
									.unitAmount(bean.getUnitAmount())
									.unitOfMeasure(bean.getUnitOfMeasure()).totalAmount(bean.getTotalAmount())
									.taxAmount(bean.getTaxAmount()).discountAmount(bean.getDiscountAmount())
									.productCode(bean.getProductCode()).commodityCode(bean.getCommodityCode()).done();
						}
					}
				}
			}

			getLoggingHandler().handleAuthorizationRequest(brainTreeAuthorizationRequest);

		}
		else
		{
			final String errorMessage = "[BT Authorization Error] Authorization Request must be Brain Tree type!";
			getLoggingHandler().getLogger().error(errorMessage);
			throw new AdapterException(errorMessage);
		}
		return request;
	}

	private boolean isLevel2Applicable(final BrainTreeAuthorizationRequest request)
	{
		boolean isOrderNumberValid = StringUtils.isAsciiPrintable(request.getPurchaseOrderNumber())
				&& (request.getPurchaseOrderNumber().length() <= 12);
		boolean isTaxAmountValid = request.getTaxAmountAuthorize().compareTo(new Double(0d)) >= 0;

		return isOrderNumberValid && isTaxAmountValid;
	}

	private boolean isLevel3Applicable(final BrainTreeAuthorizationRequest request)
	{
		String paymentVia = request.getPaymentType();

		if (paymentVia.equalsIgnoreCase(BrainTreePaymentMethod.PAYPAL.toString()) || paymentVia
				.equalsIgnoreCase(BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT))
		{
			//            Level3 disabled intentionally for PayPal payments, please contact Braintree for details
			return false;
		}
		return true;
	}

	private BigDecimal roundNumberToTwoDecimalPlaces(final Double number)
	{
		return new BigDecimal(number.doubleValue()).setScale(2, RoundingMode.HALF_UP);
	}

	private void setAdditionalParameters(final BrainTreeAuthorizationRequest brainTreeAuthorizationRequest,
			final TransactionRequest request)
	{
		TransactionOptionsRequest options = request.options();
		boolean isAlreadyVaulted = brainTreeAuthorizationRequest.getUsePaymentMethodToken();
		if (isAlreadyVaulted)
		{
			if (getBrainTreeConfigService().get3dSecureConfiguration() && brainTreeAuthorizationRequest
					.getThreeDSecureConfiguration() && BraintreeConstants.BRAINTREE_CREDITCARD_PAYMENT
					.equals(brainTreeAuthorizationRequest.getPaymentType()))
			{
				request.paymentMethodNonce(brainTreeAuthorizationRequest.getMethodNonce());
			}
			else
			{
				ServicesUtil.validateParameterNotNull(brainTreeAuthorizationRequest.getPaymentMethodToken(),
						"Error: PaymentMethodToken is null!");
				request.paymentMethodToken(brainTreeAuthorizationRequest.getPaymentMethodToken());
				request.customerId(null);
			}
		}
		else
		{
			request.paymentMethodNonce(brainTreeAuthorizationRequest.getMethodNonce());
		}

		Boolean submitForSettlement = brainTreeAuthorizationRequest.getSubmitForSettlement();
// As the auth transaction will not be settled immediately, commenting out the below code
/*		if (isAvailableSubmitForSettlement(brainTreeAuthorizationRequest) || isAvailableSubmitForSettlementForPaymentProvider( //NOSONAR
				brainTreeAuthorizationRequest)){submitForSettlement = Boolean.TRUE;} */ //NOSONAR

		if (isVenmoPayment(brainTreeAuthorizationRequest)
				&& StringUtils.isNotEmpty(brainTreeAuthorizationRequest.getVenmoProfileId()))
		{
			request.options().venmo().profileId(brainTreeAuthorizationRequest.getVenmoProfileId());
		}

		String storeInVaultForCard =
				getBrainTreeConfigService().getStoreInVaultForCardVaulting(brainTreeAuthorizationRequest.getCustomerId());
		if (isAlreadyVaulted)
		{
			options.storeInVault(Boolean.FALSE);
		}
		else if(Boolean.parseBoolean(storeInVaultForCard))
		{
			options.storeInVaultOnSuccess(Boolean.TRUE);
		}
		else
		{
			options.storeInVault(Boolean.FALSE);
		}

		options.submitForSettlement(submitForSettlement);


		if ((BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT.equals(brainTreeAuthorizationRequest.getPaymentType())
				|| BraintreeConstants.PAYPAL_PAYMENT.equals(brainTreeAuthorizationRequest.getPaymentType()))
				&& Boolean.FALSE.equals(Boolean.valueOf(brainTreeAuthorizationRequest.isStoreInVault()))
				&& Boolean.TRUE.equals(brainTreeAuthorizationRequest.getAdvancedFraudTools()))
		{
			request.deviceData(brainTreeAuthorizationRequest.getDeviceData());
		}
		if (BraintreeConstants.BRAINTREE_PAYMENT.equals(brainTreeAuthorizationRequest.getPaymentType()))
		{
			if (isThreeDSRequired(brainTreeAuthorizationRequest))
			{
				boolean threeDSecureRequired = true;
				if (BooleanUtils.isTrue(brainTreeAuthorizationRequest.getIsSkip3dSecureLiabilityResult())
						&& BooleanUtils.isFalse(brainTreeAuthorizationRequest.getLiabilityShifted()))
				{
					threeDSecureRequired = false;
				}
				options.threeDSecure().required(threeDSecureRequired);
			}
			if (Boolean.TRUE.equals(brainTreeAuthorizationRequest.getAdvancedFraudTools()))
			{
				request.deviceData(brainTreeAuthorizationRequest.getDeviceData());
			}
		}

		if ((StringUtils.isNotBlank(brainTreeAuthorizationRequest.getCreditCardStatementName())))
		{
			request.descriptor().name(brainTreeAuthorizationRequest.getCreditCardStatementName()).done();
		}

		final BillingInfo shippingInfo = brainTreeAuthorizationRequest.getShippingInfo();
		final BillingInfo billingInfo = brainTreeAuthorizationRequest.getBillingInfo();

		if (StringUtils.isNotEmpty(brainTreeAuthorizationRequest.getBrainTreeBilligAddressId()))
		{
			request.billingAddressId(brainTreeAuthorizationRequest.getBrainTreeBilligAddressId());
		}
		else
		{
			request.billingAddress().countryCodeAlpha2(billingInfo.getCountry()).region(billingInfo.getState())
					.firstName(billingInfo.getFirstName()).lastName(billingInfo.getLastName()).streetAddress(billingInfo.getStreet1())
					.extendedAddress(billingInfo.getStreet2()).locality(billingInfo.getCity()).postalCode(billingInfo.getPostalCode());
		}

		if (StringUtils.isNotEmpty(brainTreeAuthorizationRequest.getBrainTreeAddressId()))
		{
			request.shippingAddressId(brainTreeAuthorizationRequest.getBrainTreeAddressId());
		}
		else
		{
			request.shippingAddress().countryCodeAlpha2(shippingInfo.getCountry()).region(shippingInfo.getState())
					.firstName(shippingInfo.getFirstName()).lastName(shippingInfo.getLastName())
					.streetAddress(shippingInfo.getStreet1()).extendedAddress(shippingInfo.getStreet2())
					.locality(shippingInfo.getCity()).postalCode(shippingInfo.getPostalCode());
		}

		request.channel(getBrainTreeConfigService().getBrainTreeChannel());
		brainTreeAuthorizationRequest.setBrainTreeChannel(getBrainTreeConfigService().getBrainTreeChannel());
		request.merchantAccountId(brainTreeAuthorizationRequest.getMerchantAccountIdForCurrentSite());

		setCustomFields(brainTreeAuthorizationRequest, request);
		TransactionDescriptorRequest descriptor = request.descriptor();
		descriptor.name(BraintreeConstants.NAME);
		// As URL can not be more than 13 characters
		// descriptor.url(BraintreeConstants.URL);
		descriptor.phone(BraintreeConstants.PHONE_NUMBER);

		setPayee(options);

	}

	private boolean isThreeDSRequired(BrainTreeAuthorizationRequest brainTreeAuthorizationRequest)
	{
		return BooleanUtils.isTrue(brainTreeAuthorizationRequest.getThreeDSecureConfiguration())
				&& (!(brainTreeAuthorizationRequest.getUsePaymentMethodToken()))
				&& getBrainTreeConfigService().get3dSecureConfiguration();
	}

	private void setPayee(TransactionOptionsRequest options)
	{
		String payee = StringUtils.EMPTY;
		if (StringUtils.isEmpty(getBrainTreeConfigService().getIntent()) || isIntentSale())
		{
			options.payeeEmail(payee);
		}
	}

	private void setCustomFields(BrainTreeAuthorizationRequest brainTreeAuthorizationRequest, TransactionRequest request)
	{
		if (brainTreeAuthorizationRequest.getCustomFields() != null && !brainTreeAuthorizationRequest.getCustomFields().isEmpty())
		{
			for (String key : brainTreeAuthorizationRequest.getCustomFields().keySet())
			{
				request.customField(key, brainTreeAuthorizationRequest.getCustomFields().get(key));
			}
		}
	}

	private boolean isAvailableSubmitForSettlement(final BrainTreeAuthorizationRequest brainTreeAuthorizationRequest)
	{
		return (isCreditCard(brainTreeAuthorizationRequest) && getBrainTreeConfigService().getSettlementConfigParameter())
				|| (isIntentSale() && !isCreditCard(brainTreeAuthorizationRequest) && !isApplePay(brainTreeAuthorizationRequest)
				&& !isGooglePayPayment(brainTreeAuthorizationRequest) && !isVenmoPayment(brainTreeAuthorizationRequest));
	}

	private boolean isIntentSale()
	{
		return BraintreeConstants.PAYPAL_INTENT_SALE.equalsIgnoreCase(getBrainTreeConfigService().getIntent());
	}

	private boolean isApplePay(BrainTreeAuthorizationRequest brainTreeAuthorizationRequest)
	{
		return BraintreeConstants.APPLE_PAY_PAYMENT.equalsIgnoreCase(brainTreeAuthorizationRequest.getPaymentType());
	}

	private boolean isCreditCard(BrainTreeAuthorizationRequest brainTreeAuthorizationRequest)
	{
		return BraintreeConstants.BRAINTREE_PAYMENT.equalsIgnoreCase(brainTreeAuthorizationRequest.getPaymentType());
	}

	private boolean isVenmoPayment(BrainTreeAuthorizationRequest brainTreeAuthorizationRequest)
	{
		return BraintreeConstants.VENMO_CHECKOUT.equalsIgnoreCase(brainTreeAuthorizationRequest.getPaymentType());
	}

	private boolean isGooglePayPayment(BrainTreeAuthorizationRequest brainTreeAuthorizationRequest)
	{
		return BraintreeConstants.ANDROID_PAY_CARD.equalsIgnoreCase(brainTreeAuthorizationRequest.getPaymentType());
	}

	private boolean isAvailableSubmitForSettlementForPaymentProvider(BrainTreeAuthorizationRequest brainTreeAuthorizationRequest)
	{
		return (isApplePay(brainTreeAuthorizationRequest) || isGooglePayPayment(brainTreeAuthorizationRequest)
				|| isVenmoPayment(brainTreeAuthorizationRequest))
				&& getBrainTreeConfigService().getSettlementConfigParameter();
	}

	private AuthorizationResult translateResponse(final Result<Transaction> braintreeReply)
	{
		final BrainTreeAuthorizationResult result = new BrainTreeAuthorizationResult();
		result.setTransactionStatus(TransactionStatus.REJECTED);
		if (braintreeReply != null)
		{
			result.setSuccess(braintreeReply.isSuccess());

			final Transaction transaction = braintreeReply.getTarget();
			List<ValidationError> errors = null;

			if (braintreeReply.isSuccess())
			{
				if (transaction != null)
				{
					result.setAuthorizationCode(transaction.getProcessorAuthorizationCode());
					result.setAvsStatus(AvsStatus.MATCHED);
					result.setCvnStatus(CvnStatus.MATCHED);

					if (transaction.getAmount() != null)
					{
						result.setTotalAmount(transaction.getAmount());
					}

					result.setAuthorizationTime(transaction.getCreatedAt().getTime());
					result.setCurrency(Currency.getInstance(transaction.getCurrencyIsoCode()));
					result.setMerchantTransactionCode(transaction.getMerchantAccountId());
					result.setRequestId(transaction.getId());
					result.setRequestToken(transaction.getId());
					result.setTransactionStatus(TransactionStatus.ACCEPTED);
					result.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
					result.setAuthorizationExpiresAt(transaction.getAuthorizationExpiresAt());
					if (transaction.getPayPalDetails() != null){

						result.setPaymentMethodToken(transaction.getPayPalDetails().getImplicitlyVaultedPaymentMethodToken());
					}
					if (transaction.getAndroidPayDetails() != null)
					{
						result.setAndroidPayDetails(transaction.getAndroidPayDetails());
					}
					else
					{
						result.setCreditCard(transaction.getCreditCard());
					}
				}
				else
				{
					result.setTransactionStatusDetails(TransactionStatusDetails.BANK_DECLINE);
				}
			}
			else if (braintreeReply.getErrors() != null)
			{
				if (null == braintreeReply.getTransaction())
				{
					result.setRequestId("N/A");
					result.setAuthorizationTime(new Date()); // in this case timestamp not available in the response
				}
				else
				{
					result.setRequestId(braintreeReply.getTransaction().getId());
					result.setAuthorizationTime(braintreeReply.getTransaction().getCreatedAt().getTime());
					result.setTotalAmount(braintreeReply.getTransaction().getAmount());
					result.setCurrency(Currency.getInstance(braintreeReply.getTransaction().getCurrencyIsoCode()));
				}

				final StringBuilder errorMessage = new StringBuilder("[ERROR AUTHORIZATION] ");
				final StringBuilder errorMessageReason = new StringBuilder();
				if (braintreeReply.getErrors().getAllDeepValidationErrors() != null
						&& braintreeReply.getErrors().getAllDeepValidationErrors().size() > 0)
				{
					result.setTransactionStatusDetails(getCodeTranslator()
							.translateReasonCode(braintreeReply.getErrors().getAllDeepValidationErrors().get(0).getCode().code));

					errors = braintreeReply.getErrors().getAllDeepValidationErrors();
					errorMessage.append(getLoggingHandler().handleErrors(errors));
					errorMessageReason
							.append(getErrorTranslator().getMessage(braintreeReply.getErrors().getAllDeepValidationErrors().get(0)));
				}
				if (result.getTransactionStatusDetails() == null)
				{
					result.setTransactionStatusDetails(TransactionStatusDetails.NO_AUTHORIZATION_FOR_SETTLEMENT);
					errorMessage.append(braintreeReply.getMessage());
					//					errorMessageReason.append("(");
					errorMessageReason.append(braintreeReply.getMessage());
					//					errorMessageReason.append(")");
				}
				LOG.error(errorMessageReason.toString());
			}

			getLoggingHandler().handleResult(AUTHORIZATION_TRANSACTION, transaction);
		}

		return result;
	}

	public CartService getCartService()
	{
		return cartService;
	}

	public void setCartService(CartService cartService)
	{
		this.cartService = cartService;
	}

	public void setConfigurationService(ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}
}
