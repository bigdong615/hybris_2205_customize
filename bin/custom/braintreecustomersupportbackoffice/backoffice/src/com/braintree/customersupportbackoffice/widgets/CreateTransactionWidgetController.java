package com.braintree.customersupportbackoffice.widgets;


import com.braintree.command.request.BrainTreeFindMerchantAccountRequest;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.customersupportbackoffice.data.BrainTreeTransactionInfo;
import com.braintree.customersupportbackoffice.facade.BrainTreeCustomerSupportFacade;
import com.braintree.customfield.service.CustomFieldsService;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.method.BrainTreePaymentService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.site.BaseSiteService;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.Map;


public class CreateTransactionWidgetController extends DefaultWidgetController {
    private final static Logger LOG = Logger.getLogger(CreateTransactionWidgetController.class);

	@Resource(name = "brainTreeCustomerSupportFacade")
	protected BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade;

	@Resource(name = "commonI18NService")
	protected CommonI18NService commonI18NService;

	@Resource(name = "customFieldsService")
	protected CustomFieldsService customFieldsService;

	@Resource(name = "baseSiteService")
	protected BaseSiteService baseSiteService;

	@Resource(name = "brainTreeConfigService")
	protected BrainTreeConfigService brainTreeConfigService;

	@Resource(name = "brainTreePaymentService")
	protected BrainTreePaymentService brainTreePaymentService;

	private Textbox currency;

	private Textbox amount;

	private Textbox taxAmount;

	private Textbox customFields;

	private Textbox firstName;

	private Textbox lastName;

	private Textbox email;

	private Textbox billingPostCode;

	private Textbox billingStreetAddress;

	private Textbox shippingPostalCode;

	private Textbox shippingStreet;

	private String cardholder;

	private String nonce;

	private String siteUID;

	private String currentCurrency;

	@SuppressWarnings("Duplicates")
	@Override
	public void initialize(Component comp) {
		super.initialize(comp);
		amount.setConstraint((component, o) -> {
			try {
				double value = Double.parseDouble((String) o);
				if (value >= 0)
					return;
			} catch (Throwable e) {}
			throw new WrongValueException(component, "Invalid amount value");
		});

		email.setConstraint((component, o) -> {
			try {
				String emailString = (String) o;
				if (emailString.matches("[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}"))
					return;
			} catch (Throwable e) {}
			throw new WrongValueException(component, "Invalid email specified");
		});

		taxAmount.setConstraint((component, o) -> {
			try {
				double value = Double.parseDouble((String) o);
				if (value >= 0)
					return;
			} catch (Throwable e) {}
			throw new WrongValueException(component, "Invalid tax amount value");
		});
	}

	@ViewEvent(componentID = "saveBtn", eventName = Events.ON_CLICK)
	public void saveNewTransaction() throws JsonProcessingException {

        BrainTreeTransactionInfo newTransaction = new BrainTreeTransactionInfo();
        try {
            newTransaction.setNonce(nonce);
            newTransaction.setCardHolder(cardholder);

            newTransaction
                    .setAmount(BigDecimal.valueOf(Double.parseDouble(amount.getValue())))
                    .setTax(BigDecimal.valueOf(Double.parseDouble(taxAmount.getValue()))).setCurrency(currentCurrency);

            setCustomFields(newTransaction);

            newTransaction.setFirstName(firstName.getValue()).setLastName(lastName.getValue()).setEmail(email.getValue());
            newTransaction.setBillingAddress(billingStreetAddress.getValue()).setBillingPostCode(billingPostCode.getValue());
            newTransaction.setShippingAddress(shippingStreet.getValue()).setShippingPostCode(shippingPostalCode.getValue());

            newTransaction.setMerchantAccountId(getMerchantAccountId(siteUID, currentCurrency));
            SubscriptionResult customer = brainTreeCustomerSupportFacade.createCustomer(newTransaction);
            if (customer == null || StringUtils.isBlank(customer.getRequestId())) {
                showErrorMessage("Failed to create new transaction");
            } else {
                createTransaction(newTransaction, customer.getMerchantTransactionCode());
				sendOutput("outgoingMsg", "success");
            }
        } catch (Exception e) {
            showErrorMessage("Request failed!");
        }
	}

	@SocketEvent(socketId = "incomingMsg")
	public void updateTranscript(final String msg) {
		cardholder = StringUtils.split(msg, "|")[0];
		nonce = StringUtils.split(msg, "|")[1];
		siteUID = StringUtils.split(msg, "|")[2];
		currentCurrency = StringUtils.split(msg, "|")[3];
	}

	@SuppressWarnings("Duplicates")
	protected void setCustomFields(final BrainTreeTransactionInfo brainTreeInfo) {
		String customValue = customFields.getValue();
		String[] splitByParametersPair = org.apache.commons.lang.StringUtils.split(customValue, ",");

		final Map<String, String> defaultCustomFields = customFieldsService.getDefaultCustomFieldsMap();

		for (final String key : defaultCustomFields.keySet()) {
			brainTreeInfo.setCustom(key, defaultCustomFields.get(key));
		}

		if (splitByParametersPair.length > 0) {
			for (final String parameter : splitByParametersPair) {
				final String[] spitedParameter = StringUtils.split(parameter, ":");
				if (spitedParameter.length == 2) {
					brainTreeInfo.setCustom(spitedParameter[0], spitedParameter[1]);
				}
			}
		}
	}

	@SuppressWarnings("Duplicates")
	private void createTransaction(BrainTreeTransactionInfo newTransaction, String customerID) {
		BrainTreePaymentMethodResult creditCardPaymentMethod = brainTreeCustomerSupportFacade.createCreditCardPaymentMethod(customerID,
				newTransaction.getNonce(), newTransaction.getCardHolder(), false, StringUtils.EMPTY);

		if (creditCardPaymentMethod.isSuccess()) {
			newTransaction.setPaymentMethodToken(creditCardPaymentMethod.getPaymentMethodToken());
			final BrainTreeResponseResultData transaction = brainTreeCustomerSupportFacade.createTransaction(newTransaction);
			if (transaction.isSuccess()) {
				showSuccessMessage("Transaction successfully created, id: " + transaction.getTransactionId());
			}
			else
			{
				showErrorMessage(transaction.getErrorMessage());
			}
		}
		else {
			brainTreeCustomerSupportFacade.removeCustomer(customerID);
			showErrorMessage("Verify card failed");
		}
	}

	private void showErrorMessage(final String message) {
		Messagebox.show(message, "Create new transaction", Messagebox.OK, Messagebox.ERROR);
	}

	private void showSuccessMessage(final String message) {
		Messagebox.show(message, "Create new transaction", Messagebox.OK, Messagebox.EXCLAMATION, event -> {
			getWidgetInstanceManager().getWidgetslot().getParent().detach();
		});
	}

	@SuppressWarnings("Duplicates")
	private String getMerchantAccountId(String site, String currency) {
		final String merchantAccountId = brainTreeConfigService.getMerchantAccountIdByCurrentSiteNameAndCurrency(site.toLowerCase(), currency.toLowerCase());
		if (StringUtils.isNotEmpty(merchantAccountId)) {
			final BrainTreeFindMerchantAccountRequest brainTreeFindMerchantAccountRequest
					= new BrainTreeFindMerchantAccountRequest(StringUtils.EMPTY);
			brainTreeFindMerchantAccountRequest.setMerchantAccount(merchantAccountId);
			final boolean isMerchantAccountExist = brainTreePaymentService.findMerchantAccount(brainTreeFindMerchantAccountRequest).isMerchantAccountExist();
			if (isMerchantAccountExist) {
				return merchantAccountId;
			}
		} else {
			LOG.info("MerchantAccountID for current site: " + siteUID + " and currency: " + currentCurrency + " not found.");
		}
		return null;
	}
}
