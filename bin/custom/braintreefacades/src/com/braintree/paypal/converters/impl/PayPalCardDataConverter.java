/**
 *
 */
package com.braintree.paypal.converters.impl;

import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.i18n.I18NService;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.hybris.data.PayPalCheckoutData;


public class PayPalCardDataConverter implements Converter<CartModel, PayPalCheckoutData>
{
	private I18NService i18NService;
	private BrainTreeConfigService brainTreeConfigService;
	private CheckoutFacade checkoutFacade;


	@Override
	public PayPalCheckoutData convert(final CartModel cart) throws ConversionException
	{
		final PayPalCheckoutData payPalCheckoutData = new PayPalCheckoutData();
		return convert(cart, payPalCheckoutData);
	}

	@Override
	public PayPalCheckoutData convert(final CartModel cart, final PayPalCheckoutData checkoutData) throws ConversionException
	{
		checkoutData.setStoreInVault(String.valueOf(getBrainTreeConfigService().getStoreInVaultIgnoringIntent()));
		checkoutData.setIntent(getBrainTreeConfigService().getIntent());
		checkoutData.setAmount(cart.getTotalPrice());
		checkoutData.setCurrency(cart.getCurrency().getIsocode());
		checkoutData.setLocale(getI18NService().getCurrentLocale().toString());
		checkoutData.setAdvancedFraudTools(getBrainTreeConfigService().getAdvancedFraudTools());
		checkoutData.setDbaName(getBrainTreeConfigService().getCreditCardStatementName());
		checkoutData.setApplePayEnabled(String.valueOf(getBrainTreeConfigService().getApplePayEnabled()));
		checkoutData.setEnableShippingAddress(!isOnlyPickUp());
		return checkoutData;
	}

	private Boolean isOnlyPickUp() {
		return getCheckoutFacade().getCheckoutCart().getDeliveryOrderGroups().isEmpty();
	}

	public I18NService getI18NService()
	{
		return i18NService;
	}

	public void setI18NService(final I18NService i18nService)
	{
		i18NService = i18nService;
	}

	public BrainTreeConfigService getBrainTreeConfigService()
	{
		return brainTreeConfigService;
	}

	public void setBrainTreeConfigService(final BrainTreeConfigService brainTreeConfigService)
	{
		this.brainTreeConfigService = brainTreeConfigService;
	}

	public CheckoutFacade getCheckoutFacade() {
		return checkoutFacade;
	}

	public void setCheckoutFacade(CheckoutFacade checkoutFacade) {
		this.checkoutFacade = checkoutFacade;
	}
}
