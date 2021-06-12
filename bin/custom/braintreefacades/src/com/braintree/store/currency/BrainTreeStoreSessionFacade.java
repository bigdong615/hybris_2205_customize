/**
 *
 */
package com.braintree.store.currency;

import de.hybris.platform.commercefacades.storesession.impl.DefaultStoreSessionFacade;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.servicelayer.model.ModelService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.model.BrainTreePaymentInfoModel;


public class BrainTreeStoreSessionFacade extends DefaultStoreSessionFacade
{
	private static final Logger LOG = LoggerFactory.getLogger(BrainTreeStoreSessionFacade.class);

	private ModelService modelService;
	private BrainTreeConfigService brainTreeConfigService;

	@Override
	public void setCurrentCurrency(final String isocode)
	{
		final String previousCurrency = getCurrentCurrency().getIsocode();
		super.setCurrentCurrency(isocode);
		if (getCartService().hasSessionCart())
		{
			final CartModel cart = getCartService().getSessionCart();

			if (!isocode.equalsIgnoreCase(previousCurrency))
			{
				final PaymentInfoModel paymentInfo = cart.getPaymentInfo();
				if (paymentInfo != null && paymentInfo instanceof BrainTreePaymentInfoModel)
				{
					final String accountID = getBrainTreeConfigService().getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(isocode);
					((BrainTreePaymentInfoModel) paymentInfo).setMerchantAccountIdForCurrentSite(accountID);
					modelService.save(paymentInfo);

					LOG.debug("[CURRENCY CHANGED] FROM {} TO {}", previousCurrency, isocode);
					LOG.info("[MERCHANT ACCOUNT CHANGED] FROM {} TO {}",
							((BrainTreePaymentInfoModel) paymentInfo).getMerchantAccountIdForCurrentSite(), accountID);
				}
			}
		}
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	public BrainTreeConfigService getBrainTreeConfigService()
	{
		return brainTreeConfigService;
	}

	public void setBrainTreeConfigService(final BrainTreeConfigService brainTreeConfigService)
	{
		this.brainTreeConfigService = brainTreeConfigService;
	}

}
