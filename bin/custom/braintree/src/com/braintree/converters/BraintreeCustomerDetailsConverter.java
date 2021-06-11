package com.braintree.converters;

import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.model.BraintreeCustomerDetailsModel;
import com.braintreegateway.Address;
import com.braintreegateway.Customer;
import com.braintreegateway.PaymentMethod;
import de.hybris.platform.converters.impl.AbstractPopulatingConverter;
import de.hybris.platform.core.model.user.AddressModel;

import java.util.ArrayList;
import java.util.List;


public class BraintreeCustomerDetailsConverter extends AbstractPopulatingConverter<Customer, BraintreeCustomerDetailsModel>
{
	private BraintreeAddressConverter braintreeAddressConverter;
	private BraintreePaymentMethodConverter paymentMethodConverter;

	@Override
	public void populate(final Customer customer, final BraintreeCustomerDetailsModel model)
	{
		if (customer != null && model != null)
		{
			model.setId(customer.getId());
			model.setCompany(customer.getCompany());
			model.setCreatedAt(customer.getCreatedAt().getTime());
			model.setEmail(customer.getEmail());
			model.setFax(customer.getFax());
			model.setFirstName(customer.getFirstName());
			model.setLastName(customer.getLastName());
			if (customer.getUpdatedAt() != null)
			{
				model.setModifiedtime(customer.getUpdatedAt().getTime());
			}
			model.setPhone(customer.getPhone());
			model.setWebsite(customer.getWebsite());
			if (null != customer.getAddresses())
			{
				final List<AddressModel> addresses = new ArrayList<>(customer.getAddresses().size());
				for (final Address address : customer.getAddresses())
				{
					addresses.add(getBraintreeAddressConverter().convert(address));
				}
				model.setAddresses(addresses);
			}
			populateCustomerPaymentMethods(customer, model);
		}
	}

	private void populateCustomerPaymentMethods(final Customer customer, final BraintreeCustomerDetailsModel model)
	{
		final List<BrainTreePaymentInfoModel> paymentMethodList = new ArrayList<>();
		paymentMethodList.addAll(getBraintreePaymentMethodsFromPaymentMethodsList(customer.getCreditCards()));
		paymentMethodList.addAll(getBraintreePaymentMethodsFromPaymentMethodsList(customer.getPayPalAccounts()));
		paymentMethodList.addAll(getBraintreePaymentMethodsFromPaymentMethodsList(customer.getApplePayCards()));
		paymentMethodList.addAll(getBraintreePaymentMethodsFromPaymentMethodsList(customer.getAndroidPayCards()));
		paymentMethodList.addAll(getBraintreePaymentMethodsFromPaymentMethodsList(customer.getAmexExpressCheckoutCards()));
		model.setPaymentMethods(paymentMethodList);
	}

	private List<BrainTreePaymentInfoModel> getBraintreePaymentMethodsFromPaymentMethodsList(
			final List<? extends PaymentMethod> paymentMethods)
	{
		final List<BrainTreePaymentInfoModel> btPaymentMethods = new ArrayList<>((null != paymentMethods) ? paymentMethods.size()
				: 0);
		if (null != paymentMethods)
		{
			for (final PaymentMethod pm : paymentMethods)
			{
				btPaymentMethods.add(getPaymentMethodConverter().convert(pm));
			}
		}
		return btPaymentMethods;
	}

	/**
	 * @return the addressPopulator
	 */
	public BraintreeAddressConverter getBraintreeAddressConverter()
	{
		return braintreeAddressConverter;
	}

	/**
	 * @param braintreeAddressConverter the addressPopulator to set
	 */
	public void setBraintreeAddressConverter(final BraintreeAddressConverter braintreeAddressConverter)
	{
		this.braintreeAddressConverter = braintreeAddressConverter;
	}

	public BraintreePaymentMethodConverter getPaymentMethodConverter()
	{
		return paymentMethodConverter;
	}

	public void setPaymentMethodConverter(final BraintreePaymentMethodConverter paymentMethodConverter)
	{
		this.paymentMethodConverter = paymentMethodConverter;
	}
}
