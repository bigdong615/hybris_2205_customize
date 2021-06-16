/**
 *
 */
package com.braintree.customer.service;

import com.braintree.command.request.BrainTreeAddressRequest;
import com.braintree.command.result.BrainTreeAddressResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.customer.dao.BrainTreeCustomerAccountDao;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.commerceservices.customer.impl.DefaultCustomerAccountService;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;


public class BrainTreeCustomerAccountService extends DefaultCustomerAccountService
{
	private BrainTreeCustomerAccountDao brainTreeCustomerAccountDao;
	private BrainTreeConfigService brainTreeConfigService;
	private BrainTreePaymentService brainTreePaymentService;

	public BrainTreePaymentInfoModel getBrainTreePaymentInfoForCode(final CustomerModel customer, final String code)
	{
		return brainTreeCustomerAccountDao.findBrainTreePaymentInfoByCustomer(customer, code);
	}

	public List<BrainTreePaymentInfoModel> getBrainTreePaymentInfos(final CustomerModel customerModel, final boolean saved)
	{
		return brainTreeCustomerAccountDao.findBrainTreePaymentInfosByCustomer(customerModel, saved, getBrainTreeConfigService()
				.getMerchantAccountIdForCurrentSiteAndCurrency());
	}

	public void unlinkCCPaymentInfo(final CustomerModel customerModel, final BrainTreePaymentInfoModel brainTreePaymentInfo)
	{
		ServicesUtil.validateParameterNotNull(customerModel, "Customer model cannot be null");
		ServicesUtil.validateParameterNotNull(brainTreePaymentInfo, "BrainTreePaymentInfo model cannot be null");

		if (customerModel.getPaymentInfos().contains(brainTreePaymentInfo))
		{
			final Collection paymentInfoList = new ArrayList(customerModel.getPaymentInfos());
			paymentInfoList.remove(brainTreePaymentInfo);
			customerModel.setPaymentInfos(paymentInfoList);
			getModelService().save(customerModel);
			getModelService().refresh(customerModel);
		}
		else
		{
			throw new IllegalArgumentException("Credit Card Payment Info " + brainTreePaymentInfo
					+ " does not belong to the customer " + customerModel + " and will not be removed.");
		}
	}


	@Override
	public void deleteAddressEntry(CustomerModel customerModel, AddressModel addressModel)
	{
		validateParameterNotNull(addressModel, "Address model cannot be null");

		final String brainTreeAddressId = addressModel.getBrainTreeAddressId();
		if (StringUtils.isNotEmpty(brainTreeAddressId))
		{
			final BrainTreeAddressRequest addressRequest = convertBrainTreeAddress(customerModel, addressModel);
			addressRequest.setAddressId(brainTreeAddressId);
			getBrainTreePaymentService().removeAddress(addressRequest);
		}
		super.deleteAddressEntry(customerModel, addressModel);
	}


	@Override
	public void saveAddressEntry(CustomerModel customerModel, AddressModel addressModel)
	{
		super.saveAddressEntry(customerModel, addressModel);

		if (addressModel.getVisibleInAddressBook() && addressModel.getShippingAddress() &&
				Boolean.parseBoolean(getBrainTreeConfigService().getStoreInVaultForCurrentUser()))
		{
			BrainTreeAddressRequest brainTreeAddressRequest = convertBrainTreeAddress(customerModel, addressModel);
			final BrainTreeAddressResult brainTreeAddress = getBrainTreePaymentService().createAddress(brainTreeAddressRequest,
					customerModel);
			if (brainTreeAddress != null && brainTreeAddress.getAddress() != null)
			{
				addressModel.setBrainTreeAddressId(brainTreeAddress.getAddress().getId());
				getModelService().save(customerModel);
				getModelService().refresh(customerModel);
			}

		}
	}

	private BrainTreeAddressRequest convertBrainTreeAddress(CustomerModel customerModel, AddressModel address)
	{
		final BrainTreeAddressRequest addressRequest = new BrainTreeAddressRequest(customerModel.getBraintreeCustomerId());
		addressRequest.setStreetAddress(address.getLine1());
		addressRequest.setExtendedAddress(address.getLine2());
		addressRequest.setFirstName(address.getFirstname());
		addressRequest.setLastName(address.getLastname());
		addressRequest.setLocality(address.getTown());
		addressRequest.setPostalCode(address.getPostalcode());

		if (address.getCountry() != null)
		{
			addressRequest.setCountryCodeAlpha2(address.getCountry().getIsocode());
		}
		if (address.getRegion() != null)
		{
			//The state or province. For PayPal addresses, the region must be a 2-letter abbreviation; for all other payment methods, it must be less than or equal to 255 characters.
			//because of hybris specific use the isocodeShort - 2 character isocode - and its right for braintree
			//the isocode return  2 character isocode US-CA - wrong for braintree
			addressRequest.setRegion(address.getRegion().getIsocodeShort());
		}

		return addressRequest;
	}

	public BrainTreeCustomerAccountDao getBrainTreeCustomerAccountDao()
	{
		return brainTreeCustomerAccountDao;
	}

	public void setBrainTreeCustomerAccountDao(final BrainTreeCustomerAccountDao brainTreeCustomerAccountDao)
	{
		this.brainTreeCustomerAccountDao = brainTreeCustomerAccountDao;
	}

	public BrainTreeConfigService getBrainTreeConfigService()
	{
		return brainTreeConfigService;
	}

	public void setBrainTreeConfigService(final BrainTreeConfigService brainTreeConfigService)
	{
		this.brainTreeConfigService = brainTreeConfigService;
	}

	public BrainTreePaymentService getBrainTreePaymentService()
	{
		return brainTreePaymentService;
	}

	public void setBrainTreePaymentService(BrainTreePaymentService brainTreePaymentService)
	{
		this.brainTreePaymentService = brainTreePaymentService;
	}
}
