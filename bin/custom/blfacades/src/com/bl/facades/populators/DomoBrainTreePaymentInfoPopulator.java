package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;

import org.apache.log4j.Logger;

import com.braintree.hybris.data.BrainTreePaymentInfoData;
import com.braintree.model.BrainTreePaymentInfoModel;


public class DomoBrainTreePaymentInfoPopulator implements Populator<BrainTreePaymentInfoModel, BrainTreePaymentInfoData>
{
	private static final Logger LOG = Logger.getLogger(DomoBrainTreePaymentInfoPopulator.class);

	@Override
	public void populate(final BrainTreePaymentInfoModel source, final BrainTreePaymentInfoData target)
	{
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setCode(source.getCode());
		if (source.getDuplicate() != null)
		{
			target.setDuplicate(source.getDuplicate());
		}
		if (source.getUser() != null)
		{
			target.setUser(source.getUser().getUid());
		}
		target.setSaved(source.isSaved());
		if (source.getBillingAddress() != null)
		{
			target.setBillingAddress(source.getBillingAddress().getAddressID());
		}
		target.setPaymentProvider(source.getPaymentProvider());
		target.setNonce(source.getNonce());
		target.setCustomerId(source.getCustomerId());
		target.setDeviceData(source.getDeviceData());
		target.setPaymentMethodToken(source.getPaymentMethodToken());
		if (source.getLiabilityShifted() != null)
		{
			target.setLiabilityShifted(source.getLiabilityShifted());
		}
		if (source.getUsePaymentMethodToken() != null)
		{
			target.setUsePaymentMethodToken(source.getUsePaymentMethodToken());
		}
		target.setExpirationMonth(source.getExpirationMonth());
		target.setExpirationYear(source.getExpirationMonth());
		target.setExpirationYear(source.getExpirationYear());
		target.setImageSource(source.getImageSource());
		if (source.getThreeDSecureConfiguration() != null)
		{
			target.setThreeDSecureConfiguration(source.getThreeDSecureConfiguration());
		}
		if (source.getAdvancedFraudTools() != null)
		{
			target.setAdvancedFraudTools(source.getAdvancedFraudTools());
		}
		if (source.getIsSkip3dSecureLiabilityResult() != null)
		{
			target.setIsSkip3dSecureLiabilityResult(source.getIsSkip3dSecureLiabilityResult());
		}
		target.setCreditCardStatementName(source.getCreditCardStatementName());
		target.setBrainTreeChannel(source.getBrainTreeChannel());
		target.setMerchantAccountIdForCurrentSite(source.getMerchantAccountIdForCurrentSite());
		target.setPaymentInfo(source.getPaymentInfo());
		target.setCardholderName(source.getCardholderName());
		target.setCustomerLocation(source.getCustomerLocation());
		target.setPayPalIntent(source.getPayPalIntent());
		target.setPayer(source.getPayer());
		target.setShipsFromPostalCode(source.getShipsFromPostalCode());
		target.setPaymentId(source.getPaymentId());
		if (source.getDepositAmount() != null)
		{
			target.setDepositAmount(source.getDepositAmount());
		}
		if (source.getCreatedAt() != null)
		{
			target.setCreatedAt(source.getCreatedAt());
		}
		if (source.getUpdatedAt() != null)
		{
			target.setUpdatedAt(source.getUpdatedAt());
		}
		target.setShouldBeSaved(source.isShouldBeSaved());
		if (source.getNewAmount() != null)
		{
			target.setNewAmount(source.getNewAmount());
		}
		target.setIsDepositPayment(source.isIsDepositPayment());
		target.setCreateNewTransaction(source.isCreateNewTransaction());
		target.setBillPayment(source.isBillPayment());
		target.setModifyPayment(source.isModifyPayment());
		target.setExtendOrder(source.isExtendOrder());
		target.setPrimaryKey(source.getPk().toString());
	}
}
