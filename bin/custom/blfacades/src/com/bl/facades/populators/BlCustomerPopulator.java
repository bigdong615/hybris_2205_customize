package com.bl.facades.populators;

import de.hybris.platform.commercefacades.user.converters.populator.CustomerPopulator;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.core.model.user.CustomerModel;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;
import com.bl.logging.BlLogger;


public class BlCustomerPopulator extends CustomerPopulator
{
	private static final Logger LOG = Logger.getLogger(BlCustomerPopulator.class);


	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final CustomerModel source, final CustomerData target)
	{
		super.populate(source, target);
		try
		{
			target.setCreatedTS(source.getCreationtime());
			target.setModifiedTS(source.getModifiedtime());
			target.setDescription(source.getDescription());
			target.setPasswordAnswer(source.getPasswordAnswer());
			target.setPasswordQuestion(source.getPasswordQuestion());
			if (source.getRetentionState() != null)
			{
				target.setRetentionState(source.getRetentionState().getCode());
			}
			target.setTaxExemptNumber(source.getTaxExemptNumber());
			target.setTaxExemptState(source.getTaxExemptState());
			target.setLastLogin(source.getLastLogin());
			target.setCoiExpirationDate(source.getCoiExpirationDate());
			target.setTaxExemptExpiry(source.getTaxExemptExpiry());
			target.setBackofficeLoginDisabled(source.getBackOfficeLoginDisabled());
			target.setLoginDisabled(source.isLoginDisabled());
			target.setAuthorizedToUnlockPages(source.isAuthorizedToUnlockPages());
			target.setIsTaxExempt(source.getIsTaxExempt());
			target.setHmcLoginDisabled(source.getHmcLoginDisabled());
			target.setEmailPreference(source.getEmailPreference());
			target.setSmsPreference(source.getSmsPreference());
			target.setPoEnabled(source.isPoEnabled());
			target.setVip(source.isVip());
			target.setCoiVerified(source.isCoiVerified());
			target.setEmployee(source.isEmployee());
			target.setOriginalUid(source.getOriginalUid());
			if (source.getType() != null)
			{
				target.setType(source.getType().getCode());
			}
			target.setBraintreeCustomerId(source.getBraintreeCustomerId());
			if (source.getBadBehaviorTag() != null)
			{
				target.setBadBehaviorTag(source.getBadBehaviorTag().getCode());
			}
			target.setCoiCoverageAmount(source.getCoiCoverageAmount());
			target.setKycScore(source.getKycScore());

			target.setVipType(source.getVipType().getCode());

			if (source.getVerificationLinkSource() != null)
			{
				target.setVerificationLinkSource(source.getVerificationLinkSource().getCode());
			}
			target.setPhotoOrVideo(source.getPhotoOrVideo());
			target.setBadBehaviorNotes(source.getBadBehaviorNotes());
			target.setVerificationNotes(source.getVerificationNotes());
			target.setVerificationLink(source.getVerificationLink());
			target.setAccountNotes(source.getAccountNotes());
			target.setOrderCount(source.getOrderCount());
			target.setCompletedOrderCount(source.getCompletedOrderCount());
			target.setInprocessOrderCount(source.getInprocessOrderCount());
			target.setTotalAmountPastDue(source.getTotalAmountPastDue());
			target.setBlRewards(source.getBlRewards());
			target.setAverageGearOrderValue(source.getAverageGearOrderValue());
			target.setGearvalueordersinprogress(source.getGearValueOrdersInProgress());
			target.setPrimaryKey(source.getPk().toString());
			if (source.getDefaultBillingAddress() != null)
			{
				target.setDefaultBillingAddressPk(source.getDefaultBillingAddress().getPk().toString());
			}
			if (source.getDefaultPaymentAddress() != null)
			{
				target.setDefaultPaymentAddressPk(source.getDefaultPaymentAddress().getPk().toString());
			}
			if (source.getDefaultShipmentAddress() != null)
			{
				target.setDefaultShippingAddressPk(source.getDefaultShipmentAddress().getPk().toString());
			}
		}
		catch (final Exception exception)
		{
			LOG.info("Error while getting customer info for PK " + source.getPk().toString());
			BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting customer info for PK", exception);
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(), "customer api");

		}

}

/**
 * @return the defaultBlDomoFailureNotificationService
 */
public DefaultBlDomoFailureNotificationService getDefaultBlDomoFailureNotificationService()
{
	return defaultBlDomoFailureNotificationService;
}

/**
 * @param defaultBlDomoFailureNotificationService
 *           the defaultBlDomoFailureNotificationService to set
 */
public void setDefaultBlDomoFailureNotificationService(
		final DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService)
{
	this.defaultBlDomoFailureNotificationService = defaultBlDomoFailureNotificationService;
}



}
