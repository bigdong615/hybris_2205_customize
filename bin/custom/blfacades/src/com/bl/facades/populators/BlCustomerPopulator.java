package com.bl.facades.populators;

import de.hybris.platform.commercefacades.user.converters.populator.CustomerPopulator;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.core.model.user.CustomerModel;

import org.apache.log4j.Logger;


public class BlCustomerPopulator extends CustomerPopulator
{
	private static final Logger LOG = Logger.getLogger(BlCustomerPopulator.class);

  @Override
  public void populate(final CustomerModel source, final CustomerData target)
  {
	  super.populate(source, target);
	  //target.setDefaultPaymentAddress
	  target.setCreatedTS(source.getCreationtime());
	  target.setModifiedTS(source.getModifiedtime());
	  target.setDescription(source.getDescription());
	  target.setPasswordAnswer(source.getPasswordAnswer());
	  target.setPasswordQuestion(source.getPasswordQuestion());
	  if(source.getRetentionState()!=null) {
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
	  if (source.getVipType() != null)
	  {
		  target.setVipType(source.getVipType().getCode());
	  }
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

  }
}