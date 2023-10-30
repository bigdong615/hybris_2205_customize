/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.commercefacades.giftcard.movement.data.GiftCardMovementData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.GiftCardMovementModel;
import com.bl.logging.BlLogger;


public class BlGiftCardMovementPopulator implements Populator<GiftCardMovementModel, GiftCardMovementData>
{
	private static final Logger LOG = Logger.getLogger(BlGiftCardMovementPopulator.class);

	@Override
	public void populate(final GiftCardMovementModel source, final GiftCardMovementData target) throws ConversionException
	{

		try
		{
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setTransactionId(source.getTransactionId());
		if (source.getAmount() != null)
		{
			target.setAmount(source.getAmount());
		}
		if (source.getCurrency() != null)
		{
			target.setCurrency(source.getCurrency().getIsocode());
		}
		if (source.getOrder() != null)
		{
			target.setOrder(source.getOrder().getCode());
		}
		target.setRedeemDate(source.getRedeemDate());
		if (source.getBalanceAmount() != null)
		{
			target.setBalanceAmount(source.getBalanceAmount());
		}
		target.setCommitted(source.getCommitted());
		if (source.getGiftCard() != null)
		{
			target.setGiftCard(source.getGiftCard().getCode());
		}
		target.setPrimaryKey(source.getPk().toString());
	}
	catch (final Exception exception)
	{
		LOG.info("Error while getting GiftCardMovement for PK " + source.getPk().toString());
		BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting GiftCardMovement", exception);
		exception.printStackTrace();

	}
	}

}
