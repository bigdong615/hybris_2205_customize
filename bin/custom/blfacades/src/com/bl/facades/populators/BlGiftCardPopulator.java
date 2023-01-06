/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.commercefacades.giftcard.data.GiftCardData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.util.ArrayList;
import java.util.List;

import com.bl.core.model.GiftCardModel;


public class BlGiftCardPopulator implements Populator<GiftCardModel, GiftCardData>
{

	@Override
	public void populate(final GiftCardModel source, final GiftCardData target) throws ConversionException
	{
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setCode(source.getCode());
		if(source.getAmount()!=null) {
			target.setAmount(source.getAmount());
		}
		if (source.getCurrency() != null)
		{
			target.setCurrency(source.getCurrency().getIsocode());
		}
		target.setStartDate(source.getStartDate());
		target.setEndDate(source.getEndDate());
		target.setActive(source.getActive());
		target.setEmail(source.getEmail());
		target.setIssuer(source.getIssuer());
		if(source.getBalance()!=null) {
			target.setBalance(source.getBalance());
		}
		target.setIsPurchased(source.getIsPurchased());
		target.setCustomerEmail(source.getCustomerEmail());
		target.setName(source.getName());
		target.setMessage(source.getMessage());
		target.setDiscountID(source.getDiscountID());
		if (source.getOrder() != null && !source.getOrder().isEmpty())
		{
			final List<String> orderCodes = new ArrayList<String>();
			for (final AbstractOrderModel order : source.getOrder())
			{
				orderCodes.add(order.getCode());
			}
			target.setOrder(orderCodes);
		}
		if (source.getCustomer() != null)
		{
			target.setCustomer(source.getCustomer().getName());
		}
	}

}
