/**
 *
 */
package com.bl.core.model.interceptor;

import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;

import java.security.SecureRandom;
import java.util.Random;

import org.apache.commons.lang3.StringUtils;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.GiftCardModel;


/**
 * #################### BL- 1821 #################### Data Migration - Gift card - New Attribute discountID to be added
 * in gift card table
 *
 * @author Aditi Sharma
 *
 */
public class BlGiftCardPrepareInterceptor implements PrepareInterceptor<GiftCardModel>
{

	/**
	 * This method is used to set random discountId on GiftCard.
	 */
	@Override
	public void onPrepare(final GiftCardModel giftCardModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		if(!interceptorContext.isNew(giftCardModel) && interceptorContext.isModified(giftCardModel,GiftCardModel.DISCOUNTID) && checkForOriginalValue(giftCardModel))
		{
			throw new InterceptorException(
					"Modification on discount id is not allowed");
		}
		if (StringUtils.isBlank(giftCardModel.getDiscountID()))
		{
			giftCardModel.setDiscountID(generateDiscountId());
		}
	}

	/**
	 * This method will check if value for discount id is present or not
	 * @param giftCardModel
	 * @return
	 */
	private boolean checkForOriginalValue(final GiftCardModel giftCardModel)
	{
		if(giftCardModel.getItemModelContext().getOriginalValue(GiftCardModel.DISCOUNTID) instanceof String)
		{
			return StringUtils.isNotBlank((String)giftCardModel.getItemModelContext().getOriginalValue(GiftCardModel.DISCOUNTID));
		}		
		return false;
	}

	/**
	 * This method is used to generate randome discount id
	 * @return randomDiscountId
	 */
	private String generateDiscountId()
	{
		Random secureRandom = new SecureRandom();
	    return secureRandom.ints(BlCoreConstants.SEVEN, 0, BlCoreConstants.NUMERIC_VALUES.length())
	        .mapToObj(i -> BlCoreConstants.NUMERIC_VALUES.charAt(i))
	        .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append).toString();
	}



}
