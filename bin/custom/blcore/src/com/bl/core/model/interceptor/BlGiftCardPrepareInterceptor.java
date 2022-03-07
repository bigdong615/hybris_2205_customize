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
		if(!interceptorContext.isNew(giftCardModel) && interceptorContext.isModified(GiftCardModel.DISCOUNTID) && StringUtils.isNotBlank(giftCardModel.getItemModelContext().getOriginalValue(GiftCardModel.DISCOUNTID)))
		{
			throw new InterceptorException(
					"Can't modifiy discount id");
		}
		if (StringUtils.isBlank(giftCardModel.getDiscountID()))
		{
			giftCardModel.setDiscountID(generateDiscountId());
		}
	}

	/**
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
