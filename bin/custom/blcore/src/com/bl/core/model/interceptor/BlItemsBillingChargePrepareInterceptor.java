package com.bl.core.model.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.BillInfoStatus;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;

import java.math.BigDecimal;
import java.util.Date;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.services.customer.BlUserService;
import com.bl.logging.BlLogger;


/**
 * @author Ravikumar
 *
 *         This interceptor is used to modify the Item Billing Charges before saving to table.
 *
 */
public class BlItemsBillingChargePrepareInterceptor implements PrepareInterceptor<BlItemsBillingChargeModel>
{
	private static final Logger LOG = Logger.getLogger(BlItemsBillingChargePrepareInterceptor.class);

	private BlUserService userService;

	@Override
	public void onPrepare(final BlItemsBillingChargeModel blItemsBillingChargeModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		if (StringUtils.isBlank(blItemsBillingChargeModel.getCode()))
		{
			final String randomId = UUID.randomUUID().toString();
			blItemsBillingChargeModel.setCode(randomId);
			blItemsBillingChargeModel.setUpdatedBillTime(new Date());
			blItemsBillingChargeModel.setBillStatus(BillInfoStatus.NEW_BILL);
			blItemsBillingChargeModel.setChargedAmount(blItemsBillingChargeModel.getChargedAmount().setScale(BlCoreConstants.DECIMAL_PRECISION,BlCoreConstants.ROUNDING_MODE));
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BlItemsBillingChargePrepareInterceptor : UUID : {}", randomId);
		}
	}

	/**
	 * @return the userService
	 */
	public BlUserService getUserService()
	{
		return userService;
	}

	/**
	 * @param userService
	 *           the userService to set
	 */
	public void setUserService(final BlUserService userService)
	{
		this.userService = userService;
	}

}
