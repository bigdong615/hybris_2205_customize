package com.bl.core.job;

import com.bl.core.payment.service.BlPaymentService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This is responsible for creating the auth transactions and capturing the payment for the orders
 * @author Jyoti Swamy
 */
public class BlAuthorizeAndCapturePaymentJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlAuthorizeAndCapturePaymentJob.class);
	private BlPaymentService blPaymentService;

	/**
	 * This is for creating the auth transactions and capturing the payment for the orders
	 */
	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlAuthorizeAndCapturePaymentJob...");
		try
		{
			getBlPaymentService().authorizeAndCapturePaymentForOrders();
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BlAuthorizeAndCapturePaymentJob finished successfully");
		}
		catch(final Exception ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
					"Error occurred while performing BlAuthorizePaymentJob", ex);
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	/**
	 * @return the blPaymentService
	 */
	public BlPaymentService getBlPaymentService()
	{
		return blPaymentService;
	}

	/**
	 * @param blPaymentService
	 *           the blPaymentService to set
	 */
	public void setBlPaymentService(final BlPaymentService blPaymentService)
	{
		this.blPaymentService = blPaymentService;
	}

}
