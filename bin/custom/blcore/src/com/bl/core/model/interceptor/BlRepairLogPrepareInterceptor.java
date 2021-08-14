package com.bl.core.model.interceptor;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;

import org.apache.commons.lang3.Validate;

import com.bl.core.model.BlRepairLogModel;
import com.bl.core.repair.log.service.BlRepairLogService;


/**
 * The Class BlRepairLogPrepareInterceptor used to intercept the model and modify the attributes before saving the data.
 *
 * @author Ravikumar
 *
 */
public class BlRepairLogPrepareInterceptor implements PrepareInterceptor<BlRepairLogModel>
{
	private BlRepairLogService blRepairLogService;

	@Override
	public void onPrepare(final BlRepairLogModel blRepairLogModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		validateParameterNotNull(blRepairLogModel, "ERROR : BlRepairLogPrepareInterceptor : Parameter BlRepairLogModel is NULL");
		Validate.notBlank(blRepairLogModel.getItemBarcode(),
				"ERROR : BlRepairLogPrepareInterceptor : No Barcode found on Repair Log");
		getBlRepairLogService().addNecessaryDataToRepairLog(blRepairLogModel, interceptorContext);
	}

	/**
	 * @return the blRepairLogService
	 */
	public BlRepairLogService getBlRepairLogService()
	{
		return blRepairLogService;
	}

	/**
	 * @param blRepairLogService
	 *           the blRepairLogService to set
	 */
	public void setBlRepairLogService(final BlRepairLogService blRepairLogService)
	{
		this.blRepairLogService = blRepairLogService;
	}


}
