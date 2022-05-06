package com.bl.core.model.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.data.StockResult;
import com.bl.core.services.customer.BlUserService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.Date;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * @author Ravikumar
 *
 *         Validator Interceptor for OrderModel to verify the values set on attribute before saving the model
 *
 */
public class BlOrderValidateInterceptor implements ValidateInterceptor<OrderModel>
{

	private static final Logger LOG = Logger.getLogger(BlOrderValidateInterceptor.class);

	private BlUserService userService;
	private BlCommerceStockService blCommerceStockService;
	private BaseStoreService baseStoreService;

	@Override
	public void onValidate(final OrderModel orderModel, final InterceptorContext interceptorContext) throws InterceptorException
	{
		validateRentalDateChange(orderModel, interceptorContext);
	}

	/**
	 * Validate rental date change. Preventing rental start date after the rental end date or a rental end date before
	 * the rental start date
	 *
	 * @param orderModel
	 *           the order model
	 * @param interceptorContext
	 * @throws InterceptorException
	 *            the interceptor exception
	 */
	private void validateRentalDateChange(final OrderModel orderModel,
			final InterceptorContext interceptorContext) throws InterceptorException
	{
		if (getUserService().isCsUser() && ObjectUtils.allNotNull(orderModel.getRentalStartDate(), orderModel.getRentalEndDate()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"Validating Order for Rental Start Date : {} and Rental End Date : {} for CS user for Order : {}",
					orderModel.getRentalStartDate(), orderModel.getRentalEndDate(), orderModel.getCode());
			final Date rentalStartDate = orderModel.getRentalStartDate();
			final Date rentalEndDate = orderModel.getRentalEndDate();
			if (DateUtils.isSameDay(rentalStartDate, rentalEndDate) || rentalStartDate.compareTo(rentalEndDate) > 0
					|| rentalEndDate.compareTo(rentalStartDate) < 0)
			{
				throw new InterceptorException("Rental Start Date should not be a date later than Rental End Date");
			}
			if(interceptorContext.isModified(orderModel, AbstractOrderModel.RENTALSTARTDATE)|| interceptorContext.isModified(orderModel, AbstractOrderModel.RENTALENDDATE)) {
				final BaseStoreModel baseStore = getBaseStoreService()
						.getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
				for (AbstractOrderEntryModel orderEntry : orderModel.getEntries()) {
					final StockResult stockResult = getBlCommerceStockService().getStockForEntireDuration(
							orderEntry.getProduct().getCode(), baseStore.getWarehouses(), rentalStartDate,
							rentalEndDate);
					if (stockResult.getAvailableCount() == 0L) {
						throw new InterceptorException("Stock is not available for the extended dates");
					}
				}
			}
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

	public BlCommerceStockService getBlCommerceStockService() {
		return blCommerceStockService;
	}

	public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
		this.blCommerceStockService = blCommerceStockService;
	}

	public BaseStoreService getBaseStoreService() {
		return baseStoreService;
	}

	public void setBaseStoreService(BaseStoreService baseStoreService) {
		this.baseStoreService = baseStoreService;
	}

}
