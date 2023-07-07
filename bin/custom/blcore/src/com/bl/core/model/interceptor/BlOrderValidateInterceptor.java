package com.bl.core.model.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.data.StockResult;
import com.bl.core.services.customer.BlUserService;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
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
public class BlOrderValidateInterceptor implements ValidateInterceptor<AbstractOrderModel>
{

	private static final Logger LOG = Logger.getLogger(BlOrderValidateInterceptor.class);

	private BlUserService userService;
	private BlCommerceStockService blCommerceStockService;
	private BaseStoreService baseStoreService;
	@Resource(name = "blOrderService")
	private BlOrderService blOrderService;

	@Override
	public void onValidate(final AbstractOrderModel orderModel, final InterceptorContext interceptorContext) throws InterceptorException
	{
		if(orderModel instanceof OrderModel)
		validateRentalDateChange((OrderModel) orderModel, interceptorContext);
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
			if(orderModel.getIsRentalOrder() && interceptorContext.isModified(orderModel, AbstractOrderModel.RENTALSTARTDATE)|| interceptorContext.isModified(orderModel, AbstractOrderModel.RENTALENDDATE)) {

				blOrderService.updateActualRentalDatesForOrder(orderModel);

				/*if(!checkIsStockAvailableForModifyDate(orderModel)){
					throw new InterceptorException("from validator Can't modify rental date due unavailable of stock for new duration");
				}*/

			/*	final BaseStoreModel baseStore = getBaseStoreService()
						.getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
				for (AbstractOrderEntryModel orderEntry : orderModel.getEntries()) {
					final StockResult stockResult = getBlCommerceStockService().getStockForEntireDuration(
							orderEntry.getProduct().getCode(), baseStore.getWarehouses(), rentalStartDate,
							rentalEndDate);
					if (stockResult.getAvailableCount() == 0L) {
						throw new InterceptorException("Stock is not available for the extended dates");
					}
				}*/
			}
		}
	}

	private boolean checkIsStockAvailableForModifyDate(final AbstractOrderModel abstractOrderModel)
	{

		final Set<String> productCodes = abstractOrderModel.getEntries().stream().filter(entry -> !entry.isBundleMainEntry())
				.map(entry -> entry.getProduct().getCode()).collect(Collectors.toSet());

		final BaseStoreModel baseStoreModel = getBaseStoreService()
				.getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
		//Get all warehouses
		final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();

		Set<StockLevelModel> allStock = new HashSet<>();
		warehouses.forEach(warehouseModel -> {
			final Collection<StockLevelModel> stockLevels = blCommerceStockService
					.getStockForProductCodesAndDate(productCodes,
							warehouseModel, abstractOrderModel.getActualRentalStartDate(), abstractOrderModel.getActualRentalEndDate());
			if(CollectionUtils.isNotEmpty(stockLevels)){
				allStock.addAll(stockLevels);
			}
		});


		Map<String, List<StockLevelModel>> productWiseAvailabilityMap;
		if (CollectionUtils.isNotEmpty(allStock)) {
			productWiseAvailabilityMap = allStock.stream()
					.collect(Collectors.groupingBy(StockLevelModel::getProductCode));

			for (String skuProduct : productCodes) {
				final Long availableQty = getAvailabilityForProduct(skuProduct, productWiseAvailabilityMap);
				final Optional<AbstractOrderEntryModel> orderEntryModel = abstractOrderModel.getEntries()
						.stream().filter(orderEntry -> orderEntry.getProduct().getCode()
								.equals(skuProduct)).findFirst();

				if (orderEntryModel.isPresent()) {
					final AbstractOrderEntryModel orderEntry = orderEntryModel.get();
					final Long unallocatedQty = orderEntry.getQuantity();
					if (unallocatedQty > availableQty) {
						return false;
					}
				}
			}
		}
		return false;
	}

	public Long getAvailabilityForProduct(final String skuProduct, final
	Map<String, List<StockLevelModel>> availabilityMap) {
		Long stockLevel = 0L;
		if (MapUtils.isNotEmpty(availabilityMap)) {
			final List<StockLevelModel> stockLevelList =
					Objects.isNull(availabilityMap.get(skuProduct)) ? Collections.emptyList() :
							availabilityMap.get(skuProduct);
			final Set<String> serialProductCodes = stockLevelList.stream()
					.map(StockLevelModel::getSerialProductCode).collect(Collectors.toSet());

			stockLevel = Long.valueOf(serialProductCodes.size());
		}
		BlLogger.logFormatMessageInfo(LOG, Level.INFO,
				"available quantity {} for the product {} ", stockLevel, skuProduct);
		return stockLevel;
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
