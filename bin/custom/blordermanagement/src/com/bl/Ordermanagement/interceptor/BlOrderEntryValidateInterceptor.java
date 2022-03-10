package com.bl.Ordermanagement.interceptor;

import com.bl.Ordermanagement.actions.order.BlSourceOrderAction;
import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.services.impl.DefaultBlAllocationService;
import com.bl.Ordermanagement.services.impl.DefaultBlOrderModificationService;
import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.shipping.strategy.BlShippingOptimizationStrategy;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import javax.annotation.Resource;
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * This validator used to validate order entry for modified order
 *
 * @author Aditi Sharma
 */
public class BlOrderEntryValidateInterceptor implements ValidateInterceptor<OrderEntryModel>
{
	private static final Logger LOG = Logger.getLogger(BlOrderEntryValidateInterceptor.class);

	@Resource(name = "defaultBlAllocationService")
	private DefaultBlAllocationService defaultBlAllocationService;

	@Resource(name = "allocationService")
	private AllocationService allocationService;

	@Resource(name = "modelService")
	private ModelService modelService;

	@Resource(name = "userService")
	private UserService userService;

	@Resource(name="blSourceOrderAction")
	private BlSourceOrderAction blSourceOrderAction;

	@Resource(name="blShippingOptimizationStrategy")
	private BlShippingOptimizationStrategy blShippingOptimizationStrategy;

	@Resource(name="defaultBlUserService")
	private DefaultBlUserService defaultBlUserService;

	@Resource(name = "blStockLevelDao")
	private BlStockLevelDao blStockLevelDao;

	private DefaultBlESPEventService defaultBlESPEventService;

	@Resource(name="blOrderModificationService")
	private DefaultBlOrderModificationService blOrderModificationService;

	/**
	 * method will validate order entry for modified order
	 */
	@Override
	public void onValidate(final OrderEntryModel orderEntryModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		final List<BlSerialProductModel> serialProduct = orderEntryModel.getModifiedSerialProductList();
		final WarehouseModel warehouse = orderEntryModel.getWarehouse();
		if (getDefaultBlUserService().isCsUser())
		{
			if(((BlProductModel)orderEntryModel.getProduct()).isBundleProduct()){
				orderEntryModel.setBundleMainEntry(Boolean.TRUE);
			}
			if (orderEntryModel.getOrder().getIsRentalOrder() && CollectionUtils.isNotEmpty(serialProduct) && warehouse != null)
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Modifiy order {} ", orderEntryModel.getOrder().getCode());
				checkForOrderModification(orderEntryModel, interceptorContext, serialProduct, warehouse);
			}
			else if(CollectionUtils.isEmpty(serialProduct) && warehouse == null)
			{
				modifyUsedGearOrder(orderEntryModel,interceptorContext);
			}

		}
	}

	/**
	 * This method is used to modify used gear order
	 * @param orderEntryModel
	 * @param interceptorContext
	 */
	private void modifyUsedGearOrder(final OrderEntryModel orderEntryModel,final InterceptorContext interceptorContext)
	{
		if (!interceptorContext.isNew(orderEntryModel) && orderEntryModel.isIsModifiedOrder()
				&& interceptorContext.isModified(orderEntryModel, AbstractOrderEntryModel.ISMODIFIEDORDER)) {
			final SourcingResults resultsForUsedGearOrder = getBlOrderModificationService().getResultsForUsedGearOrder(orderEntryModel);
			final SourcingResult sourcingResult = getBlOrderModificationService().createSourcingResultForUsedGear(orderEntryModel, resultsForUsedGearOrder);
			final Optional<ConsignmentModel> consignmentModel = getBlOrderModificationService().checkifConsignmentIsPresent(orderEntryModel,sourcingResult);
			if (consignmentModel.isPresent()) {
				createNewConsignmentEntry(orderEntryModel, sourcingResult, consignmentModel.get());
			} else {
				getBlOrderModificationService().createConsignmentForModifiedOrder(orderEntryModel, resultsForUsedGearOrder);
			}
			final BlSerialProductModel updatedSerialProduct =  (BlSerialProductModel) orderEntryModel.getProduct();
			updatedSerialProduct.setSerialStatus(SerialStatusEnum.SOLD);
			updatedSerialProduct.setHardAssigned(Boolean.TRUE);
			interceptorContext.getModelService().save(updatedSerialProduct);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Modified order {} for serial product ", orderEntryModel.getOrder().getCode(),updatedSerialProduct.getCode());

		}
	}

	/**
	 * method will check for order modification from cs
	 * @param orderEntryModel
	 * @param interceptorContext
	 * @param serialProduct
	 * @param warehouse
	 */
	private void checkForOrderModification(final OrderEntryModel orderEntryModel, final InterceptorContext interceptorContext,
										   final List<BlSerialProductModel> serialProduct, final WarehouseModel warehouse)
	{
		if (!interceptorContext.isNew(orderEntryModel) && orderEntryModel.isIsModifiedOrder()
				&& interceptorContext.isModified(orderEntryModel, AbstractOrderEntryModel.ISMODIFIEDORDER))
		{
			isOrderModified(orderEntryModel, serialProduct, warehouse);
			orderEntryModel.setUpdatedTime(new Date());
			final AbstractOrderModel abstractOrderModel = orderEntryModel.getOrder();
			abstractOrderModel.setOrderModifiedDate(new Date());
			abstractOrderModel.setUpdatedTime(new Date());
			modelService.save(abstractOrderModel);
			modelService.refresh(abstractOrderModel);
			BlLogger.logFormattedMessage(LOG , Level.DEBUG , "order{} is modified and updated with OrderModifiedDate {} and UpdatedTime {} " ,
					orderEntryModel.getOrder().getCode() , abstractOrderModel.getOrderModifiedDate() , abstractOrderModel.getUpdatedTime());
		}
	}

	/**
	 * method will be called when there is any modification on order
	 * @param orderEntryModel
	 * @param serialProduct
	 * @param warehouse
	 */
	private void isOrderModified(final OrderEntryModel orderEntryModel, final List<BlSerialProductModel> serialProduct,
								 final WarehouseModel warehouse)
	{
		final SourcingLocation finalSourcingLocation = createSourcingLocation(orderEntryModel);
		final SourcingResult sourceResult = createSourceResult(orderEntryModel, serialProduct, finalSourcingLocation);
		final Optional<ConsignmentModel> consignmentModel = orderEntryModel.getOrder().getConsignments().stream()
				.filter(consignment -> consignment.getWarehouse().getCode().equals(warehouse.getCode())).findFirst();
		if (consignmentModel.isPresent())
		{
			createNewConsignmentEntry(orderEntryModel, sourceResult, consignmentModel.get());
		}
		else
		{
			createNewConsignment(orderEntryModel, sourceResult);
		}

		try {
			if (orderEntryModel.getOrder().getConsignments().stream().anyMatch(consignmentModel1 ->
					DateUtils.isSameDay(consignmentModel1.getOptimizedShippingStartDate(),
							orderEntryModel.getModifiedtime()))) {
				getDefaultBlESPEventService().sendOrderPullBackItemsAdded(orderEntryModel.getOrder() , orderEntryModel);
			}
		}
		catch (final Exception e){
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY ,  e,
					"BlOrderEntryValidateInterceptor :- Error while performing order pull back items added ESP Event for order {}", orderEntryModel.getOrder().getCode());
		}

	}

	/**
	 * method will be called when any order entry is created for order having the same warehouse whose consignment is already present.
	 * @param orderEntryModel
	 * @param sourceResult
	 * @param consignment
	 */
	private void createNewConsignmentEntry(final OrderEntryModel orderEntryModel, final SourcingResult sourceResult,
										   final ConsignmentModel consignment)
	{
		final Set<ConsignmentEntryModel> consignmentEntries = consignment.getConsignmentEntries();
		final Set<String> serialCodes = new HashSet<>();
		if(CollectionUtils.isNotEmpty(orderEntryModel.getModifiedSerialProductList()))
		{
			orderEntryModel.getModifiedSerialProductList().forEach(serialProduct -> serialCodes.add(serialProduct.getCode()));
		}
		if(!orderEntryModel.getOrder().getIsRentalOrder() && CollectionUtils.isEmpty(orderEntryModel.getModifiedSerialProductList()))
		{
			serialCodes.add(orderEntryModel.getProduct().getCode());
		}
		final ConsignmentEntryModel createConsignmentEntry = defaultBlAllocationService
				.createConsignmentEntry(orderEntryModel, orderEntryModel.getQuantity(), consignment, sourceResult);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment enrty created for order {}",consignment.getOrder().getCode());

		final Set<ConsignmentEntryModel> entries = new HashSet<>(consignmentEntries);
		entries.add(createConsignmentEntry);

		consignment.setConsignmentEntries(entries);

		if (consignment.getOrder().getIsRentalOrder()) {

			Collection<StockLevelModel> serialStocks = blStockLevelDao
					.findSerialStockLevelsForDateAndCodes(serialCodes, consignment.getOptimizedShippingStartDate(),
							consignment.getOptimizedShippingEndDate(), Boolean.FALSE);

			if(CollectionUtils.isNotEmpty(serialStocks))
			{
				serialStocks.forEach(stock -> {
					stock.setReservedStatus(true);
					stock.setOrder(orderEntryModel.getOrder().getCode());
				});
				modelService.saveAll(serialStocks);
			}
		}
		modelService.save(consignment);
		modelService.refresh(consignment);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment Entry created for consignment {} and order {}.", consignment.getCode(),consignment.getOrder().getCode());
		getBlOrderModificationService().recalculateOrder(orderEntryModel.getOrder());
	}

	/** method will be called when any order entry is created for order .
	 * @param orderEntryModel
	 * @param sourceResult
	 */
	private void createNewConsignment(final OrderEntryModel orderEntryModel, SourcingResult sourceResult)
	{
		final SourcingResults sourcingResults = new SourcingResults();
		final Set<SourcingResult> results = new HashSet<>();
		results.add(sourceResult);
		sourcingResults.setResults(results);
		getBlOrderModificationService().createConsignmentForModifiedOrder(orderEntryModel,sourcingResults);
	}


	/**
	 * method will be used to create sourcing location data
	 * @param orderEntryModel
	 * @return
	 */
	private SourcingLocation createSourcingLocation(final OrderEntryModel orderEntryModel)
	{
		final SourcingContext context = new SourcingContext();

		orderEntryModel.getOrder().getEntries().forEach(orderEntry -> {

			if(orderEntry.getPk().toString().equals(orderEntryModel.getPk().toString()))
			{
				context.setOrderEntries(Arrays.asList(orderEntryModel));
			}
		});

		final SourcingLocation sourcingLocation = new SourcingLocation();
		sourcingLocation.setWarehouse(orderEntryModel.getWarehouse());
		final Map<String, Long> allocationMap = new HashedMap()
				;
		allocationMap.put(orderEntryModel.getProduct().getCode() + BlOrdermanagementConstants.UNDER_SCORE + orderEntryModel.getEntryNumber(), orderEntryModel.getQuantity());
		sourcingLocation.setAllocatedMap(allocationMap);
		sourcingLocation.setContext(context);

		return  blShippingOptimizationStrategy.getProductAvailabilityForThreeDayGround(context,
				sourcingLocation);

	}

	/**
	 * method is used to create source result for update consignment
	 *
	 * @param serialProduct
	 * @param finalSourcingLocation
	 */
	private SourcingResult createSourceResult(final OrderEntryModel orderEntryModel,
											  final List<BlSerialProductModel> serialProduct, SourcingLocation finalSourcingLocation)
	{

		final SourcingResult result = new SourcingResult();
		final Map<AbstractOrderEntryModel, Long> allocationMap = new HashedMap();
		allocationMap.put(orderEntryModel, orderEntryModel.getQuantity());

		final Map<Integer, Set<BlSerialProductModel>> serialProductMap = new HashedMap();
		final Set<BlSerialProductModel> serialProductSet = new HashSet<>(serialProduct);
		serialProductMap.put(orderEntryModel.getEntryNumber(), serialProductSet);

		result.setAllocation(allocationMap);
		result.setWarehouse(orderEntryModel.getWarehouse());
		result.setSerialProductMap(serialProductMap);

		if (OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode().equals(finalSourcingLocation.getGroundAvailabilityCode())
				&& finalSourcingLocation.isGroundAvailability())
		{
			result.setThreeDayGroundAvailability(finalSourcingLocation.isGroundAvailability());
		}

		return result;
	}

	/**
	 * @return the defaultBlUserService
	 */
	public DefaultBlUserService getDefaultBlUserService()
	{
		return defaultBlUserService;
	}

	/**
	 * @param defaultBlUserService the defaultBlUserService to set
	 */
	public void setDefaultBlUserService(DefaultBlUserService defaultBlUserService)
	{
		this.defaultBlUserService = defaultBlUserService;
	}

	public DefaultBlESPEventService getDefaultBlESPEventService() {
		return defaultBlESPEventService;
	}

	public void setDefaultBlESPEventService(
			DefaultBlESPEventService defaultBlESPEventService) {
		this.defaultBlESPEventService = defaultBlESPEventService;
	}

	public DefaultBlOrderModificationService getBlOrderModificationService() {
		return blOrderModificationService;
	}

	public void setBlOrderModificationService(DefaultBlOrderModificationService blOrderModificationService) {
		this.blOrderModificationService = blOrderModificationService;
	}

}
