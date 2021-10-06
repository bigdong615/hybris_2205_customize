package com.bl.core.model.interceptor;


import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.NotesEnum;
import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.services.order.note.BlOrderNoteService;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.shipping.strategy.impl.DefaultBlShippingOptimizationStrategy;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.google.common.collect.Lists;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.order.strategies.impl.EventPublishingSubmitOrderStrategy;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ItemModelContextImpl;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * This class is for setting consignments in to each order notes of the order.
 *
 * @author Sunil
 */
public class BlOrderPrepareInterceptor implements PrepareInterceptor<AbstractOrderModel> {

  private static final Logger LOG = Logger.getLogger(BlOrderPrepareInterceptor.class);
  private BlOrderNoteService blOrderNoteService;
  private EventPublishingSubmitOrderStrategy eventPublishingSubmitOrderStrategy;
  private DefaultBlESPEventService blEspEventService;
  
	@Resource(name = "blDeliveryModeService")
	private BlDeliveryModeService blDeliveryModeService;

	@Resource(name = "defaultBlUserService")
	private DefaultBlUserService defaultBlUserService;

	@Resource(name = "modelService")
	private ModelService modelService;

	@Resource(name = "blShippingOptimizationStrategy")
	private DefaultBlShippingOptimizationStrategy blShippingOptimizationStrategy;

	@Resource(name = "blConsignmentEntryService")
	BlConsignmentEntryService blConsignmentEntryService;

  @Override
  public void onPrepare(final AbstractOrderModel abstractOrderModel,
      final InterceptorContext interceptorContext) throws InterceptorException {
	  
	  if (getDefaultBlUserService().isCsUser() && (interceptorContext.isModified(abstractOrderModel, AbstractOrderModel.RENTALSTARTDATE)
				|| interceptorContext.isModified(abstractOrderModel, AbstractOrderModel.RENTALENDDATE)))
		{
			modifyOrderDate(abstractOrderModel);
		}
		
     final Set<ConsignmentModel> consignmentModels = abstractOrderModel.getConsignments();
    if (interceptorContext.isModified(abstractOrderModel, AbstractOrderModel.ORDERNOTES)) {
		if (CollectionUtils.isNotEmpty(consignmentModels)) {
        //set order notes
        setConsignmentsInNotes(abstractOrderModel, consignmentModels, interceptorContext);
      }
      //Setting consolidated Notes on order which can be used to display order notes in backoffice view
      getBlOrderNoteService().setConsolidatedNoteOnOrder(abstractOrderModel);
    }
    doChangeDirtyPriorityStatus(abstractOrderModel, interceptorContext);  //BL-822 AC.3

    //BL-1052 internal order transfer
    if (!interceptorContext.isNew(abstractOrderModel) && interceptorContext
        .isModified(abstractOrderModel, AbstractOrderModel.CREATECONSIGNMENT)) {

      final ItemModelContextImpl itemModelCtx = (ItemModelContextImpl) abstractOrderModel
          .getItemModelContext();
      final Object originalValue = itemModelCtx.getOriginalValue(AbstractOrderModel.CREATECONSIGNMENT);

      if (Objects.nonNull(originalValue) && BooleanUtils.isFalse((Boolean) originalValue)
          && BooleanUtils.isTrue(abstractOrderModel.getInternalTransferOrder()) && CollectionUtils
          .isEmpty(abstractOrderModel.getConsignments())) {

        getEventPublishingSubmitOrderStrategy().submitOrder((OrderModel)abstractOrderModel);
      }
    }
    try {
      triggerEspPaymentDeclined(abstractOrderModel, interceptorContext);
      triggerEspVerificationRequired(abstractOrderModel, interceptorContext);
      triggerEspShipped(abstractOrderModel, interceptorContext);
			triggerNewShippingInfoEvent(abstractOrderModel, interceptorContext);
			triggerExceptionExtraItemEvent(abstractOrderModel,interceptorContext);
    }
    catch (final Exception e){
      BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
          "Event API call failed", e);
    }
  }


  /**
   * Do change dirty priority status on serial if Order is cancelled and shipping date is current date.
   *
   * @param abstractOrderModel the abstract order model
   * @param interceptorContext the interceptor context
   */
  private void doChangeDirtyPriorityStatus(final AbstractOrderModel abstractOrderModel,
	      final InterceptorContext interceptorContext)
  {
	  if(!interceptorContext.isNew(abstractOrderModel) && OrderStatus.CANCELLED.equals(abstractOrderModel.getStatus()))
	  {
		  final List<ConsignmentModel> consignmentsForCurrentDate = getConsignmentsForCurrentDate(abstractOrderModel);
		  if(CollectionUtils.isNotEmpty(consignmentsForCurrentDate))
		  {
			  consignmentsForCurrentDate.forEach(consignment -> consignment.getConsignmentEntries()
					  .forEach(consignmentEntry -> consignmentEntry.getSerialProducts()
							  .forEach(entryItem -> checkAndChangePriorityStatusOnSerial(entryItem, interceptorContext))));
		  }
	  }
  }
  
  /**
   * Gets the consignments for current date.
   *
   * @param abstractOrderModel the abstract order model
   * @return the consignments for current date
   */
  private List<ConsignmentModel> getConsignmentsForCurrentDate(final AbstractOrderModel abstractOrderModel)
  {
	  final Date currentDate = new Date();
	  List<ConsignmentModel> filteredConsignment = abstractOrderModel.getConsignments().stream()
			  .filter(consignment -> {
				  if(Objects.nonNull(consignment.getOptimizedShippingStartDate()))
				  {
					  return DateUtils.isSameDay(consignment.getOptimizedShippingStartDate(), currentDate);
				  }
				  return false;
			  }).collect(Collectors.toList());
	  BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Number of Consignmnets found for current date : {} is : {} on Order with code : {}",
			  currentDate, filteredConsignment.size(), abstractOrderModel.getCode());
	  return filteredConsignment;
  }
  
  /**
   * Check and change priority status on serial.
   *
   * @param entryItem the entry item
   * @param interceptorContext the interceptor context
   */
  private void checkAndChangePriorityStatusOnSerial(final BlProductModel entryItem, final InterceptorContext interceptorContext)
  {
	  if(entryItem instanceof BlSerialProductModel)
	  {
		  final BlSerialProductModel serialItem = ((BlSerialProductModel)entryItem);
		  if(serialItem.isDirtyPriorityStatus()) {
			  serialItem.setDirtyPriorityStatus(Boolean.FALSE);
			  interceptorContext.getModelService().save(serialItem);
			  interceptorContext.getModelService().refresh(serialItem);
		  }
	  }
  }

  
  /**
   * Update consignment in order notes.
   *
   * @param abstractOrderModel - the order model
   * @param consignmentModels      - list of consignments
   * @param interceptorContext      - interceptorContext
   */
  private void setConsignmentsInNotes(final AbstractOrderModel abstractOrderModel,
      final Set<ConsignmentModel> consignmentModels, final InterceptorContext interceptorContext) {

    final List<NotesModel> orderNotesFromOrder = abstractOrderModel.getOrderNotes();
    orderNotesFromOrder.forEach(orderNote -> {
      final Set<ConsignmentModel> orderNoteConsignments = new HashSet<>(orderNote.getConsignment());
      orderNoteConsignments.addAll(consignmentModels);
      orderNote.setConsignment(orderNoteConsignments);
    });
    interceptorContext.getModelService().saveAll(orderNotesFromOrder);

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignments are set in to Order order Notes");
  }
  /**
   * trigger Esp Payment Declined event
   *
   * @param abstractOrderModel the abstract order model
   * @param interceptorContext the interceptor context
   */
  private void triggerEspPaymentDeclined(final AbstractOrderModel abstractOrderModel,
      final InterceptorContext interceptorContext) {
    if (interceptorContext.isModified(abstractOrderModel, AbstractOrderModel.STATUS) && abstractOrderModel instanceof OrderModel && OrderStatus.PAYMENT_DECLINED.equals(abstractOrderModel.getStatus())) {
      try
      {
        getBlEspEventService().sendOrderPaymentDeclinedEvent(((OrderModel) abstractOrderModel));
      }catch (final Exception e)
      {
        BlLogger.logMessage(LOG,Level.ERROR,"Failed to trigger payment declined event.",e);
      }

    }
  }

  /**
   * trigger Esp Shipped event
   *
   * @param abstractOrderModel the abstract order model
   * @param interceptorContext the interceptor context
   */
  private void triggerEspShipped(final AbstractOrderModel abstractOrderModel, final InterceptorContext interceptorContext) {
    if(interceptorContext.isModified(abstractOrderModel, AbstractOrderModel.STATUS)  && abstractOrderModel instanceof OrderModel
        && OrderStatus.SHIPPED.equals(abstractOrderModel.getStatus())){

      final AtomicBoolean isEligibleToTrigger = new AtomicBoolean(Boolean.FALSE);
      final Set<ConsignmentModel> consignments = abstractOrderModel.getConsignments();

      for(ConsignmentModel consignmentModel : consignments){
        final WarehouseModel warehouses = consignmentModel.getWarehouse();
        final String deliveryMode = Objects.nonNull(consignmentModel.getDeliveryMode()) ? consignmentModel.getDeliveryMode().getCode() : StringUtils.EMPTY;
        if(consignmentModel.getStatus().equals(ConsignmentStatus.BL_SHIPPED) && Objects.nonNull(warehouses) && (StringUtils.isNotBlank(deliveryMode)
            && (!StringUtils.containsIgnoreCase(BlCoreConstants.BL_WALTHAM , deliveryMode) ||
            !StringUtils.containsIgnoreCase(BlCoreConstants.BL_SAN_CARLOS , deliveryMode)))){
          isEligibleToTrigger.set(Boolean.TRUE);
        }
        else {
          isEligibleToTrigger.set(Boolean.FALSE);
          break;
        }
    }
      if(isEligibleToTrigger.get()){
        getBlEspEventService().sendOrderShippedEvent((OrderModel) abstractOrderModel);
      }
    }
  }
	/**
	 * trigger Esp New Shipping Info
	 *
	 * @param abstractOrderModel the abstract order model
	 * @param interceptorContext the interceptor context
	 */
	private void triggerNewShippingInfoEvent(final AbstractOrderModel abstractOrderModel, final InterceptorContext interceptorContext) {
		if(getDefaultBlUserService().isCsUser() && interceptorContext.isModified(abstractOrderModel, AbstractOrderModel.DELIVERYADDRESS) && abstractOrderModel instanceof OrderModel){
			getBlEspEventService().sendOrderNewShippingEvent((OrderModel) abstractOrderModel);
		}
	}

		/**
     * trigger Esp verification required event
     *
     * @param abstractOrderModel the abstract order model
     * @param interceptorContext the interceptor context
     */
  private void triggerEspVerificationRequired(final AbstractOrderModel abstractOrderModel,
      final InterceptorContext interceptorContext) {
    if (abstractOrderModel.getStatus() != null && abstractOrderModel.getStatus().equals(OrderStatus.INVERIFICATION) && interceptorContext
        .isModified(abstractOrderModel, AbstractOrderModel.STATUS)) {
      try
      {
        getBlEspEventService().sendOrderVerificationRequiredEvent((OrderModel) abstractOrderModel);
      }catch (final Exception e)
      {
        BlLogger.logMessage(LOG,Level.ERROR,"Failed to trigger verification Required Event",e);
      }
		}
  }
  
  /**
	 * This method will called when cs agent will modify order date
	 *
	 * @param abstractOrderModel
	 */
	private void modifyOrderDate(final AbstractOrderModel abstractOrderModel)
	{
		final DeliveryModeModel deliveryMode = abstractOrderModel.getDeliveryMode();
		final SourcingLocation sourcingLocation = new SourcingLocation();
		final AtomicBoolean isGroundAvailability = new AtomicBoolean();
		for (final ConsignmentModel consignmentModel : abstractOrderModel.getConsignments())
		{
			updareShippingOptimizationDate(abstractOrderModel, sourcingLocation, isGroundAvailability, consignmentModel);
		}
		if (deliveryMode instanceof ZoneDeliveryModeModel)
		{
			updateActualRentalDatesForOrder(abstractOrderModel, deliveryMode);
		}
	}

	/**
	 * It triggers Exception Extra Item event.
	 *
	 * @param abstractOrderModel the AbstractOrderModel
	 * @param interceptorContext the InterceptorContext
	 */
	private void triggerExceptionExtraItemEvent(final AbstractOrderModel abstractOrderModel,
			final InterceptorContext interceptorContext) {
		if (abstractOrderModel instanceof OrderModel && BooleanUtils
				.isTrue(getDefaultBlUserService().isTechEngUser()) && interceptorContext
				.isModified(abstractOrderModel, AbstractOrderModel.ORDERNOTES)) {
			List<NotesModel> modifiedOrderNotes = abstractOrderModel.getOrderNotes();
			List<Object> previousChangedOrderNotesList = getPreviousChangedOrderNotesList(
					abstractOrderModel);
			if (CollectionUtils.isNotEmpty(previousChangedOrderNotesList)) {
				modifiedOrderNotes.removeIf(previousChangedOrderNotesList::contains);
			}
			Optional<NotesModel> customerOwnedItemsNotes = modifiedOrderNotes.stream()
					.filter(note -> note.getType().equals(NotesEnum.CUSTOMER_OWNED_ITEMS_NOTES)).findAny();
			if (customerOwnedItemsNotes.isPresent()) {
				try {
					getBlEspEventService().sendOrderExtraItems((OrderModel) abstractOrderModel);
				} catch (final Exception exception) {
					BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger Exception extra item Event",
							exception);
				}
			}
		}
	}

	/**
	 * It fetches order Notes list.
	 * @param abstractOrderModel
	 * @return List of OrderNotes
	 */
	private List<Object> getPreviousChangedOrderNotesList(final AbstractOrderModel abstractOrderModel)
	{
		final Object previousValue = abstractOrderModel.getItemModelContext()
				.getOriginalValue(AbstractOrderModel.ORDERNOTES);
		if (previousValue instanceof List)
		{
			return Lists.newArrayList((List) previousValue);
		}
		return Collections.emptyList();
	}


	/**
	 * This method will be used to update shipping optimization date for rental duration change
	 *
	 * @param abstractOrderModel
	 * @param sourcingLocation
	 * @param isGroundAvailability
	 * @param consignmentModel
	 * @return
	 */
	private void updareShippingOptimizationDate(final AbstractOrderModel abstractOrderModel, SourcingLocation sourcingLocation,
			final AtomicBoolean isGroundAvailability, final ConsignmentModel consignmentModel)
	{
		final int result = BlDateTimeUtils.getBusinessDaysDifferenceWithCutOffTime(
				BlDateTimeUtils.convertStringDateToDate(
						BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
						BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN),
				abstractOrderModel.getRentalStartDate(), consignmentModel.getWarehouse().getCutOffTime());

		if (result >= BlInventoryScanLoggingConstants.THREE)
		{
			sourcingLocation = createSourcingLocation();
			isGroundAvailability
					.set(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode().equals(sourcingLocation.getGroundAvailabilityCode())
							&& sourcingLocation.isGroundAvailability());
			optimizeShippingMethodForModifiedOrder(consignmentModel, isGroundAvailability.get());
		}
		else
		{
			setFalseSourcingLocation(sourcingLocation);
		}
	}	
	
	/**
	 * This method will update the actual rental order date for order
	 *
	 * @param abstractOrderModel
	 * @param deliveryMode
	 */
	private void updateActualRentalDatesForOrder(final AbstractOrderModel abstractOrderModel, final DeliveryModeModel deliveryMode)
	{
		final ZoneDeliveryModeModel zoneDeliveryMode = (ZoneDeliveryModeModel) deliveryMode;
		final DateFormat dateFormat = new SimpleDateFormat(BlCoreConstants.DATE_FORMAT);

		BlDateTimeUtils.updateActualRentalStartDate(abstractOrderModel, zoneDeliveryMode, dateFormat);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "ActaualRentalStartDate updated to {} for order {}", abstractOrderModel.getActualRentalStartDate(),abstractOrderModel.getCode());
		
		BlDateTimeUtils.updateActualRentalEndDate(abstractOrderModel, zoneDeliveryMode, dateFormat);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "ActaualRentalEndDate updated to {} for order {}", abstractOrderModel.getActualRentalEndDate(),abstractOrderModel.getCode());
	}
	
	/**
	 * This method will create sourcing location data
	 *
	 * @return sourcingLocation
	 */
	private SourcingLocation createSourcingLocation()
	{
		final SourcingLocation sourcingLocation = new SourcingLocation();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Returning three day ground availability stock!!");
		sourcingLocation.setGroundAvailability(Boolean.TRUE);
		sourcingLocation.setGroundAvailabilityCode(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode());
		return sourcingLocation;
	}
	
	/**
	 * This method will optimize shipping method for modifiedOrder
	 *
	 * @param consignment
	 * @param isGroundAvailability
	 */
	private void optimizeShippingMethodForModifiedOrder(final ConsignmentModel consignment, final boolean isGroundAvailability)
	{
		if (!consignment.isOrderTransferConsignment())
		{
			try
			{
				consignment.setThreeDayGroundAvailability(isGroundAvailability);
				getBlShippingOptimizationStrategy().getOptimizedShippingMethodForOrder(consignment);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						"OptimizedShippingStartDate update to {} and OptimizedShippingEndDate update to {} for order {}",
						consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate(), consignment.getOrder().getCode());
			}
			catch (final Exception exception)
			{
				BlLogger.logMessage(LOG, Level.ERROR, exception.getMessage());
				throw exception;
			}
		}
	}

	/**
	 * method to set failure result for 3 day ground
	 *
	 * @param sourcingLocation
	 *           details
	 * @return Updated SourcingLocation
	 */
	private SourcingLocation setFalseSourcingLocation(final SourcingLocation sourcingLocation)
	{
		sourcingLocation.setGroundAvailability(Boolean.FALSE);
		sourcingLocation.setGroundAvailabilityCode(OptimizedShippingMethodEnum.DEFAULT.getCode());
		return sourcingLocation;
	}
	
  public BlOrderNoteService getBlOrderNoteService() {
    return blOrderNoteService;
  }

  public void setBlOrderNoteService(BlOrderNoteService blOrderNoteService) {
    this.blOrderNoteService = blOrderNoteService;
  }

  public EventPublishingSubmitOrderStrategy getEventPublishingSubmitOrderStrategy() {
    return eventPublishingSubmitOrderStrategy;
  }

  public void setEventPublishingSubmitOrderStrategy(
      final EventPublishingSubmitOrderStrategy eventPublishingSubmitOrderStrategy) {
    this.eventPublishingSubmitOrderStrategy = eventPublishingSubmitOrderStrategy;
  }
    public DefaultBlESPEventService getBlEspEventService(){
      return blEspEventService;
    }

    public void setBlEspEventService(final DefaultBlESPEventService blEspEventService){
      this.blEspEventService = blEspEventService;
    }



	public DefaultBlUserService getDefaultBlUserService() {
		return defaultBlUserService;
	}

	public void setDefaultBlUserService(
			DefaultBlUserService defaultBlUserService) {
		this.defaultBlUserService = defaultBlUserService;
	}


	/**
	 * @return the blShippingOptimizationStrategy
	 */
	public DefaultBlShippingOptimizationStrategy getBlShippingOptimizationStrategy()
	{
		return blShippingOptimizationStrategy;
	}


	/**
	 * @param blShippingOptimizationStrategy the blShippingOptimizationStrategy to set
	 */
	public void setBlShippingOptimizationStrategy(DefaultBlShippingOptimizationStrategy blShippingOptimizationStrategy)
	{
		this.blShippingOptimizationStrategy = blShippingOptimizationStrategy;
	}
}
