package com.bl.core.model.interceptor;


import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.services.order.note.BlOrderNoteService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.strategies.impl.EventPublishingSubmitOrderStrategy;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ItemModelContextImpl;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
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

  @Override
  public void onPrepare(final AbstractOrderModel abstractOrderModel,
      final InterceptorContext interceptorContext) throws InterceptorException {

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
    triggerEspPaymentDeclined(abstractOrderModel, interceptorContext);
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
    if (!interceptorContext.isNew(abstractOrderModel) && interceptorContext
        .isModified(abstractOrderModel, AbstractOrderModel.STATUS) && abstractOrderModel.getStatus()
        .equals(OrderStatus.PAYMENT_DECLINED) && abstractOrderModel instanceof OrderModel) {
      getBlEspEventService().sendOrderPaymentDeclinedEvent(((OrderModel) abstractOrderModel));
    }
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
}
