package com.bl.core.model.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.services.order.note.BlOrderNoteService;
import com.bl.logging.BlLogger;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is for setting order and other consignments in to order notes of the order.
 *
 * @author Sunil
 */
public class BlConsignmentPrepareInterceptor implements PrepareInterceptor<ConsignmentModel> {

  private static final Logger LOG = Logger.getLogger(BlConsignmentPrepareInterceptor.class);
  private BlOrderNoteService blOrderNoteService;
  private DefaultBlESPEventService blEspEventService;

  @Override
  public void onPrepare(final ConsignmentModel consignmentModel,
      final InterceptorContext interceptorContext) throws InterceptorException {
	  setShipmentBlShippedStatusDate(consignmentModel, interceptorContext);
    final AbstractOrderModel abstractOrderModel = consignmentModel.getOrder();

    if (null != abstractOrderModel) {

      final Set<ConsignmentModel> otherConsignmentModels = new HashSet<>(
          abstractOrderModel.getConsignments());
      otherConsignmentModels.remove(consignmentModel);

      final List<NotesModel> orderNotesFromConsignment = consignmentModel.getOrderNotes();

      if (interceptorContext.isModified(consignmentModel, ConsignmentModel.ORDERNOTES)) {
        if (CollectionUtils.isNotEmpty(orderNotesFromConsignment)) {
          setOrderAndOtherConsignmentsInNotes(abstractOrderModel, otherConsignmentModels,
              orderNotesFromConsignment, interceptorContext);
        }
        //Setting consolidated Notes on order which can be used to display order notes in backoffice view
        getBlOrderNoteService().setConsolidatedNoteOnOrder(abstractOrderModel);
        interceptorContext.getModelService().save(abstractOrderModel);
      }
    }

    changePriorityStatusOnSerial(consignmentModel, interceptorContext); //BL-822 AC.4
    triggerEspReadyForPickupEvent(consignmentModel, interceptorContext);
    triggerEspPickedUpEvent(consignmentModel, interceptorContext);

    if(interceptorContext.isModified(consignmentModel, ConsignmentModel.STATUS) && (ConsignmentStatus.RECEIVED_READY_TO_SHIP.equals(consignmentModel.getStatus()) ||
        ConsignmentStatus.RECEIVED_READY_FOR_PICKUP.equals(consignmentModel.getStatus())) && Objects.nonNull(abstractOrderModel)) {
      abstractOrderModel.setOrderModifiedDate(new Date());
      interceptorContext.getModelService().save(abstractOrderModel);
      interceptorContext.getModelService().refresh(abstractOrderModel);
    }
  }

  /**
   * Update consignment and order in order notes.
   *
   * @param abstractOrderModel - the order model
   * @param otherConsignmentModels      - list of other consignments
   * @param orderNotesFromConsignment - order notes
   * @param interceptorContext      - interceptorContext
   */
  private void setOrderAndOtherConsignmentsInNotes(final AbstractOrderModel abstractOrderModel,
      final Set<ConsignmentModel> otherConsignmentModels,
      final List<NotesModel> orderNotesFromConsignment,
      final InterceptorContext interceptorContext) {

    orderNotesFromConsignment.forEach(orderNote -> {
      final Set<ConsignmentModel> orderNoteConsignments = new HashSet<>(orderNote.getConsignment());
      orderNoteConsignments.addAll(otherConsignmentModels);
      orderNote.setConsignment(orderNoteConsignments);
      orderNote.setOrder(abstractOrderModel);
    });
    interceptorContext.getModelService().saveAll(orderNotesFromConsignment);

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignments and Order are set in to consignment order Notes");
  }
  
  /**
   * Change priority status on serial if shipping date is changed.
   *
   * @param consignmentModel
   *           the consignment model
   * @param interceptorContext
   *           the interceptor context
   */
  private void changePriorityStatusOnSerial(final ConsignmentModel consignmentModel, final InterceptorContext interceptorContext)
  {
	  if (!interceptorContext.isNew(consignmentModel)
			  && interceptorContext.isModified(consignmentModel, ConsignmentModel.OPTIMIZEDSHIPPINGSTARTDATE)
			  && Objects.nonNull(consignmentModel.getOptimizedShippingStartDate()))
	  {
		  final Object previousShippingDate = consignmentModel.getItemModelContext()
				  .getOriginalValue(ConsignmentModel.OPTIMIZEDSHIPPINGSTARTDATE);
		  if (previousShippingDate instanceof Date
				  && !DateUtils.isSameDay(((Date) previousShippingDate), consignmentModel.getOptimizedShippingStartDate())
				  && DateUtils.isSameDay(((Date) previousShippingDate), new Date()))
		  {
			  consignmentModel.getConsignmentEntries().forEach(consignmentEntry -> consignmentEntry.getSerialProducts()
					  .forEach(entryItem -> checkAndChangePriorityStatusOnSerial(entryItem, interceptorContext)));
		  }
	  }
  }

  /**
   * Check and change priority status on serial.
   *
   * @param entryItem
   *           the entry item
   * @param interceptorContext
   *           the interceptor context
   */
  private void checkAndChangePriorityStatusOnSerial(final BlProductModel entryItem, final InterceptorContext interceptorContext)
  {
	  if (entryItem instanceof BlSerialProductModel)
	  {
		  final BlSerialProductModel serialItem = ((BlSerialProductModel) entryItem);
		  if (serialItem.isDirtyPriorityStatus())
		  {
			  serialItem.setDirtyPriorityStatus(Boolean.FALSE);
			  interceptorContext.getModelService().save(serialItem);
			  interceptorContext.getModelService().refresh(serialItem);
		  }
	  }
  }

  /**
   * This method created to trigger the ESP event for ready for pickup order
   * @param consignmentModel  consignmentModel
   * @param interceptorContext interceptorContext
   */
  public void triggerEspReadyForPickupEvent(final ConsignmentModel consignmentModel,
      final InterceptorContext interceptorContext){
    final WarehouseModel warehouses = consignmentModel.getWarehouse();
    final String deliveryMode = Objects.nonNull(consignmentModel.getDeliveryMode()) ? consignmentModel.getDeliveryMode().getCode() : StringUtils.EMPTY;
    if(interceptorContext.isModified(consignmentModel, ConsignmentModel.STATUS) && ConsignmentStatus.READY_FOR_PICKUP.equals(consignmentModel.getStatus())
        && Objects.nonNull(warehouses) && (StringUtils.isNotBlank(deliveryMode)
        && (StringUtils.containsIgnoreCase(BlCoreConstants.BL_WALTHAM , deliveryMode) ||
        StringUtils.containsIgnoreCase(BlCoreConstants.BL_SAN_CARLOS , deliveryMode)))){
       final OrderModel orderModel = (OrderModel) consignmentModel.getOrder();
       orderModel.setStatus(OrderStatus.RECEIVED_READY_FOR_PICKUP);
      interceptorContext.getModelService().save(orderModel);
      interceptorContext.getModelService().refresh(orderModel);
      try{
        getBlEspEventService().sendOrderReadyForPickupEvent((OrderModel) consignmentModel.getOrder());
      }
      catch (final Exception exception){
        BlLogger.logMessage(LOG , Level.ERROR , "Error while executing OrderReadyForPickup ESP Event" , exception);
      }
    }

  }

  /**
   * This method created to trigger the ESP event for picked up order
   *
   * @param consignmentModel   consignmentModel
   * @param interceptorContext interceptorContext
   */
  private void triggerEspPickedUpEvent(final ConsignmentModel consignmentModel,
      final InterceptorContext interceptorContext) {
    if (interceptorContext.isModified(consignmentModel, ConsignmentModel.STATUS)
        && ConsignmentStatus.PICKED_UP.equals(consignmentModel.getStatus())) {
      final OrderModel orderModel = (OrderModel) consignmentModel.getOrder();
      orderModel.setStatus(OrderStatus.RECEIVED_PICKED_UP);
      interceptorContext.getModelService().save(orderModel);
      interceptorContext.getModelService().refresh(orderModel);
      try {
        getBlEspEventService().sendOrderPickedUpEvent(orderModel);
      } catch (final Exception exception) {
        BlLogger
            .logMessage(LOG, Level.ERROR, "Error while executing Picked Up ESP Event", exception);
      }
    }
  }

  public BlOrderNoteService getBlOrderNoteService() {
    return blOrderNoteService;
  }

  public void setBlOrderNoteService(BlOrderNoteService blOrderNoteService) {
    this.blOrderNoteService = blOrderNoteService;
  }
  public DefaultBlESPEventService getBlEspEventService(){

    return blEspEventService;

  }
  public void setBlEspEventService(final DefaultBlESPEventService blEspEventService){

    this.blEspEventService = blEspEventService;

  }
  
  /**
   * Sets the shipment bl shipped status date.
   *
   * @param consignment
   *           the consignment
   * @param interceptorContext
   *           the interceptor context
   */
  private void setShipmentBlShippedStatusDate(final ConsignmentModel consignment, final InterceptorContext interceptorContext)
  {
	  if (interceptorContext.isModified(consignment, ConsignmentModel.STATUS)
			  && consignment.getStatus().getCode().equals(ConsignmentStatus.BL_SHIPPED.getCode()))
	  {
		  consignment.setShipmentBlShippedStatusDate(new Date());
	  }
  }

}
