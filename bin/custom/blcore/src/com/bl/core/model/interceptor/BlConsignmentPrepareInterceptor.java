package com.bl.core.model.interceptor;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.services.order.note.BlOrderNoteService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
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

  @Override
  public void onPrepare(final ConsignmentModel consignmentModel,
      final InterceptorContext interceptorContext) throws InterceptorException {

    final AbstractOrderModel abstractOrderModel = consignmentModel.getOrder();
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
    changePriorityStatusOnSerial(consignmentModel, interceptorContext); //BL-822 AC.4
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

  public BlOrderNoteService getBlOrderNoteService() {
    return blOrderNoteService;
  }

  public void setBlOrderNoteService(BlOrderNoteService blOrderNoteService) {
    this.blOrderNoteService = blOrderNoteService;
  }

}
