package com.bl.core.model.interceptor;


import com.bl.core.model.NotesModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is for setting consignments in to each order notes of the order.
 *
 * @author Sunil
 */
public class BlOrderPrepareInterceptor implements PrepareInterceptor<AbstractOrderModel> {

  private static final Logger LOG = Logger.getLogger(BlOrderPrepareInterceptor.class);

  @Override
  public void onPrepare(final AbstractOrderModel abstractOrderModel,
      final InterceptorContext interceptorContext) throws InterceptorException {

    final Set<ConsignmentModel> consignmentModels = abstractOrderModel.getConsignments();
    if (interceptorContext.isModified(abstractOrderModel, AbstractOrderModel.ORDERNOTES) && CollectionUtils
        .isNotEmpty(consignmentModels)) {

        //set order notes
        setConsignmentsInNotes(abstractOrderModel, consignmentModels, interceptorContext);
    }

  }

  private void setConsignmentsInNotes(final AbstractOrderModel abstractOrderModel,
      final Set<ConsignmentModel> consignmentModels, final InterceptorContext interceptorContext) {

    List<NotesModel> orderNotesFromOrder = abstractOrderModel.getOrderNotes();
    orderNotesFromOrder.forEach(orderNote -> {
      List<ConsignmentModel> orderNoteConsignments = new ArrayList<>(orderNote.getConsignment());
      orderNoteConsignments.addAll(consignmentModels);
      orderNote.setConsignment(orderNoteConsignments);
    });
    interceptorContext.getModelService().saveAll(orderNotesFromOrder);

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignments are set in to Order order Notes");
  }

}
