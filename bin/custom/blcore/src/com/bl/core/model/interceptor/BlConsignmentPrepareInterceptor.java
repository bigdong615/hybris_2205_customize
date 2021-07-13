package com.bl.core.model.interceptor;

import com.bl.core.model.NotesModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is for setting order and other consignments in to order notes of the order.
 *
 * @author Sunil
 */
public class BlConsignmentPrepareInterceptor implements PrepareInterceptor<ConsignmentModel> {

  private static final Logger LOG = Logger.getLogger(BlConsignmentPrepareInterceptor.class);

  @Override
  public void onPrepare(final ConsignmentModel consignmentModel,
      final InterceptorContext interceptorContext) throws InterceptorException {

    final AbstractOrderModel abstractOrderModel = consignmentModel.getOrder();

    Set<ConsignmentModel> otherConsignmentModels = new HashSet<>(abstractOrderModel.getConsignments());
    otherConsignmentModels.remove(consignmentModel);

    List<NotesModel> orderNotesFromConsignment = consignmentModel.getOrderNotes();

    if (interceptorContext.isModified(consignmentModel, ConsignmentModel.ORDERNOTES)
        && CollectionUtils.isNotEmpty(orderNotesFromConsignment)) {

      setOrderAndOtherConsignmentsInNotes(abstractOrderModel, otherConsignmentModels,
          orderNotesFromConsignment, interceptorContext);
    }

  }

  private void setOrderAndOtherConsignmentsInNotes(final AbstractOrderModel abstractOrderModel,
      final Set<ConsignmentModel> otherConsignmentModels,
      List<NotesModel> orderNotesFromConsignment, final InterceptorContext interceptorContext) {

    orderNotesFromConsignment.forEach(orderNote -> {
      List<ConsignmentModel> orderNoteConsignments = new ArrayList<>(orderNote.getConsignment());
      orderNoteConsignments.addAll(otherConsignmentModels);
      orderNote.setConsignment(orderNoteConsignments);
      orderNote.setOrder(abstractOrderModel);
    });
    interceptorContext.getModelService().saveAll(orderNotesFromConsignment);

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignments and Order are set in to consignment order Notes");
  }

}
