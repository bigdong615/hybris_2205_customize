package com.bl.core.model.interceptor;


import com.bl.core.model.NotesModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ItemModelContextImpl;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is for setting order notes from order to their respective consignments
 *
 * @author Sunil
 */
public class BlOrderPrepareInterceptor implements PrepareInterceptor<AbstractOrderModel> {

  private static final Logger LOG = Logger.getLogger(BlOrderPrepareInterceptor.class);

  @Override
  public void onPrepare(final AbstractOrderModel abstractOrderModel,
      final InterceptorContext interceptorContext) throws InterceptorException {

    final Object initialValue = getInitialValue(abstractOrderModel, AbstractOrderModel.ORDERNOTES);
    final Set<ConsignmentModel> consignmentModels = abstractOrderModel.getConsignments();
    if (null != initialValue && interceptorContext
        .isModified(abstractOrderModel, AbstractOrderModel.ORDERNOTES) && CollectionUtils
        .isNotEmpty(consignmentModels)) {

        //set order notes
        setOrderNotesInConsignments(abstractOrderModel, consignmentModels, interceptorContext);
    }


  }

  private void setOrderNotesInConsignments(final AbstractOrderModel abstractOrderModel,
      final Set<ConsignmentModel> consignmentModels, final InterceptorContext interceptorContext) {

    consignmentModels.forEach(consignmentModel -> {
      List<NotesModel> consignmentNotes = new ArrayList<>(consignmentModel.getOrderNotes());
      consignmentNotes.addAll(abstractOrderModel.getOrderNotes());
      consignmentModel.setOrderNotes(consignmentNotes);
      interceptorContext.getModelService().save(consignmentModel);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "Order notes {} is also set in consignment with code {}",
          abstractOrderModel.getOrderNotes(), consignmentModel.getCode());
    });
  }

  /**
   * It gets the initial value of the attribute before update
   *
   * @param order
   *           the order
   */
  private Object getInitialValue(final AbstractOrderModel order, final String status) {
    final ItemModelContextImpl itemModelCtx = (ItemModelContextImpl) order
        .getItemModelContext();
    return itemModelCtx.exists() ? itemModelCtx.getOriginalValue(status) : null;
  }

}
