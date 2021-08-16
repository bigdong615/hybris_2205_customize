package com.bl.core.model.interceptor;

import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.services.order.note.BlOrderNoteService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
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
    setSerialCodesToBillingCharges(consignmentModel, interceptorContext);
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
	 * Sets the serial codes to billing charges.
	 *
	 * @param consignmentModel
	 *           the consignment model
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void setSerialCodesToBillingCharges(final ConsignmentModel consignmentModel,
			final InterceptorContext interceptorContext)
	{
		final boolean needToSave = BooleanUtils.negate(interceptorContext.isNew(consignmentModel));
		consignmentModel.getConsignmentEntries().forEach(
				consignmentEntryModel -> addSerialCodeListToChargeMap(consignmentEntryModel, needToSave, interceptorContext));
	}

	/**
	 * Adds the serial code list to billing charge map.
	 *
	 * @param consignmentEntryModel
	 *           the consignment entry model
	 * @param isModified
	 *           the is modified
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void addSerialCodeListToChargeMap(final ConsignmentEntryModel consignmentEntryModel, final boolean needToSave,
			final InterceptorContext interceptorContext)
	{
		final List<BlProductModel> serialProducts = Lists
				.newArrayList(CollectionUtils.emptyIfNull(consignmentEntryModel.getSerialProducts()));
		Map<String, List<BlItemsBillingChargeModel>> billingCharges = MapUtils
				.emptyIfNull(consignmentEntryModel.getBillingCharges());
		if (MapUtils.isEmpty(billingCharges))
		{
			billingCharges = serialProducts.stream()
					.collect(Collectors.toMap(BlProductModel::getCode, item -> new ArrayList<BlItemsBillingChargeModel>()));
		}
		consignmentEntryModel.setBillingCharges(billingCharges);
		if (needToSave)
		{
			interceptorContext.getModelService().save(consignmentEntryModel);
		}
	}

  public BlOrderNoteService getBlOrderNoteService() {
    return blOrderNoteService;
  }

  public void setBlOrderNoteService(BlOrderNoteService blOrderNoteService) {
    this.blOrderNoteService = blOrderNoteService;
  }

}
