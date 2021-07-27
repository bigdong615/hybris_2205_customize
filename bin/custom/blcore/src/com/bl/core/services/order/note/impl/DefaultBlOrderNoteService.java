package com.bl.core.services.order.note.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.NotesModel;
import com.bl.core.services.order.note.BlOrderNoteService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.collections.CollectionUtils;

public class DefaultBlOrderNoteService implements BlOrderNoteService {

  private ModelService modelService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void setConsolidatedNoteOnOrder(final AbstractOrderModel abstractOrderModel) {
      if (null != abstractOrderModel) {
        abstractOrderModel.setConsolidatedOrderNote(getConcatenatedNotes(abstractOrderModel, new StringBuilder()));
      }
  }

  /**
   * Get all order notes after Concatenation
   *
   * @param abstractOrderModel - the order model
   * @param consolidatedOrderNotes - String Builder Object
   *
   */
  private String getConcatenatedNotes(final AbstractOrderModel abstractOrderModel,
      final StringBuilder consolidatedOrderNotes) {

    if(CollectionUtils.isNotEmpty(abstractOrderModel.getOrderNotes())){
      abstractOrderModel.getOrderNotes().stream().forEach(notesModel -> {
        concatNotes(notesModel, consolidatedOrderNotes);
      });
    }

    return consolidatedOrderNotes.toString();
  }

  /**
   * Concat all order notes
   *
   * @param notesModel - notes model
   * @param consolidatedOrderNotes - String Builder Object
   *
   */
  private StringBuilder concatNotes(final NotesModel notesModel,
      final StringBuilder consolidatedOrderNotes) {
    return consolidatedOrderNotes.append(notesModel.getType()).append(BlCoreConstants.HYPHEN)
        .append(notesModel.getNote()).append(BlCoreConstants.NEW_LINE_CHARACTER).append(BlCoreConstants.DELIMETER);
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

}
