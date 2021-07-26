package com.bl.core.services.order.note;

import de.hybris.platform.core.model.order.AbstractOrderModel;

public interface BlOrderNoteService {

  /**
   * Set consolidated note after concatenating all notes on order.
   *
   * @param abstractOrderModel - the order model
   *
   */
  void setConsolidatedNoteOnOrder(final AbstractOrderModel abstractOrderModel);
}
