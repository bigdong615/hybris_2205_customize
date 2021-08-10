package com.braintree.customersupportbackoffice.widgets.order.partialrefund.dto;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import java.io.Serializable;

/**
 * The type Bl order partial refund dto.
 *
 * @author Krishan Vashishth
 * 
 */
public class BlOrderPartialRefundDto implements Serializable {

  private AbstractOrderEntryModel orderEntry;

  private Double lineItemRefundAmount;

  public BlOrderPartialRefundDto(final AbstractOrderEntryModel entryModel) {
    this.orderEntry = entryModel;
    this.lineItemRefundAmount = 0.0;
  }

  public AbstractOrderEntryModel getOrderEntry() {
    return orderEntry;
  }

  public void setOrderEntry(final AbstractOrderEntryModel orderEntry) {
    this.orderEntry = orderEntry;
  }

  public Double getLineItemRefundAmount() {
    return lineItemRefundAmount;
  }

  public void setLineItemRefundAmount(final Double lineItemRefundAmount) {
    this.lineItemRefundAmount = lineItemRefundAmount;
  }
}
