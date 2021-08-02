package com.braintree.customersupportbackoffice.widgets.order.partialrefund.dto;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;

public class BlOrderPartialRefundDto implements Comparable<BlOrderPartialRefundDto> {

  private AbstractOrderEntryModel orderEntry;

  private Double lineItemRefundAmount;

  public BlOrderPartialRefundDto(final AbstractOrderEntryModel entryModel) {
    this.orderEntry = entryModel;
    this.lineItemRefundAmount = 0.0;
  }

  @Override
  public int compareTo(final BlOrderPartialRefundDto refundDto) {
    return Long.compare(this.getOrderEntry().getProduct().getPk().getLong(),
        refundDto.getOrderEntry().getProduct().getPk().getLong());
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
