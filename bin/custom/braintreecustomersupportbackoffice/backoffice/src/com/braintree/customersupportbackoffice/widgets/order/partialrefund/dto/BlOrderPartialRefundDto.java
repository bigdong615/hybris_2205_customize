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

  private Double lineItemShippingPrice;

  private Double lineItemTax;

  private Double lineItemDamageWaiverCost;

  public BlOrderPartialRefundDto(final AbstractOrderEntryModel entryModel) {
    this.orderEntry = entryModel;
    this.lineItemRefundAmount = 0.0;
    this.lineItemShippingPrice = 0.0;
    this.lineItemTax = 0.0;
    this.lineItemDamageWaiverCost = 0.0;
  }

  public BlOrderPartialRefundDto(final AbstractOrderEntryModel orderEntry,
      final Double lineItemShippingPrice, final Double lineItemTax,
      final Double lineItemDamageWaiverCost, final Double lineItemRefundAmount) {
    this.orderEntry = orderEntry;
    this.lineItemShippingPrice = lineItemShippingPrice;
    this.lineItemTax = lineItemTax;
    this.lineItemDamageWaiverCost = lineItemDamageWaiverCost;
    this.lineItemRefundAmount = lineItemRefundAmount;
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

  public Double getLineItemShippingPrice() {
    return lineItemShippingPrice;
  }

  public void setLineItemShippingPrice(final Double lineItemShippingPrice) {
    this.lineItemShippingPrice = lineItemShippingPrice;
  }

  public Double getLineItemTax() {
    return lineItemTax;
  }

  public void setLineItemTax(final Double lineItemTax) {
    this.lineItemTax = lineItemTax;
  }

  public Double getLineItemDamageWaiverCost() {
    return lineItemDamageWaiverCost;
  }

  public void setLineItemDamageWaiverCost(final Double lineItemDamageWaiverCost) {
    this.lineItemDamageWaiverCost = lineItemDamageWaiverCost;
  }
}
