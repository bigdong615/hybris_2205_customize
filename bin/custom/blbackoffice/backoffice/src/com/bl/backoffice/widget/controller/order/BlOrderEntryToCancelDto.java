package com.bl.backoffice.widget.controller.order;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.omsbackoffice.dto.OrderEntryToCancelDto;

import java.util.List;

/**
 * Custom DTO
 *
 * @author Namrata Lohar
 */
public class BlOrderEntryToCancelDto extends OrderEntryToCancelDto {

    private double amount;
    private boolean tax;
    private boolean waiver;
    private Long refundedAmount;

    public BlOrderEntryToCancelDto(final AbstractOrderEntryModel orderEntry, final List<String> reasons, final Long quantityAvailableToCancel,
                                   final String deliveryModeName, final double amount, final boolean tax, final boolean waiver, final Long refundedAmount) {
        super(orderEntry, reasons, quantityAvailableToCancel, deliveryModeName);
        this.amount = amount;
        this.tax = tax;
        this.waiver = waiver;
        this.refundedAmount = refundedAmount;
    }

    public double getAmount() {
        return amount;
    }

    public void setAmount(double amount) {
        this.amount = amount;
    }

    public boolean isTax() {
        return tax;
    }

    public void setTax(boolean tax) {
        this.tax = tax;
    }

    public boolean isWaiver() {
        return waiver;
    }

    public void setWaiver(boolean waiver) {
        this.waiver = waiver;
    }

    public Long getRefundedAmount() {
        return refundedAmount;
    }

    public void setRefundedAmount(Long refundedAmount) {
        this.refundedAmount = refundedAmount;
    }
}
