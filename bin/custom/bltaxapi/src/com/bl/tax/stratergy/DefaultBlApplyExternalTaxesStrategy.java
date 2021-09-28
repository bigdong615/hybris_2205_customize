package com.bl.tax.stratergy;

import de.hybris.platform.core.CoreAlgorithms;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import de.hybris.platform.externaltax.impl.DefaultApplyExternalTaxesStrategy;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.TaxValue;
import java.math.BigDecimal;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * This class is created for calculating the external taxes
 * @author Manikandan
 */
public class DefaultBlApplyExternalTaxesStrategy extends DefaultApplyExternalTaxesStrategy {

  private ModelService modelService;

  /**
   * this method oevrrides to calculate line item and shipping taxes
   */
  @Override
  public void applyExternalTaxes(final AbstractOrderModel order, final ExternalTaxDocument externalTaxes) {
    if (!Boolean.TRUE.equals(order.getNet())) {
      throw new IllegalStateException(
          "Order " + order.getCode() + " must be of type NET to apply external taxes to it.");
    } else if(BooleanUtils.isTrue(order.isUnPaidBillPresent())) {
      applyEntryTaxesForPayBill(order, externalTaxes);
    }
    else {
      final BigDecimal entryTaxSum = applyEntryTaxes(order, externalTaxes);
      final BigDecimal shippingTaxSum = super.applyShippingCostTaxes(order, externalTaxes);
      setTotalTax(order, entryTaxSum.add(shippingTaxSum));
    }
  }

  /**
   * It sets the calculated tax on billing charges entry of consignment
   * @param order
   * @param taxDoc
   */
  private void applyEntryTaxesForPayBill(final AbstractOrderModel order,
      final ExternalTaxDocument taxDoc) {
    AtomicInteger i = new AtomicInteger(0);
    order.getConsignments()
        .forEach(consignment -> consignment.getConsignmentEntries()
            .forEach(consignmentEntry -> consignmentEntry
                .getBillingCharges()
                .forEach((serialCode, listOfCharges) -> listOfCharges.forEach(billing -> {
                  if (BooleanUtils.isFalse(billing.isBillPaid())) {
                    final List<TaxValue> taxesForOrderEntry = taxDoc.getTaxesForOrderEntry(i.get());
                    billing.setTaxAmount(BigDecimal.valueOf(taxesForOrderEntry.get(0).getValue()));
                    getModelService().save(billing);
                    i.set(i.incrementAndGet());
                  }
                }))));

  }

  /**
   * this method is created to calculate line item tax
   */
  @Override
  protected BigDecimal applyEntryTaxes(final AbstractOrderModel order, final ExternalTaxDocument taxDoc) {
    BigDecimal totalTax = BigDecimal.ZERO;
    Iterator var6 = order.getEntries().iterator();

    while (var6.hasNext()) {
      final AbstractOrderEntryModel entry = (AbstractOrderEntryModel) var6.next();
      final Integer entryNumber = entry.getEntryNumber();
      if (entryNumber == null) {
        throw new IllegalStateException("Order entry " + order.getCode() + "." + entry
            + " does not have a entry number. Cannot apply external tax to it.");
      }

      final List<TaxValue> taxesForOrderEntry = taxDoc.getTaxesForOrderEntry(entryNumber);
      TaxValue taxForOrderEntry;
      if (taxesForOrderEntry != null) {
        for (Iterator var10 = taxesForOrderEntry.iterator(); var10.hasNext();
            totalTax = totalTax.add(BigDecimal.valueOf(taxForOrderEntry.getAppliedValue()))) {
          taxForOrderEntry = (TaxValue) var10.next();
          this.assertValidTaxValue(order, taxForOrderEntry);
        }
      }

      entry.setTaxValues(taxesForOrderEntry);
    }

      return totalTax;
  }

  /**
   * this method created to set the total tax to order
   */
  protected void setTotalTax(final AbstractOrderModel order, final BigDecimal totalTaxSum) {
    final Integer digits = order.getCurrency().getDigits();
    if (digits == null) {
      throw new IllegalStateException("Order " + order.getCode() + " has got a currency without decimal digits defined. Cannot apply external taxes.");
    } else {
      if(totalTaxSum.doubleValue() == order.getTotalTax()) {
        order.setTotalTax(CoreAlgorithms.round(totalTaxSum.doubleValue(), digits));
      }
    }
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }
}

