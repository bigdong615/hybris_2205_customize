package com.bl.tax.stratergy;

import de.hybris.platform.core.CoreAlgorithms;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.externaltax.ExternalTaxDocument;
import de.hybris.platform.externaltax.impl.DefaultApplyExternalTaxesStrategy;
import de.hybris.platform.util.TaxValue;
import java.math.BigDecimal;
import java.util.Iterator;
import java.util.List;

public class DefaultBlApplyExternalTaxesStrategy extends DefaultApplyExternalTaxesStrategy {

  @Override
  public void applyExternalTaxes(AbstractOrderModel order, ExternalTaxDocument externalTaxes) {
    if (!Boolean.TRUE.equals(order.getNet())) {
      throw new IllegalStateException(
          "Order " + order.getCode() + " must be of type NET to apply external taxes to it.");
    } else {
      BigDecimal entryTaxSum = applyEntryTaxes(order, externalTaxes);
      BigDecimal shippingTaxSum = super.applyShippingCostTaxes(order, externalTaxes);
      setTotalTax(order, entryTaxSum.add(shippingTaxSum));
    }
  }

  @Override
  protected BigDecimal applyEntryTaxes(AbstractOrderModel order, ExternalTaxDocument taxDoc) {
    BigDecimal totalTax = BigDecimal.ZERO;
    Iterator var6 = order.getEntries().iterator();

    while (var6.hasNext()) {
      AbstractOrderEntryModel entry = (AbstractOrderEntryModel) var6.next();
      Integer entryNumber = entry.getEntryNumber();
      if (entryNumber == null) {
        throw new IllegalStateException("Order entry " + order.getCode() + "." + entry
            + " does not have a entry number. Cannot apply external tax to it.");
      }

      List<TaxValue> taxesForOrderEntry = taxDoc.getTaxesForOrderEntry(entryNumber);
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

  protected void setTotalTax(AbstractOrderModel order, BigDecimal totalTaxSum) {
    Integer digits = order.getCurrency().getDigits();
    if (digits == null) {
      throw new IllegalStateException("Order " + order.getCode() + " has got a currency without decimal digits defined. Cannot apply external taxes.");
    } else {
      if(totalTaxSum.doubleValue() == order.getTotalTax()) {
        order.setTotalTax(CoreAlgorithms.round(totalTaxSum.doubleValue(), digits));
      }
    }
  }
}

