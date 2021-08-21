package com.bl.tax.service.impl;

import com.bl.tax.TaxResponse;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.math.BigDecimal;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * This class is created for setting avalara tax with order
 * @author Manikandan
 */

public class BlAvalaraTaxCalculationService {

private ModelService modelService;

  /**
   * This method set the order tax exempt bases on condition
   * @param abstractOrderModel the order model
   * @param taxResponse tax response
   */
  public void calculateTaxWithOrderTotal(final AbstractOrderModel abstractOrderModel , final TaxResponse taxResponse)
  {
    if(BooleanUtils.isTrue(abstractOrderModel.getUnPaidBillPresent())) {
     abstractOrderModel.getConsignments()
         .forEach(consignment -> consignment.getConsignmentEntries().forEach(consignmentEntry -> consignmentEntry
             .getBillingCharges().forEach((serialCode, listOfCharges) -> listOfCharges.forEach(billing ->
               taxResponse.getTaxLines().forEach(taxLine -> {
                 if (taxLine.getItemCode().equals(serialCode))
                 {
                  billing.setTaxAmount(BigDecimal.valueOf(setTotalTaxToOrder(taxResponse)));
                   getModelService().save(billing);
                 }
               })
             ))));
    } else {
      abstractOrderModel.setTotalTax(setTotalTaxToOrder(taxResponse));
      abstractOrderModel.setIsOrderTaxExempt(0.0 < taxResponse.getTotalExempt()
          && 0.0 == taxResponse.getTotalTax() ? Boolean.TRUE : Boolean.FALSE);
    }
}

  /**
   * this method set the total tax return from the avalara
   */
  private Double setTotalTaxToOrder(final TaxResponse taxResponse) {
    return taxResponse.getTotalTax() > 0.0 ? taxResponse.getTotalTax() : 0.0;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }
}
