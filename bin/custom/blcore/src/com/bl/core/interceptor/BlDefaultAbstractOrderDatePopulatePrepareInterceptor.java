package com.bl.core.interceptor;

import com.bl.core.service.BlBackOfficePriceService;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Date;
import org.apache.commons.collections.CollectionUtils;

public class BlDefaultAbstractOrderDatePopulatePrepareInterceptor implements
    PrepareInterceptor<AbstractOrderModel> {

  private BlBackOfficePriceService blBackOfficePriceService;

  @Override
  public void onPrepare(AbstractOrderModel abstractOrderModel,
      InterceptorContext interceptorContext) throws InterceptorException {
    Date rentalStartDate = abstractOrderModel.getRentalStartDate();
    Date rentalReturnDate = abstractOrderModel.getRentalReturnDate();
    if (CollectionUtils.isNotEmpty(abstractOrderModel.getEntries())) {
      for (AbstractOrderEntryModel orderEntry : abstractOrderModel.getEntries()) {
        //update rental date based on order dates
        orderEntry.setRentalStartDate(rentalStartDate);
        orderEntry.setRentalReturnDate(rentalReturnDate);

        // calculating base price after updating effective dates
        try {
          BigDecimal calculatedBasePrice = getBlBackOfficePriceService()
              .getProductPrice(orderEntry.getProduct(), orderEntry.getRentalStartDate(),
                  orderEntry.getRentalReturnDate());
          orderEntry.setBasePrice(calculatedBasePrice.doubleValue());
        } catch (ParseException e) {
          e.printStackTrace();
        }
      }
    }
  }

  public BlBackOfficePriceService getBlBackOfficePriceService() {
    return blBackOfficePriceService;
  }

  public void setBlBackOfficePriceService(BlBackOfficePriceService blBackOfficePriceService) {
    this.blBackOfficePriceService = blBackOfficePriceService;
  }
}
