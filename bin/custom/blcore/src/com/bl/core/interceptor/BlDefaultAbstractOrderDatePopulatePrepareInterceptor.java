package com.bl.core.interceptor;

import com.bl.core.service.BlBackOfficePriceService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Date;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class BlDefaultAbstractOrderDatePopulatePrepareInterceptor implements
    PrepareInterceptor<AbstractOrderModel> {

  private static final Logger LOG = Logger.getLogger(BlDefaultAbstractOrderDatePopulatePrepareInterceptor.class);

  private BlBackOfficePriceService blBackOfficePriceService;

  @Override
  public void onPrepare(final AbstractOrderModel abstractOrderModel,
      final InterceptorContext interceptorContext) throws InterceptorException {
    final Date rentalStartDate = abstractOrderModel.getRentalStartDate();
    final Date rentalReturnDate = abstractOrderModel.getRentalEndDate();
    if (CollectionUtils.isNotEmpty(abstractOrderModel.getEntries())) {
      for (final AbstractOrderEntryModel orderEntry : abstractOrderModel.getEntries()) {
        //update rental date based on order dates
        orderEntry.setRentalStartDate(rentalStartDate);
        orderEntry.setRentalReturnDate(rentalReturnDate);
        // calculating base price after updating effective dates
        try {
          final BigDecimal calculatedBasePrice = getBlBackOfficePriceService()
              .getProductPrice(orderEntry.getProduct(), orderEntry.getRentalStartDate(),
                  orderEntry.getRentalReturnDate());
          if (calculatedBasePrice != null) {
            orderEntry.setBasePrice(calculatedBasePrice.doubleValue());
          }
        } catch (final ParseException e) {
          BlLogger.logMessage(LOG, Level.ERROR, e.getMessage(), e);
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
