package com.bl.core.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.service.BlBackOfficePriceService;
import com.bl.logging.BlLogger;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Date;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Prepare interceptor for AbstractOrder item type.
 *
 * @author Kalyan Kumar
 */
public class BlDefaultAbstractOrderDatePopulatePrepareInterceptor implements
    PrepareInterceptor<AbstractOrderModel> {

  private static final Logger LOG = Logger.getLogger(BlDefaultAbstractOrderDatePopulatePrepareInterceptor.class);

  private BlBackOfficePriceService blBackOfficePriceService;

  @Resource
  private CatalogVersionService catalogVersionService;

  @Override
  public void onPrepare(final AbstractOrderModel abstractOrderModel,
      final InterceptorContext interceptorContext) throws InterceptorException {
    if (CollectionUtils.isEmpty(catalogVersionService.getSessionCatalogVersions())) {
      catalogVersionService.setSessionCatalogVersion(BlCoreConstants.CATALOG_VALUE,
          BlCoreConstants.CATALOG_VERSION_NAME);
    }

     Date rentalStartDate = null;
     Date rentalReturnDate = null;

    if(BooleanUtils.isTrue(abstractOrderModel.getIsExtendedOrder())) {
      rentalStartDate = abstractOrderModel.getExtendRentalStartDate();
      rentalReturnDate = abstractOrderModel.getExtendRentalEndDate();
    }
    else {
      rentalStartDate = abstractOrderModel.getRentalStartDate();
      rentalReturnDate = abstractOrderModel.getRentalEndDate();
    }

    if (CollectionUtils.isNotEmpty(abstractOrderModel.getEntries()) && rentalStartDate != null
        && rentalReturnDate != null) {
      for (final AbstractOrderEntryModel orderEntry : abstractOrderModel.getEntries()) {
        //update rental date based on order dates
        // calculating base price after updating effective dates
        try {
          final BigDecimal calculatedBasePrice = getBlBackOfficePriceService()
              .getProductPrice(orderEntry.getProduct(), rentalStartDate,
                  rentalReturnDate , BooleanUtils.isTrue(abstractOrderModel.getIsExtendedOrder()));
          if (calculatedBasePrice != null) {
            orderEntry.setBasePrice(calculatedBasePrice.doubleValue());
          }
        } catch (final ParseException e) {
          BlLogger.logMessage(LOG, Level.ERROR, "Error while parsing the product price : ", e);
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
