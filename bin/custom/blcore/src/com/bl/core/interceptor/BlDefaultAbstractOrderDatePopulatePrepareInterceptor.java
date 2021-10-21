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
import java.util.List;
import java.util.stream.Collectors;
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

    if (BooleanUtils.isFalse(abstractOrderModel.getInternalTransferOrder()) && CollectionUtils
        .isNotEmpty(abstractOrderModel.getEntries()) && rentalStartDate != null
        && rentalReturnDate != null && BooleanUtils.isFalse(abstractOrderModel.isGiftCardOrder())
        && BooleanUtils.isTrue(abstractOrderModel.getIsRentalCart())) {
      final List<AbstractOrderEntryModel> entryModelList = abstractOrderModel.getEntries().stream().filter(entry ->!entry.isBundleEntry()).collect(
          Collectors.toList());
      for (final AbstractOrderEntryModel orderEntry : entryModelList) {
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
    } else if (BooleanUtils.isTrue(abstractOrderModel.getInternalTransferOrder())) {
      setBasePriceToZero(abstractOrderModel);
    }
  }

  /**
   * To set zero to base price of order entries in case of null rental start date and end date,
   * exa:- internal transfer order
   *
   * @param abstractOrderModel
   */
  private void setBasePriceToZero(final AbstractOrderModel abstractOrderModel) {

    if (CollectionUtils.isNotEmpty(abstractOrderModel.getEntries())) {
      for (final AbstractOrderEntryModel orderEntry : abstractOrderModel.getEntries()) {
        orderEntry.setBasePrice(0.0d);
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
