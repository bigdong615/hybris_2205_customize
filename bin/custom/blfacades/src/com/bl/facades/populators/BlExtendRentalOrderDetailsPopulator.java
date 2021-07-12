package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.order.impl.DefaultBlCalculationService;
import com.bl.core.price.service.BlCommercePriceService;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import java.math.BigDecimal;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;

public class BlExtendRentalOrderDetailsPopulator <SOURCE extends OrderModel, TARGET extends OrderData> implements
    Populator<SOURCE, TARGET> {

  private PriceDataFactory priceDataFactory;
  private BlCommercePriceService commercePriceService;
  private ModelService modelService;
  private SessionService sessionService;
  private DefaultBlCalculationService defaultBlCalculationService;

  @Override
  public void populate(final OrderModel source, final OrderData target) throws ConversionException {

    int defaultAddedTimeForExtendRental = 1; // Default value
    target.setAddedTimeForExtendRental(defaultAddedTimeForExtendRental); // Default value which added for extend order
     PriceDataType priceType = PriceDataType.BUY;
     BigDecimal subTotal = BigDecimal.valueOf(0.0);

     for(AbstractOrderEntryModel entries : source.getEntries()) {
       checkForExistinDamageWaiverSelected(entries);
       updateOrderEntryDamageWaiver(entries.getEntryNumber() , source);
       try {
         getDefaultBlCalculationService().recalculateForExtendOrder(source , defaultAddedTimeForExtendRental);
         subTotal = subTotal.add(BigDecimal.valueOf(entries.getTotalPrice()).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));

       } catch (CalculationException e) {
         e.printStackTrace();
       }

     }
     target.setTotalCostForExtendRental(getPriceDataFactory().create(priceType, subTotal , source.getCurrency().getIsocode()));
     target.setTotalDamageWaiverCostForExtendRental(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(0.0) ,
         source.getCurrency().getIsocode()));
     target.setTotalTaxForExtendRental(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(0.0) ,
         source.getCurrency().getIsocode()));
     final BigDecimal orderTotalWithTax = subTotal.add(target.getTotalDamageWaiverCostForExtendRental().getValue()).add(target.getTotalTaxForExtendRental().getValue());
     target.setOrderTotalWithTaxForExtendRental(getPriceDataFactory().create(priceType ,orderTotalWithTax , source.getCurrency().getIsocode()));
  }

  private void checkForExistinDamageWaiverSelected(final AbstractOrderEntryModel entries) {
    if(BooleanUtils.isTrue(entries.getGearGuardProFullWaiverSelected())) {
      setFlags(entries, Boolean.TRUE, Boolean.FALSE, Boolean.FALSE);
    }
    else if(BooleanUtils.isTrue(entries.getGearGuardWaiverSelected())) {
      setFlags(entries, Boolean.FALSE, Boolean.TRUE, Boolean.FALSE);
    }
    else if(BooleanUtils.isTrue(entries.getNoDamageWaiverSelected())) {
      setFlags(entries, Boolean.FALSE, Boolean.FALSE, Boolean.TRUE);
    }
  }

  private void updateOrderEntryDamageWaiver(final long entryNumber, OrderModel orderModel) {
    if (CollectionUtils.isNotEmpty(orderModel.getEntries())) {
      final AbstractOrderEntryModel abstractOrderEntryModel = orderModel.getEntries().stream()
          .filter(cartEntry -> Integer.valueOf((int) entryNumber).equals(cartEntry.getEntryNumber())).findFirst().orElse(null);
      orderModel.setCalculated(Boolean.FALSE);
      getModelService().save(abstractOrderEntryModel);
      getModelService().save(orderModel);
    }
  }


  private void setFlags(final AbstractOrderEntryModel cartEntryModel, final Boolean gearGuardProFullWaiverSelected,
      final Boolean gearGuardWaiverSelected, final Boolean noGearGuardWaiverSelected) {
    cartEntryModel.setGearGuardProFullWaiverSelected(gearGuardProFullWaiverSelected);
    cartEntryModel.setGearGuardWaiverSelected(gearGuardWaiverSelected);
    cartEntryModel.setNoDamageWaiverSelected(noGearGuardWaiverSelected);
    cartEntryModel.setCalculated(Boolean.FALSE);
  }

  public PriceDataFactory getPriceDataFactory() {
    return priceDataFactory;
  }

  public void setPriceDataFactory(
      PriceDataFactory priceDataFactory) {
    this.priceDataFactory = priceDataFactory;
  }


  public BlCommercePriceService getCommercePriceService() {
    return commercePriceService;
  }

  public void setCommercePriceService(
      BlCommercePriceService commercePriceService) {
    this.commercePriceService = commercePriceService;
  }

  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(SessionService sessionService) {
    this.sessionService = sessionService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }



  public DefaultBlCalculationService getDefaultBlCalculationService() {
    return defaultBlCalculationService;
  }

  public void setDefaultBlCalculationService(
      DefaultBlCalculationService defaultBlCalculationService) {
    this.defaultBlCalculationService = defaultBlCalculationService;
  }

}
