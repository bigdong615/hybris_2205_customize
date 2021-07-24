package com.bl.facades.populators;

import com.bl.core.order.impl.DefaultBlCalculationService;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.services.extendorder.impl.DefaultBlExtendOrderService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.promotions.PromotionsService;
import de.hybris.platform.promotions.jalo.PromotionsManager.AutoApplyMode;
import de.hybris.platform.promotions.model.PromotionGroupModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.time.TimeService;
import de.hybris.platform.site.BaseSiteService;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This populatar created to populate custom attributes for extend rental page
 * @author Manikandan
 */
public class BlExtendRentalOrderDetailsPopulator <SOURCE extends AbstractOrderModel, TARGET extends OrderData> implements
    Populator<SOURCE, TARGET> {

  private static final Logger LOG = Logger.getLogger(BlExtendRentalOrderDetailsPopulator.class);

  private PriceDataFactory priceDataFactory;
  private BlCommercePriceService commercePriceService;
  private ModelService modelService;
  private SessionService sessionService;
  private DefaultBlCalculationService defaultBlCalculationService;
  private ProductService productService;
  private DefaultBlExtendOrderService defaultBlExtendOrderService;
  private PromotionsService promotionsService;
  private  BaseSiteService baseSiteService;
  private  TimeService timeService;


  /**
   * This method created to populate custom attribute to order data
   */

  @Override
  public void populate(final AbstractOrderModel orderModel, final OrderData target) throws ConversionException {

     PriceDataType priceType = PriceDataType.BUY;

       try {
         getDefaultBlCalculationService().recalculateForExtendOrder(orderModel , orderModel.getTotalExtendDays());
         if(null != orderModel.getAllPromotionResults()) {
           getPromotionsService().updatePromotions(getPromotionGroups(), orderModel, true,
               AutoApplyMode.APPLY_ALL, AutoApplyMode.APPLY_ALL, getTimeService().getCurrentTime());
         }
       } catch (CalculationException e) {
         BlLogger.logMessage(LOG , Level.ERROR , "Error while calculating" + orderModel.getCode() , e);
       }

       target.setCode(orderModel.getCode());
       target.setAddedTimeForExtendRental(orderModel.getTotalExtendDays());
     target.setSubTotalTaxForExtendRental(getPriceDataFactory().create(priceType, BigDecimal.valueOf(orderModel.getSubtotal()) ,
         orderModel.getCurrency().getIsocode()));
     target.setTotalDamageWaiverCostForExtendRental(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(orderModel.getTotalDamageWaiverCost()) ,
         orderModel.getCurrency().getIsocode()));
     target.setTotalTaxForExtendRental(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(orderModel.getTotalTax()),
         orderModel.getCurrency().getIsocode()));

     target.setTotalDiscounts(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(orderModel.getTotalDiscounts()),
         orderModel.getCurrency().getIsocode()));

     target.setOrderTotalWithTaxForExtendRental(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(orderModel.getTotalPrice()) ,
         orderModel.getCurrency().getIsocode()));

     //To set customer Mail
    target.setCustomerMail(orderModel.getUser().getUid());
    target.setExtendOrderConfirmationDate(convertDateToString(orderModel.getRentalEndDate() ,
        BlFacadesConstants.EXTEND_ORDER_FORMAT_PATTERN));

  }

  /**
   * This Method converts rental startDate and rental endDate to String
   */
  private String convertDateToString(final Date rentalDate , final String dateFormat) {
    return BlDateTimeUtils.convertDateToStringDate(rentalDate,dateFormat);
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


  public ProductService getProductService() {
    return productService;
  }

  public void setProductService(ProductService productService) {
    this.productService = productService;
  }


  public DefaultBlExtendOrderService getDefaultBlExtendOrderService() {
    return defaultBlExtendOrderService;
  }

  public void setDefaultBlExtendOrderService(
      DefaultBlExtendOrderService defaultBlExtendOrderService) {
    this.defaultBlExtendOrderService = defaultBlExtendOrderService;
  }

  protected Collection<PromotionGroupModel> getPromotionGroups()
  {
    final Collection<PromotionGroupModel> promotionGroupModels = new ArrayList<>();
    if (getBaseSiteService().getCurrentBaseSite() != null
        && getBaseSiteService().getCurrentBaseSite().getDefaultPromotionGroup() != null)
    {
      promotionGroupModels.add(getBaseSiteService().getCurrentBaseSite().getDefaultPromotionGroup());
    }
    return promotionGroupModels;
  }


  public PromotionsService getPromotionsService() {
    return promotionsService;
  }

  public void setPromotionsService(PromotionsService promotionsService) {
    this.promotionsService = promotionsService;
  }

  public BaseSiteService getBaseSiteService() {
    return baseSiteService;
  }

  public void setBaseSiteService(BaseSiteService baseSiteService) {
    this.baseSiteService = baseSiteService;
  }

  public TimeService getTimeService() {
    return timeService;
  }

  public void setTimeService(TimeService timeService) {
    this.timeService = timeService;
  }

}
