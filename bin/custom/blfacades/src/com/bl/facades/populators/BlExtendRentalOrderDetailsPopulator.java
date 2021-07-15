package com.bl.facades.populators;

import com.bl.core.order.impl.DefaultBlCalculationService;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.services.extendorder.impl.DefaultBlExtendOrderService;
import com.bl.core.utils.BlExtendOrderUtils;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
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
import org.springframework.beans.factory.annotation.Autowired;

public class BlExtendRentalOrderDetailsPopulator <SOURCE extends OrderModel, TARGET extends OrderData> implements
    Populator<SOURCE, TARGET> {

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



  @Override
  public void populate(final OrderModel source, final OrderData target) throws ConversionException {

    /*long defaultAddedTimeForExtendRental = 1; // Default value
    target.setAddedTimeForExtendRental((int) defaultAddedTimeForExtendRental); // Default value which added for extend order
     PriceDataType priceType = PriceDataType.BUY;
     OrderModel extendOrderModel =  getDefaultBlExtendOrderService().cloneOrderModelForExtendRental(source);
     for(AbstractOrderEntryModel entries : extendOrderModel.getEntries()) {
       try {
         getDefaultBlCalculationService().recalculateForExtendOrder(extendOrderModel , (int) defaultAddedTimeForExtendRental);
         if(null != extendOrderModel.getAllPromotionResults()) {
           getPromotionsService().updatePromotions(getPromotionGroups(), extendOrderModel, true,
               AutoApplyMode.APPLY_ALL,
               AutoApplyMode.APPLY_ALL, getTimeService().getCurrentTime());
         }
       } catch (CalculationException e) {
         e.printStackTrace();
       }
     }
     target.setSubTotalTaxForExtendRental(getPriceDataFactory().create(priceType, BigDecimal.valueOf(extendOrderModel.getSubtotal()) ,
         extendOrderModel.getCurrency().getIsocode()));
     target.setTotalDamageWaiverCostForExtendRental(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(extendOrderModel.getTotalDamageWaiverCost()) ,
         extendOrderModel.getCurrency().getIsocode()));
     target.setTotalTaxForExtendRental(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(extendOrderModel.getTotalTax()),
         extendOrderModel.getCurrency().getIsocode()));

     target.setTotalDiscounts(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(extendOrderModel.getTotalDiscounts()),
         extendOrderModel.getCurrency().getIsocode()));

     target.setOrderTotalWithTaxForExtendRental(getPriceDataFactory().create(priceType ,BigDecimal.valueOf(extendOrderModel.getTotalPrice()) ,
         extendOrderModel.getCurrency().getIsocode()));

    // To set current extendOrderModel to session
    BlExtendOrderUtils.setCurrentExtendOrderToSession(extendOrderModel);*/
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
    final Collection<PromotionGroupModel> promotionGroupModels = new ArrayList<PromotionGroupModel>();
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
