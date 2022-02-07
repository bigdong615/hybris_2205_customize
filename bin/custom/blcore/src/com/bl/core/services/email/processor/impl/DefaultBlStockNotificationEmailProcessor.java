package com.bl.core.services.email.processor.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.esp.service.BlESPEventService;
import com.bl.esp.dto.notify.data.NotifyMeEmailRequestData;
import de.hybris.platform.acceleratorservices.urlresolver.SiteBaseUrlResolutionService;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.converters.populator.ProductPrimaryImagePopulator;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ImageDataType;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.stocknotificationservices.constants.StocknotificationservicesConstants;
import de.hybris.platform.stocknotificationservices.email.processor.impl.DefaultStockNotificationEmailProcessor;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

public class DefaultBlStockNotificationEmailProcessor extends
    DefaultStockNotificationEmailProcessor {

  private BlESPEventService blESPEventService;
  private SiteBaseUrlResolutionService siteBaseUrlResolutionService;
  private BaseSiteService baseSiteService;
  private ProductFacade productFacade;

  @Resource(name = "productConverter")
  private Converter<ProductModel, ProductData> productConverter;

  @Resource(name = "productPrimaryImagePopulator")
  ProductPrimaryImagePopulator populator;

  @Override
  public void process(final CustomerModel customer, final Map<String, ? extends ItemModel> dataMap)
  {
    final ProductModel product = (ProductModel) dataMap.get(StocknotificationservicesConstants.PRODUCT);
    final ProductData productData = new ProductData();
    populator.populate(product,productData);
    final List<ImageData> thumbnail = productData.getImages().stream()
        .filter(imageData ->
            (imageData.getImageType().equals( ImageDataType.PRIMARY)
                && imageData.getFormat().equalsIgnoreCase(BlCoreConstants.THUMBNAIL))
        ).collect(Collectors.toList());
    final NotifyMeEmailRequestData emailRequestData = new NotifyMeEmailRequestData();
    emailRequestData.setEmailAddress(customer.getUid());
    emailRequestData.setProductName(product.getName());
    emailRequestData.setProductUrl(getRequestedURL(BlCoreConstants.RENTAL_PDP_URL_PREFIX +product.getCode()));
    if (CollectionUtils.isEmpty(thumbnail)){
      emailRequestData.setProductThumbURL(StringUtils.EMPTY);
    }else{emailRequestData.setProductThumbURL(thumbnail.get(0).getUrl());}
    getBlESPEventService().sendBackInStockEmailRequest(emailRequestData);
  }

  public String getRequestedURL(final String urlString){
    return getSiteBaseUrlResolutionService()
        .getWebsiteUrlForSite(getBaseSiteService().getBaseSiteForUID(BlCoreConstants.BASE_STORE_ID),
            org.apache.commons.lang.StringUtils.EMPTY, Boolean.TRUE, urlString);
  }

  public BlESPEventService getBlESPEventService() {
    return blESPEventService;
  }

  public void setBlESPEventService(BlESPEventService blESPEventService) {
    this.blESPEventService = blESPEventService;
  }

  public SiteBaseUrlResolutionService getSiteBaseUrlResolutionService() {
    return siteBaseUrlResolutionService;
  }

  public void setSiteBaseUrlResolutionService(
      SiteBaseUrlResolutionService siteBaseUrlResolutionService) {
    this.siteBaseUrlResolutionService = siteBaseUrlResolutionService;
  }

  public BaseSiteService getBaseSiteService() {
    return baseSiteService;
  }

  public void setBaseSiteService(BaseSiteService baseSiteService) {
    this.baseSiteService = baseSiteService;
  }
  public ProductFacade getProductFacade() {
    return productFacade;
  }

  public void setProductFacade(ProductFacade productFacade) {
    this.productFacade = productFacade;
  }

}
