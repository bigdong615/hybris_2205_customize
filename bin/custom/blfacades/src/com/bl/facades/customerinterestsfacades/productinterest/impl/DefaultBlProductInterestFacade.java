package com.bl.facades.customerinterestsfacades.productinterest.impl;

import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.utils.BlURLServicesUtils;
import com.bl.esp.dto.notify.data.NotifyMeEmailRequestData;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.customerinterestsfacades.productinterest.BlProductInterestFacade;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ImageDataType;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.customerinterestsfacades.data.ProductInterestData;
import de.hybris.platform.customerinterestsfacades.productinterest.impl.DefaultProductInterestFacade;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.drools.core.util.StringUtils;

/**
 * @auther vijay vishwakarma
 * This class is created to start business process for sending notification confirmation email.
 * */
public class DefaultBlProductInterestFacade extends DefaultProductInterestFacade implements BlProductInterestFacade {

    private ModelService modelService;
    private UserService userService;
    private BaseStoreService baseStoreService;
    private CommonI18NService commonI18NService;
    private BusinessProcessService businessProcessService;
    private BlURLServicesUtils  blURLServicesUtils;
  private BlESPEventService blESPEventService;
  @Resource(name = "productFacade")
  private ProductFacade productFacade;

    @Override
    public void saveProductInterest(final ProductInterestData productInterest)
    {
        super.saveProductInterest(productInterest);
        final ProductData productData = productFacade.getProductForCodeAndOptions(productInterest.getProduct().getCode(),
          Arrays.asList(ProductOption.BASIC, ProductOption.IMAGES));
       createESPEVENTForSendingEmailRequest(productData);
    }
   /**
    * This method is responsible for create and send ESP event request for product notification email.
    */
  protected void createESPEVENTForSendingEmailRequest(final ProductData productData){
    final CustomerModel customer = (CustomerModel) getUserService().getCurrentUser();
    final List<ImageData> thumbnail = productData.getImages().stream()
        .filter(imageData -> {
          return (imageData.getImageType().equals( ImageDataType.PRIMARY)
              && imageData.getFormat().toLowerCase().equals(BlFacadesConstants.THUMBNAIL_STRING));
        }).collect(Collectors.toList());
    final NotifyMeEmailRequestData emailRequestData = new NotifyMeEmailRequestData();
    emailRequestData.setEmailAddress(customer.getUid());
    emailRequestData.setProductName(productData.getName());
    emailRequestData.setProductUrl(getBlURLServicesUtils().getRequestedURl(BlFacadesConstants.RENTAL_PDP_URL_PREFIX +productData.getCode()));
    if (CollectionUtils.isNotEmpty(thumbnail)){
      emailRequestData.setProductThumbURL(thumbnail.get(0).getUrl());
    }else{emailRequestData.setProductThumbURL(StringUtils.EMPTY);}
    getBlESPEventService().sendNotifyMeConfirmEmailRequest(emailRequestData);
  }


    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

    public BusinessProcessService getBusinessProcessService() {
        return businessProcessService;
    }

    public void setBusinessProcessService(BusinessProcessService businessProcessService) {
        this.businessProcessService = businessProcessService;
    }

    @Override
    public UserService getUserService() {
        return userService;
    }

    @Override
    public void setUserService(UserService userService) {
        this.userService = userService;
    }

    @Override
    public BaseStoreService getBaseStoreService() {
        return baseStoreService;
    }

    @Override
    public void setBaseStoreService(BaseStoreService baseStoreService) {
        this.baseStoreService = baseStoreService;
    }

    public CommonI18NService getCommonI18NService() {
        return commonI18NService;
    }

    public void setCommonI18NService(CommonI18NService commonI18NService) {
        this.commonI18NService = commonI18NService;
    }
  public BlURLServicesUtils getBlURLServicesUtils() {
    return blURLServicesUtils;
  }

  public void setBlURLServicesUtils(BlURLServicesUtils blURLServicesUtils) {
    this.blURLServicesUtils = blURLServicesUtils;
  }
  public BlESPEventService getBlESPEventService() {
    return blESPEventService;
  }

  public void setBlESPEventService(BlESPEventService blESPEventService) {
    this.blESPEventService = blESPEventService;
  }
}
