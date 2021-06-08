package com.bl.facades.customerinterestsfacades.productinterest.impl;

import com.bl.facades.customerinterestsfacades.productinterest.BlProductInterestFacade;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.customerinterestsfacades.data.ProductInterestData;
import de.hybris.platform.customerinterestsfacades.productinterest.impl.DefaultProductInterestFacade;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.stocknotificationservices.model.StockNotificationProcessModel;
import de.hybris.platform.store.services.BaseStoreService;

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

    @Override
    public void saveProductInterest(final ProductInterestData productInterest)
    {
        super.saveProductInterest(productInterest);
        createBusinessProcessForSendingEmail(productInterest.getProduct().getCode());
    }

    protected void createBusinessProcessForSendingEmail(String productCode){
        final LanguageModel language =  getCommonI18NService().getCurrentLanguage();
        final ProductModel product = getProductService().getProductForCode(productCode);
        final CustomerModel customer = (CustomerModel) getUserService().getCurrentUser();
        final BaseSiteModel baseSite = getBaseSiteService().getCurrentBaseSite();


        final StockNotificationProcessModel stockNotificationProcessModel =  getBusinessProcessService()
                .createProcess("productOutOfStockNotificationEmailProcess-" + customer.getUid() + "-" + System.currentTimeMillis()
                                + "-" + Thread.currentThread().getId(),
                        "productOutOfStockNotificationEmailProcess");
        stockNotificationProcessModel.setLanguage(language);
        stockNotificationProcessModel.setProduct(product);
        stockNotificationProcessModel.setCustomer(customer);
        stockNotificationProcessModel.setBaseSite(baseSite);
        getModelService().save(stockNotificationProcessModel);
        getBusinessProcessService().startProcess(stockNotificationProcessModel);
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
}
