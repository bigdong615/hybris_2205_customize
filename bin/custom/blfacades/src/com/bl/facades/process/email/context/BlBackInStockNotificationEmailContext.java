package com.bl.facades.process.email.context;

import de.hybris.platform.acceleratorservices.model.cms2.pages.EmailPageModel;
import de.hybris.platform.stocknotificationfacades.process.email.context.BackInStockNotificationEmailContext;
import de.hybris.platform.stocknotificationservices.model.StockNotificationProcessModel;

public class BlBackInStockNotificationEmailContext extends BackInStockNotificationEmailContext {

    private String productPageURL;
    private static final String RENTAL_PDP_URL_PREFIX = "/rent/product/";

    @Override
    public void init(final StockNotificationProcessModel businessProcessModel, final EmailPageModel emailPageModel)
    {
        super.init(businessProcessModel, emailPageModel);
        this.setProductPageURL(getBaseSiteurlData());
    }

    /**
     * This method is responsible for providing rental product url.
     * @return
     */
    private String getBaseSiteurlData(){
        return getSiteBaseUrlResolutionService().getWebsiteUrlForSite(getBaseSite(),getUrlEncodingAttributes(), false, RENTAL_PDP_URL_PREFIX+getProductData().getCode());
    }
    public String getProductPageURL() {
        return productPageURL;
    }

    public void setProductPageURL(String productPageURL) {
        this.productPageURL = productPageURL;
    }



}
