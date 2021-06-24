package com.bl.facades.process.email.context;

import de.hybris.platform.acceleratorservices.model.cms2.pages.EmailPageModel;
import de.hybris.platform.acceleratorservices.process.email.context.AbstractEmailContext;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.stocknotificationservices.model.StockNotificationProcessModel;
import org.apache.commons.lang.StringUtils;


import java.util.Locale;

/**
 * @author vijay vishwakarma
 * This class is created for providing dynamic data for notify me email.
 */
public class ProductNotifyMeEmailContext extends AbstractEmailContext<StockNotificationProcessModel> {

    private ProductData productData;

    private Converter<ProductModel, ProductData> productConverter;
    private Locale emailLocale;
    private String productPageURL;
    private static final String RENTAL_PDP_URL_PREFIX = "/rent/product/";
    private static final String ADDRESS_BOOK_URL = "/my-account/address-book";
    private String addressUrl;

    @Override
    public void init(final StockNotificationProcessModel businessProcessModel, final EmailPageModel emailPageModel)
    {
        super.init(businessProcessModel, emailPageModel);
        productData = getProductConverter().convert(businessProcessModel.getProduct());
        this.setProductPageURL(getBaseSiteurlData());
        addressUrl = getAddressBookUrl();
        setEmailLocale(businessProcessModel);
        updateBaseUrl(businessProcessModel, emailLocale);
        updateTitle(businessProcessModel, emailLocale);
        updateProductName(businessProcessModel, emailLocale);
    }

    protected void setEmailLocale(final StockNotificationProcessModel businessProcessModel)
    {
        final String isoCode = getEmailLanguage(businessProcessModel).getIsocode();
        emailLocale = new Locale(isoCode);
    }

    /**
     * This method is responsible for base site url.
     * @param businessProcessModel
     * @param emailLocale
     */
    protected void updateBaseUrl(final StockNotificationProcessModel businessProcessModel, final Locale emailLocale)
    {
        final String baseUrl = (String) get(BASE_URL);
        final String baseSecrueUrl = (String) get(SECURE_BASE_URL);
        final String defaultIsoCode = businessProcessModel.getBaseSite().getDefaultLanguage().getIsocode();
        final String siteIsoCode = emailLocale.getLanguage();
        put(BASE_URL, baseUrl.replaceAll("/" + defaultIsoCode + "$", "/" + siteIsoCode));
        put(SECURE_BASE_URL, baseSecrueUrl.replaceAll("/" + defaultIsoCode + "$", "/" + siteIsoCode));
    }

    /**
     * This method is responsible for providing title
     * @param businessProcessModel
     * @param emailLocale
     */
    protected void updateTitle(final StockNotificationProcessModel businessProcessModel, final Locale emailLocale)
    {
        String title= StringUtils.EMPTY;
        if(businessProcessModel.getCustomer()!=null && businessProcessModel.getCustomer().getTitle()!=null) {
            title = businessProcessModel.getCustomer().getTitle().getName(emailLocale);
        }
        put(TITLE, title);
    }

    /**
     * This method is responsible for updating product name as per current locale.
     * @param businessProcessModel
     * @param emailLocale
     */
    protected void updateProductName(final StockNotificationProcessModel businessProcessModel, final Locale emailLocale)
    {
        final String productName = businessProcessModel.getProduct().getName(emailLocale);
        productData.setName(productName);
    }

    @Override
    protected BaseSiteModel getSite(final StockNotificationProcessModel stockNotificationProcessModel)
    {
        return stockNotificationProcessModel.getBaseSite();
    }

    @Override
    protected CustomerModel getCustomer(final StockNotificationProcessModel stockNotificationProcessModel)
    {
        return stockNotificationProcessModel.getCustomer();
    }

    @Override
    protected LanguageModel getEmailLanguage(final StockNotificationProcessModel stockNotificationProcessModel)
    {
        return stockNotificationProcessModel.getLanguage();
    }

    public Converter<ProductModel, ProductData> getProductConverter() {
        return productConverter;
    }

    public void setProductConverter(Converter<ProductModel, ProductData> productConverter) {
        this.productConverter = productConverter;
    }
    public ProductData getProductData() {
        return productData;
    }

    /**
     * This method is responsible for providing rental product url.
     * @return
     */
    private String getBaseSiteurlData(){
       return getSiteBaseUrlResolutionService().getWebsiteUrlForSite(getBaseSite(),getUrlEncodingAttributes(), false, RENTAL_PDP_URL_PREFIX+productData.getCode());
    }

    private String getAddressBookUrl(){
        return getSiteBaseUrlResolutionService().getWebsiteUrlForSite(getBaseSite(),getUrlEncodingAttributes(), false, ADDRESS_BOOK_URL);
    }

    public String getProductPageURL() {
        return productPageURL;
    }

    public void setProductPageURL(String productPageURL) {
        this.productPageURL = productPageURL;
    }
    public String getAddressUrl() {
        return addressUrl;
    }
    public void setAddressUrl(String addressUrl) {
        this.addressUrl = addressUrl;
    }

}

