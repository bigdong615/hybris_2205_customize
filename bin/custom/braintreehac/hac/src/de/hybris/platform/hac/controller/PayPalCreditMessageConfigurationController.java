package de.hybris.platform.hac.controller;

import de.hybris.platform.cms2.model.contents.components.SimpleCMSComponentModel;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import braintreehac.services.PayPalCreditMessageConfigurationService;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;


@Controller
@RequestMapping("/braintreehac")
public class PayPalCreditMessageConfigurationController
{
    private static final String REDIRECT_PREFIX = "redirect:";
    private static final String PRODUCT_GRID_CREDIT_MESSAGE = "PayPalProductGridCreditMessageComponent";
    private static final String PRODUCT_GRID_CREDIT_MESSAGE_B2B = "PayPalProductGridCreditMessageComponentB2B";
    private static final String PRODUCT_LIST_CREDIT_MESSAGE = "PayPalProductListCreditMessageComponent";
    private static final String PRODUCT_LIST_CREDIT_MESSAGE_B2B = "PayPalProductListCreditMessageComponentB2B";
    private static final String PRODUCT_DETAILS_PAGE_CREDIT_MESSAGE = "PayPalProductDetailsPageMessageComponent";
    private static final String PRODUCT_DETAILS_PAGE_CREDIT_MESSAGE_B2B = "PayPalProductDetailsPageMessageComponentB2B";
    private static final String CART_PAGE_CREDIT_MESSAGE = "PayPalCartPageCreditMessageComponent";
    private static final String CART_PAGE_CREDIT_MESSAGE_B2B = "PayPalCartPageCreditMessageComponentB2B";
    private static final String REDIRECTION_TO_GRID_CREDIT_MESSAGE_URL = "/braintreehac/productGridCreditMessage/config";
    private static final String REDIRECTION_TO_LIST_CREDIT_MESSAGE_URL = "/braintreehac/productListCreditMessage/config";
    private static final String REDIRECTION_TO_PRODUCT_DETAILS_PAGE_CREDIT_MESSAGE_URL = "/braintreehac/productDetailsPageCreditMessage/config";
    private static final String REDIRECTION_TO_CART_PAGE_CREDIT_MESSAGE_URL = "/braintreehac/cartPageCreditMessage/config";
    private static final String CREDIT_MESSAGE = "creditMessage";
    private static final String LABEL = "label";
    private static final String CREDIT_MESSAGE_CONFIG_PAGE = "creditMessageConfig";

    private static final Logger LOG = Logger.getLogger(PayPalCreditMessageConfigurationController.class);

    @Resource(name = "payPalCreditMessageConfigurationService")
    private PayPalCreditMessageConfigurationService payPalCreditMessageConfigurationService;


    @RequestMapping(value = "/productGridCreditMessage/config", method = RequestMethod.GET)
    public String configureProductGridCreditMessage(HttpServletRequest request, HttpServletResponse response, ModelMap model) throws Exception
    {

        payPalCreditMessageConfigurationService.processNormalRequest();

        SimpleCMSComponentModel creditMessageComponent = payPalCreditMessageConfigurationService
                .getComponentByUid(PRODUCT_GRID_CREDIT_MESSAGE);
        if (creditMessageComponent == null){
            creditMessageComponent = payPalCreditMessageConfigurationService.getComponentByUid(PRODUCT_GRID_CREDIT_MESSAGE_B2B);
        }

        model.addAttribute(CREDIT_MESSAGE, payPalCreditMessageConfigurationService
                .getCreditMessageItemByUid(creditMessageComponent));
        model.addAttribute(LABEL, "Product List Credit Message configuration");
        setEscentialValues(model);

        return CREDIT_MESSAGE_CONFIG_PAGE;
    }

    @RequestMapping(value = "/productListCreditMessage/config", method = RequestMethod.GET)
    public String configureProductListCreditMessage(HttpServletRequest request, HttpServletResponse response, ModelMap model) throws Exception
    {

        payPalCreditMessageConfigurationService.processNormalRequest();

        SimpleCMSComponentModel creditMessageComponent = payPalCreditMessageConfigurationService
                .getComponentByUid(PRODUCT_LIST_CREDIT_MESSAGE);
        if (creditMessageComponent == null){
            creditMessageComponent = payPalCreditMessageConfigurationService.getComponentByUid(PRODUCT_LIST_CREDIT_MESSAGE_B2B);
        }

        model.addAttribute(CREDIT_MESSAGE, payPalCreditMessageConfigurationService
                .getCreditMessageItemByUid(creditMessageComponent));
        model.addAttribute(LABEL, "Product List Credit Message configuration");
        setEscentialValues(model);

        return CREDIT_MESSAGE_CONFIG_PAGE;
    }

    @RequestMapping(value = "/productDetailsPageCreditMessage/config", method = RequestMethod.GET)
    public String configureProductDetailPageCreditMessage(HttpServletRequest request, HttpServletResponse response, ModelMap model) throws Exception
    {

        payPalCreditMessageConfigurationService.processNormalRequest();

        SimpleCMSComponentModel creditMessageComponent = payPalCreditMessageConfigurationService
                .getComponentByUid(PRODUCT_DETAILS_PAGE_CREDIT_MESSAGE);
        if (creditMessageComponent == null){
            creditMessageComponent = payPalCreditMessageConfigurationService.getComponentByUid(PRODUCT_DETAILS_PAGE_CREDIT_MESSAGE_B2B);
        }

        model.addAttribute(CREDIT_MESSAGE, payPalCreditMessageConfigurationService.getCreditMessageItemByUid(creditMessageComponent));
        model.addAttribute(LABEL, "Product Details Page Credit Message configuration");
        setEscentialValues(model);

        return CREDIT_MESSAGE_CONFIG_PAGE;
    }

    @RequestMapping(value = "/cartPageCreditMessage/config", method = RequestMethod.GET)
    public String configureCartPageCreditMessage(HttpServletRequest request, HttpServletResponse response, ModelMap model) throws Exception
    {

        payPalCreditMessageConfigurationService.processNormalRequest();

        SimpleCMSComponentModel creditMessageComponent = payPalCreditMessageConfigurationService
                .getComponentByUid(CART_PAGE_CREDIT_MESSAGE);
        if (creditMessageComponent == null){
            creditMessageComponent = payPalCreditMessageConfigurationService.getComponentByUid(CART_PAGE_CREDIT_MESSAGE_B2B);
        }

        model.addAttribute(CREDIT_MESSAGE, payPalCreditMessageConfigurationService
                .getCreditMessageItemByUid(creditMessageComponent));
        model.addAttribute(LABEL, "Cart Page Credit Message configuration");
        setEscentialValues(model);

        return CREDIT_MESSAGE_CONFIG_PAGE;
    }

    private void setEscentialValues(ModelMap model ){
        model.addAttribute("layout", payPalCreditMessageConfigurationService.getLayout());
        model.addAttribute("logoType", payPalCreditMessageConfigurationService.getLogoType());
        model.addAttribute("logoPosition", payPalCreditMessageConfigurationService.getLogoPosition());
        model.addAttribute("textColor", payPalCreditMessageConfigurationService.getTextColor());
        model.addAttribute("color", payPalCreditMessageConfigurationService.getColor());
        model.addAttribute("ratio", payPalCreditMessageConfigurationService.getRatio());

    }

    @RequestMapping(value = "/saveConfigs", method = RequestMethod.GET)
    public String saveCreditMessageConfigs(HttpServletRequest request, HttpServletResponse response, ModelMap model) throws Exception
    {

        Map<String, String[]> refreshedParameters = new HashMap<>(request.getParameterMap());
        final String creditMessageComponentUid = Arrays.stream(refreshedParameters.get("uid")).findFirst().get();
        refreshedParameters.remove("uid");

        payPalCreditMessageConfigurationService.processComponentUpdating(creditMessageComponentUid, refreshedParameters);

        return getRedirectUrl(creditMessageComponentUid);
    }

    private String getRedirectUrl(final String creditMessageUid)
    {
        String redirectURL = REDIRECT_PREFIX;

        switch (creditMessageUid) {
            case PRODUCT_GRID_CREDIT_MESSAGE:
            case PRODUCT_GRID_CREDIT_MESSAGE_B2B:
                redirectURL = redirectURL.concat(REDIRECTION_TO_GRID_CREDIT_MESSAGE_URL);
                break;

            case PRODUCT_LIST_CREDIT_MESSAGE:
            case PRODUCT_LIST_CREDIT_MESSAGE_B2B:
                redirectURL = redirectURL.concat(REDIRECTION_TO_LIST_CREDIT_MESSAGE_URL);
                break;

            case PRODUCT_DETAILS_PAGE_CREDIT_MESSAGE:
            case PRODUCT_DETAILS_PAGE_CREDIT_MESSAGE_B2B:
                redirectURL = redirectURL.concat(REDIRECTION_TO_PRODUCT_DETAILS_PAGE_CREDIT_MESSAGE_URL);
                break;

            case CART_PAGE_CREDIT_MESSAGE:
            case CART_PAGE_CREDIT_MESSAGE_B2B:
                redirectURL = redirectURL.concat(REDIRECTION_TO_CART_PAGE_CREDIT_MESSAGE_URL);
                break;

        }

        return redirectURL;
    }

}
