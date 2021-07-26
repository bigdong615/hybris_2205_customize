package de.hybris.platform.hac.controller;

import de.hybris.platform.braintreehac.data.PayPalButtonStyleData;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import braintreehac.services.PayPalButtonConfigurationService;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Map;

@Controller
@RequestMapping("/braintreehac")
public class PayPalButtonConfigurationController {

    private static final Logger LOG = Logger.getLogger(PayPalButtonConfigurationController.class);

    private static final String REDIRECT_PREFIX = "redirect:";
    private static final String REDIRECTION_TO_CART_BUTTON_CONFIG_URL = "/braintreehac/button/config/cart";
    private static final String REDIRECTION_TO_MINI_CART_BUTTON_CONFIG_URL = "/braintreehac/button/config/minicart";
    private static final String REDIRECTION_TO_MARK_BUTTON_CONFIG_URL = "/braintreehac/button/config/mark";
    private static final String CART_BUTTON_CONFIG = "cart.button.config";
    private static final String MINI_CART_BUTTON_CONFIG = "mini.cart.button.config";
    private static final String MARK_BUTTON_CONFIG = "mark.button.config";
    private static final String PAYPAL_COMPONENT = "payPalComponent";
    private static final String BUTTON_CONFIG_PAGE = "buttonConfig";
    private static final String LABEL = "pageLabel";
    private static final String BUTTON_CONFIG = "buttonConfig";
    private static Map<String, Object> payPalButtonItemByUid;

    @Autowired
    private PayPalButtonConfigurationService payPalButtonConfigurationService;

    @RequestMapping(value = "/button/config/cart", method = RequestMethod.GET)
    public String saveCartButtonConfig(HttpServletRequest request, HttpServletResponse response, ModelMap model)throws Exception
    {
        String buttonStyleConfig = payPalButtonConfigurationService.getProperty(CART_BUTTON_CONFIG);
        PayPalButtonStyleData buttonStyleData = payPalButtonConfigurationService
                .parsingJsonStringIntoPayPalButtonConfigData(buttonStyleConfig);
        LOG.warn(buttonStyleData);

        setEscentialValues(model);
        model.addAttribute(PAYPAL_COMPONENT, buttonStyleData);
        model.addAttribute(LABEL, "Cart Page Smart Button configuration");
        model.addAttribute(BUTTON_CONFIG, CART_BUTTON_CONFIG);

        return BUTTON_CONFIG_PAGE;
    }

    @RequestMapping(value = "/button/config/minicart", method = RequestMethod.GET)
    public String saveMiniCartButtonConfig(HttpServletRequest request, HttpServletResponse response, ModelMap model)throws Exception
    {
        String buttonStyleConfig = payPalButtonConfigurationService.getProperty(MINI_CART_BUTTON_CONFIG);
        PayPalButtonStyleData buttonStyleData = payPalButtonConfigurationService
                .parsingJsonStringIntoPayPalButtonConfigData(buttonStyleConfig);
        LOG.warn(buttonStyleData);

        setEscentialValues(model);
        model.addAttribute(PAYPAL_COMPONENT, buttonStyleData);
        model.addAttribute(LABEL, "Mini Cart Smart Button configuration");
        model.addAttribute(BUTTON_CONFIG, MINI_CART_BUTTON_CONFIG);

        return BUTTON_CONFIG_PAGE;
    }

    @RequestMapping(value = "/button/config/mark", method = RequestMethod.GET)
    public String saveMarkButtonConfig(HttpServletRequest request, HttpServletResponse response, ModelMap model)throws Exception
    {
        String buttonStyleConfig = payPalButtonConfigurationService.getProperty(MARK_BUTTON_CONFIG);
        PayPalButtonStyleData buttonStyleData = payPalButtonConfigurationService
                .parsingJsonStringIntoPayPalButtonConfigData(buttonStyleConfig);
        LOG.warn(buttonStyleData);

        setEscentialValues(model);
        model.addAttribute(PAYPAL_COMPONENT, buttonStyleData);
        model.addAttribute(LABEL, "Mark Smart Button configuration");
        model.addAttribute(BUTTON_CONFIG, MARK_BUTTON_CONFIG);

        return BUTTON_CONFIG_PAGE;
    }

    private void setEscentialValues(ModelMap model ){
        model.addAttribute("color", payPalButtonConfigurationService.getSmartButtonColor());
        model.addAttribute("shape", payPalButtonConfigurationService.getSmartButtonShape());
        model.addAttribute("height", payPalButtonConfigurationService.getSmartButtonHeight());
        model.addAttribute("label", payPalButtonConfigurationService.getSmartButtonLabel());
        model.addAttribute("layout", payPalButtonConfigurationService.getSmartButtonLayout());
    }

    @RequestMapping(value = "/button/config/saveAll", method = RequestMethod.GET)
    public String saveAllButtonsConfig(HttpServletRequest request, HttpServletResponse response, ModelMap model) throws Exception
    {
        String buttonConfig = request.getParameter(BUTTON_CONFIG);
        payPalButtonConfigurationService.handleStyleDataUpdate(request, buttonConfig);

        return getRedirectUrl(buttonConfig);
    }

    private String getRedirectUrl(String buttonConfig){
        String redirectURL = REDIRECT_PREFIX;

        switch (buttonConfig) {
            case MARK_BUTTON_CONFIG:
                redirectURL = redirectURL.concat(REDIRECTION_TO_MARK_BUTTON_CONFIG_URL);
                break;

            case MINI_CART_BUTTON_CONFIG:
                redirectURL = redirectURL.concat(REDIRECTION_TO_MINI_CART_BUTTON_CONFIG_URL);
                break;

            case CART_BUTTON_CONFIG:
                redirectURL = redirectURL.concat(REDIRECTION_TO_CART_BUTTON_CONFIG_URL);
                break;

        }

        return redirectURL;
    }

    public Map<String, Object> getPayPalButtonItemByUid()
    {
        return payPalButtonItemByUid;
    }

    public void setPayPalButtonItemByUid(Map<String, Object> payPalButtonItemByUid)
    {
        this.payPalButtonItemByUid = payPalButtonItemByUid;
    }
}
