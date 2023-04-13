package com.bl.storefront.controllers.pages;

import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.util.Config;
import org.apache.commons.lang.BooleanUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@Controller
@RequestMapping("/contactus")
public class ContactUsPageController extends DefaultPageController{
    public static final String REDIRECT_PREFIX = "redirect:";

    @GetMapping
    public String getRedirectLink(final Model model, final HttpServletRequest request, final HttpServletResponse response)
            throws CMSItemNotFoundException
    {
        final String latestRedirectURL = Config.getString("bl.contact.us.redirect.url","");
        final String redirectToLatestURL = Config.getString("bl.contact.us.redirect","");

        if(latestRedirectURL !=null || redirectToLatestURL !=null) {
            if (BooleanUtils.isTrue(Boolean.valueOf(redirectToLatestURL))) {
                return REDIRECT_PREFIX + latestRedirectURL;
            }
        }

        return super.get(model, request, response);

    }
}
