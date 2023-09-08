package com.bl.storefront.controllers.misc;

import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

@Controller
@RequestMapping("/writereview")
public class BlReviewPageController extends AbstractPageController {

    private static final String BL_REVIEW_PAGE = "blWriteReviewPage";

    @Resource(name = "configurationService")
    private ConfigurationService configurationService;

    @ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
    private RentalDateDto getRentalsDuration()
    {
        return BlRentalDateUtils.getRentalsDuration();
    }

    @GetMapping(params = "!q")
    public String textSearch(@RequestParam(value = "pr_page_id", defaultValue = "") final String productCode,
                             final HttpServletRequest request, final Model model) throws CMSItemNotFoundException
    {
        final ContentPageModel blReviewPage = getContentPageForLabelOrId(BL_REVIEW_PAGE);
        storeCmsPageInModel(model, blReviewPage);
        setUpMetaDataForContentPage(model, blReviewPage);
        model.addAttribute("productCode", productCode);
        addReviewConfigs(model);
        model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
        if(StringUtils.isBlank(productCode)){
            GlobalMessages.addErrorMessage(model, "text.review.error.message");
        }
        return getViewForPage(model);

    }

    private void addReviewConfigs(Model model) {
        model.addAttribute("merchantAPI_Key", getConfigurationService().getConfiguration().getString("powerreviews.merchant.api", "54a82048-9c20-4a13-a01b-dd5b0415a965"));
        model.addAttribute("merchantID", getConfigurationService().getConfiguration().getString("powerreviews.merchant.id ", "1415200746"));
        model.addAttribute("merchantGroupId", getConfigurationService().getConfiguration().getString("powerreviews.merchant.groupid ", "2120371445"));
    }

    @Override
    public ConfigurationService getConfigurationService() {
        return configurationService;
    }

    public void setConfigurationService(ConfigurationService configurationService) {
        this.configurationService = configurationService;
    }
}
