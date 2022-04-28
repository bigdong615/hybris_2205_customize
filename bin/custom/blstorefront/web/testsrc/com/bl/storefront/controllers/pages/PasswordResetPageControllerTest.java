package com.bl.storefront.controllers.pages;

import static de.hybris.platform.testframework.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.acceleratorservices.storefront.util.PageTitleResolver;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.Breadcrumb;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.forms.ForgottenPwdForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.UpdatePasswordFormValidator;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.AbstractPageModel;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.cms2.servicelayer.services.CMSPageService;
import de.hybris.platform.cms2.servicelayer.services.CMSPreviewService;
import de.hybris.platform.commercefacades.customer.CustomerFacade;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.support.BindingAwareModelMap;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.bl.storefront.forms.BlUpdatePwdForm;

@UnitTest
public class PasswordResetPageControllerTest {
    @InjectMocks
    private final PasswordResetPageController passwordResetPageController = Mockito.spy(new PasswordResetPageController());

    private final Model page = Mockito.spy(new BindingAwareModelMap());
    private BindingResult bindingResult;
    private static final String PasswordResetRequestPopup = "fragments/password/passwordResetRequestPopup";
    private static final String ForgotPasswordValidationMessage = "fragments/password/forgotPasswordValidationMessage";
    private static final String PasswordResetChangePage = "pages/password/passwordResetChangePage";
    public static final String EMAIL = "test11@gmail.com";
    public static final String TOKEN = "U/AKJuIpHRBPcyKNHACqAtJADRK0DCQaYX/bwVUqdBJ26gNB0blVqX7qEjAM7YGoZgzdGFP4Iiyin6nwYur7cMIOzf/ITArzBav9gSx+4o+YTfobp/9zPl2Deceok347jUV4KjitXNLQUvyutHQovrUEya1NJ2Dvs4+h2G44/5eV";
    public static final String BLANK_TOKEN = "";
    public static final String REDIRECT_HOME = "redirect:/";
    private static final String TITLE_FOR_CONTENT_PAGE =  "Update Forgotten Password";
    private static final String TITLE_FOR_PAGE ="Update Forgotten Password | Electronics Site";
    private static final String PASSWORD="12345678";
    private static final String REDIRECT_LOGIN = "redirect:/login";

    @Mock
    ForgottenPwdForm forgottenPwdForm;
    @Mock
    private CustomerFacade customerFacade;
    @Mock
    private CMSPageService cmsPageService;
    @Mock
    private AbstractPageModel abstractPageModel;
    @Mock
    private ContentPageModel contentPageModel;
    @Mock
    private CMSPreviewService cmsPreviewService;
    @Mock
    private PageTitleResolver pageTitleResolver;
    @Mock
    private ResourceBreadcrumbBuilder resourceBreadcrumbBuilder;
    @Mock
    private Breadcrumb breadcrumb;
    @Mock
    private BlUpdatePwdForm form;
    @Mock
    private RedirectAttributes redirectModel;
    @Mock
    private UpdatePasswordFormValidator updatePasswordFormValidator;

    @Before
    public void prepare()
    {
        MockitoAnnotations.initMocks(this);
        bindingResult= Mockito.spy(BindingResult.class);
    }

    @Test
    public  void shouldShowPasswordResetPopup() throws CMSItemNotFoundException {
        final String acknowledgeJsp = passwordResetPageController.getPasswordRequest(page);
        assertNotNull(page.getAttribute("forgottenPwdForm"));
        assertEquals( PasswordResetRequestPopup,acknowledgeJsp);
    }

    @Test
    public void shouldNotSubmitErrorForm() throws CMSItemNotFoundException {
        BDDMockito.given(forgottenPwdForm.getEmail()).willReturn(null);
        BDDMockito.given(bindingResult.hasErrors()).willReturn(true);
        final String acknowledgeJsp=passwordResetPageController.passwordRequest(forgottenPwdForm,bindingResult,page);
        assertEquals(PasswordResetRequestPopup,acknowledgeJsp);
    }

    @Test
    public void shouldSendForgotPasswordLink() throws CMSItemNotFoundException {

        BDDMockito.given(forgottenPwdForm.getEmail()).willReturn(EMAIL);
        final String acknowledgeJsp=passwordResetPageController.passwordRequest(forgottenPwdForm,bindingResult,page);
        assertEquals(ForgotPasswordValidationMessage,acknowledgeJsp);
    }

    @Test
    public void shouldRenderChangePasswordPage() throws CMSItemNotFoundException {
        BDDMockito.given(cmsPageService.getPageForLabelOrId(Mockito.anyString(), Mockito.anyObject())).willReturn(contentPageModel);BDDMockito.given(contentPageModel.getTitle()).willReturn(TITLE_FOR_CONTENT_PAGE);
        BDDMockito.given(pageTitleResolver.resolveContentPageTitle(Mockito.anyString())).willReturn(TITLE_FOR_PAGE);
        final List breadcrumbsList = new ArrayList();
        breadcrumbsList.add(breadcrumb);
        BDDMockito.given(resourceBreadcrumbBuilder.getBreadcrumbs(Mockito.anyString())).willReturn(breadcrumbsList);
        final String acknowledgeJsp =passwordResetPageController.getChangePassword(TOKEN,page);

        Mockito.verify(page).addAttribute("cmsPage",contentPageModel);
        Mockito.verify(page).addAttribute("pageTitle",TITLE_FOR_PAGE);
        assertNotNull(page.getAttribute("updatePwdForm"));
        assertEquals(PasswordResetChangePage,acknowledgeJsp);
    }

    @Test
    public void shouldRedirectToHomePage() throws CMSItemNotFoundException {
        final String acknowledgeJsp =passwordResetPageController.getChangePassword(BLANK_TOKEN,page);
        assertEquals(REDIRECT_HOME,acknowledgeJsp);
    }

    @Test
    public void shouldUpdatePassword() throws CMSItemNotFoundException {
        BDDMockito.given(form.getToken()).willReturn(TOKEN);
        BDDMockito.given(form.getPwd()).willReturn(PASSWORD);
        final String acknowledgeJsp =passwordResetPageController.changePassword(form,bindingResult,page,redirectModel);
        assertEquals(REDIRECT_LOGIN,acknowledgeJsp);
    }

}
