package com.bl.storefront.controllers.pages;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.acceleratorservices.config.SiteConfigService;
import de.hybris.platform.acceleratorservices.storefront.util.PageTitleResolver;
import de.hybris.platform.acceleratorstorefrontcommons.forms.RegisterForm;
import de.hybris.platform.acceleratorstorefrontcommons.security.AutoLoginStrategy;
import de.hybris.platform.acceleratorstorefrontcommons.strategy.CustomerConsentDataStrategy;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.cms2.data.PagePreviewCriteriaData;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.cms2.servicelayer.services.CMSPageService;
import de.hybris.platform.cms2.servicelayer.services.CMSPreviewService;
import de.hybris.platform.commercefacades.customer.CustomerFacade;
import de.hybris.platform.commercefacades.user.data.RegisterData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.site.BaseSiteService;

import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.BDDMockito;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.MessageSource;
import org.springframework.security.web.savedrequest.HttpSessionRequestCache;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.Validator;
import org.springframework.validation.support.BindingAwareModelMap;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.bl.storefront.validator.BlRegistrationValidator;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class LoginPageControllerTest {
  @InjectMocks
  private final LoginPageController loginPageController = Mockito.spy(new LoginPageController());

  private final Model page = Mockito.spy(new BindingAwareModelMap());
  private final String refer= "refer";
  public static final String EMAIL = "test11@gmail.com";
  public static final String PASSWORD ="12341234";
  public static final String CHECK_PASSWORD = "12341234";
  private static final String VIEW_FOR_PAGE = "account/accountLoginPage";
  private static final String VIEW_PREFIX = "pages/";
  private static final String FULL_VIEW_PATH = VIEW_PREFIX + VIEW_FOR_PAGE;
  private static final String TITLE_FOR_PAGE = "Login Test Title";
  private static final String REDIRECT_HOMEPAGE="redirect:/";

  @Mock
  private CustomerFacade customerFacade;
  @Mock
  private AutoLoginStrategy autoLoginStrategy;
  @Mock
  private CustomerConsentDataStrategy customerConsentDataStrategy;
  @Mock
  private HttpSessionRequestCache httpSessionRequestCache;
  @Mock
  private RegisterForm form;

  private BindingResult bindingResult;
  @Mock
  private HttpServletRequest request;
  @Mock
  private HttpServletResponse response;
  @Mock
  private HttpSession session;
  @Mock
  RedirectAttributes redirectModel;
  @Mock
  private Validator registerFormValidator;
  @Mock
  private ContentPageModel contentPageModel;
  @Mock
  private CMSPageService cmsPageService;
  @Mock
  private CMSPreviewService cmsPreviewService;
  @Mock
  private PageTitleResolver pageTitleResolver;
  private final PagePreviewCriteriaData pagePreviewCriteriaData = new PagePreviewCriteriaData();
  @Mock
  private SiteConfigService siteConfigService;
  @Mock
  private BaseSiteService baseSiteService;
  private final Locale locale =new Locale("en");
  @Mock
  private MessageSource messageSource;
  @Mock
  private I18NService i18nService;
  @Mock
  private BaseSiteModel baseSiteModel;

  RegisterData data ;
  @Before
  public void prepare() throws CMSItemNotFoundException {
	  //MockitoAnnotations.initMocks(this);
    data = new RegisterData();
    bindingResult= Mockito.spy(BindingResult.class);
    registerFormValidator= Mockito.spy(BlRegistrationValidator.class);
    BDDMockito.given(cmsPreviewService.getPagePreviewCriteria()).willReturn(pagePreviewCriteriaData);
    BDDMockito.given(cmsPageService.getPageForLabelOrId(Mockito.anyString(), Mockito.anyObject())).willReturn(contentPageModel);
    BDDMockito.given(pageTitleResolver.resolveContentPageTitle(Mockito.anyString())).willReturn(TITLE_FOR_PAGE);

  }


  @Test
  public void shouldRenderLoginPage() throws CMSItemNotFoundException {
    BDDMockito.given(baseSiteService.getCurrentBaseSite()).willReturn(baseSiteModel);
    final String acknowledgeJspPage =loginPageController.doLogin(refer,false , page, request, response,session);
    assertNotNull(acknowledgeJspPage);
    assertEquals(FULL_VIEW_PATH,acknowledgeJspPage);
    Mockito.verify(page).addAttribute("cmsPage",contentPageModel);
    Mockito.verify(page).addAttribute("pageTitle",TITLE_FOR_PAGE);
    assertNotNull(page.getAttribute("loginForm"));
    assertNotNull(page.getAttribute("registerForm"));
    assertNotNull(page.getAttribute("guestForm"));
  }

  @Test
  public void shouldRegisterCustomer() throws CMSItemNotFoundException {
    createBasicRegisterForm();
    BDDMockito.given(request.getHeader("Referer")).willReturn("/");
    final String  redirectUrl = loginPageController.doRegister(refer,form,bindingResult,page,request,response,redirectModel);
    assertNotNull(redirectUrl);
    assertEquals(REDIRECT_HOMEPAGE,redirectUrl);
  }

  @Test
  public void shouldValidateDublicateUid() throws CMSItemNotFoundException, DuplicateUidException {
    createBasicRegisterForm();
    BDDMockito.given(baseSiteService.getCurrentBaseSite()).willReturn(baseSiteModel);
    BDDMockito.given(baseSiteService.getCurrentBaseSite().getUid()).willReturn("ElectonicsSite");
    BDDMockito.doThrow(new DuplicateUidException("Email has already present")).when(customerFacade).register(Mockito.any(RegisterData.class));
    final String  acknowledgeJspPage = loginPageController.doRegister(refer,form,bindingResult,page,request,response,redirectModel);
    assertNotNull(acknowledgeJspPage);
    Assert.assertEquals(FULL_VIEW_PATH,acknowledgeJspPage);
    Mockito.verify(page).addAttribute("cmsPage",contentPageModel);
    Mockito.verify(page).addAttribute("pageTitle",TITLE_FOR_PAGE);
    assertNotNull(page.getAttribute("loginForm"));
    assertNotNull(page.getAttribute("registerForm"));
    assertNotNull(page.getAttribute("accErrorMsgs"));
  }


  public void createBasicRegisterForm(){
    BDDMockito.given(form.getEmail()).willReturn(EMAIL);
    BDDMockito.given(form.getPwd()).willReturn(PASSWORD);
    BDDMockito.given(form.getCheckPwd()).willReturn(CHECK_PASSWORD);
  }


}
