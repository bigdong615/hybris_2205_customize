package com.bl.storefront.security.impl;

import com.bl.storefront.controllers.pages.BlControllerConstants;
import de.hybris.platform.acceleratorstorefrontcommons.security.StorefrontAuthenticationSuccessHandler;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang.StringUtils;
import org.springframework.security.core.Authentication;

/**
 * @author Ritika
 * This class is overridded to add restricted groups presence in the session
 *
 */
public class BlStorefrontAuthenticationSuccessHandler extends
    StorefrontAuthenticationSuccessHandler {

  private UserService userService;
  private SessionService sessionService;

  /**
   * method to remove the add restriction in the session
   * @param request
   * @param response
   * @param authentication
   * @throws IOException
   * @throws ServletException
   */
  @Override
  public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {

      final CustomerModel customerModel = (CustomerModel) getUserService().getCurrentUser();
      if (customerModel != null && customerModel.getGroups().stream()
          .anyMatch(userGroup -> StringUtils.containsIgnoreCase(userGroup.getUid(), BlControllerConstants.BL_GROUP))) {
        getSessionService().setAttribute(BlControllerConstants.HAS_USER_RESTRICTION, String.valueOf(true));
      }
     super.onAuthenticationSuccess(request, response, authentication);
  }


  public UserService getUserService() {
    return userService;
  }

  public void setUserService(UserService userService) {
    this.userService = userService;
  }

  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(SessionService sessionService) {
    this.sessionService = sessionService;
  }
}