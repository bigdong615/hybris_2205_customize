package com.bl.storefront.security.cookie;

import com.bl.storefront.controllers.pages.BlControllerConstants;

/**
 * Cookie generator of selected duration
 *
 * @author Ritika
 */
public class BlRentalDurationCookieGenerator extends EnhancedCookieGenerator
{
  @Override
  public String getCookieName()
  {
    return BlControllerConstants.SELECTED_DURATION;
  }
}
