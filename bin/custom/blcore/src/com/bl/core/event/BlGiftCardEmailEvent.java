package com.bl.core.event;

import com.bl.core.model.GiftCardModel;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.commerceservices.event.AbstractCommerceUserEvent;

/**
 * It is a custom implementation of OOTB class {@link AbstractCommerceUserEvent}.
 * @author Neeraj Singh
 *
 */
public class BlGiftCardEmailEvent extends AbstractCommerceUserEvent<BaseSiteModel>
{
  private String userEmail;
  private GiftCardModel giftcard;

  /**
   * @return the userEmail
   */
  public String getUserEmail()
  {
    return userEmail;
  }

  /**
   * @param userEmail
   *           the userEmail to set
   */
  public void setUserEmail(final String userEmail)
  {
    this.userEmail = userEmail;
  }

  /**
   * @return the giftcard
   */
  public GiftCardModel getGiftcard()
  {
    return giftcard;
  }

  /**
   * @param giftcard the giftcard to set
   */
  public void setGiftcard(GiftCardModel giftcard)
  {
    this.giftcard = giftcard;
  }
}
