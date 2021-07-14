package com.bl.storefront.forms;

import de.hybris.platform.acceleratorstorefrontcommons.util.XSSFilterUtil;
import java.io.Serializable;

/**
 * It's gift card form
 * @author Neeraj Singh
 */
public class GiftCardForm implements Serializable {

  private static final long serialVersionUID = 1L;
  private String giftCardCode;

  public String getGiftCardCode() {
    return giftCardCode;
  }

  public void setGiftCardCode(String giftCardCode) {
    this.giftCardCode = XSSFilterUtil.filter(giftCardCode);
  }
}
