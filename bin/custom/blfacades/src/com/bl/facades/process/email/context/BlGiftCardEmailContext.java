package com.bl.facades.process.email.context;


import de.hybris.platform.acceleratorservices.model.cms2.pages.EmailPageModel;
import de.hybris.platform.acceleratorservices.process.email.context.AbstractEmailContext;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.user.CustomerModel;

import java.text.DecimalFormat;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.GiftCardEmailProcessModel;
import com.bl.core.model.GiftCardModel;
import com.bl.logging.BlLogger;

/**
 * It is a custom implementation of OOTB class {@link AbstractEmailContext} for gift card email.
 * @author Neeraj Singh
 */
public class BlGiftCardEmailContext extends AbstractEmailContext<GiftCardEmailProcessModel> {

  private static final Logger LOGGER = Logger.getLogger(BlGiftCardEmailContext.class.getName());

  private GiftCardModel giftcard;
  private String customerEmail;
  private String custname;
  private String code;
  private String amount;
  private String msg;
  private Boolean isPurchased;


@Override
  public void init(final GiftCardEmailProcessModel giftCardEmailProcessModel,
      final EmailPageModel emailPageModel) {

    BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG,"Gift Card Email Context");
    super.init(giftCardEmailProcessModel, emailPageModel);
    put(DISPLAY_NAME, giftCardEmailProcessModel.getGiftcard().getName());
    put(EMAIL, giftCardEmailProcessModel.getGiftcard().getCustomerEmail());
    setCustomerEmail(giftCardEmailProcessModel.getCustomerEmail());
    if (giftCardEmailProcessModel.getGiftcard() != null) {
      setGiftcard(giftCardEmailProcessModel.getGiftcard());
      setMsg(giftCardEmailProcessModel.getGiftcard().getMessage());
      setCustname(giftCardEmailProcessModel.getGiftcard().getName());
      setCode(giftCardEmailProcessModel.getGiftcard().getCode());
      final DecimalFormat decimalFormat = new DecimalFormat("0.00");
      if (giftCardEmailProcessModel.getGiftcard().getCurrency() != null
          && giftCardEmailProcessModel.getGiftcard().getAmount() != null) {
        final String giftCardAmount =
            giftCardEmailProcessModel.getGiftcard().getCurrency().getSymbol()
                + decimalFormat
                .format(giftCardEmailProcessModel.getGiftcard().getAmount().doubleValue());
        setAmount(giftCardAmount);
		  setIsPurchased(giftCardEmailProcessModel.getGiftcard().getIsPurchased());
      }
    }
  }

  @Override
  protected BaseSiteModel getSite(final GiftCardEmailProcessModel businessProcessModel) {
    return businessProcessModel.getSite();
  }

  @Override
  protected CustomerModel getCustomer(final GiftCardEmailProcessModel businessProcessModel) {
    return businessProcessModel.getCustomer();
  }

  @Override
  protected LanguageModel getEmailLanguage(final GiftCardEmailProcessModel businessProcessModel) {
    return businessProcessModel.getLanguage();
  }

  public void setGiftcard(final GiftCardModel giftcard) {
    this.giftcard = giftcard;
  }

  public GiftCardModel getGiftcard() {
    return giftcard;
  }

  public String getCustomerEmail() {
    return customerEmail;
  }

  public void setCustomerEmail(final String customerEmail) {
    this.customerEmail = customerEmail;
  }

  public String getCustname() {
    return custname;
  }

  public void setCustname(final String custname) {
    this.custname = custname;
  }

  public String getCode() {
    return code;
  }

  public void setCode(final String code) {
    this.code = code;
  }

  public String getAmount() {
    return amount;
  }

  public void setAmount(final String amount) {
    this.amount = amount;
  }
  /**
 * @return the msg
 */
public String getMsg()
{
	return msg;
}

/**
 * @param msg the msg to set
 */
public void setMsg(final String msg)
{
	this.msg = msg;
}

  /**
   * @return the isPurchased
   */
  public Boolean getIsPurchased()
  {
	  return isPurchased;
  }

  /**
   * @param isPurchased
   *           the isPurchased to set
   */
  public void setIsPurchased(final Boolean isPurchased)
  {
	  this.isPurchased = isPurchased;
  }
}