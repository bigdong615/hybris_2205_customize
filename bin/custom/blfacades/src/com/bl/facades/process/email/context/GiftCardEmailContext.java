package com.bl.facades.process.email.context;


import com.bl.core.model.GiftCardEmailProcessModel;
import com.bl.core.model.GiftCardModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.acceleratorservices.model.cms2.pages.EmailPageModel;
import de.hybris.platform.acceleratorservices.process.email.context.AbstractEmailContext;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.user.CustomerModel;
import java.text.DecimalFormat;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is a custom implementation of OOTB class {@link AbstractEmailContext}.
 * @author Neeraj Singh
 */
public class GiftCardEmailContext extends AbstractEmailContext<GiftCardEmailProcessModel> {

  private static final Logger LOGGER = Logger.getLogger(GiftCardEmailContext.class.getName());

  private GiftCardModel giftcard;
  private String customerEmail;
  private String custname;
  private String code;
  private String amount;

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
      setCustname(giftCardEmailProcessModel.getGiftcard().getName());
      setCode(giftCardEmailProcessModel.getGiftcard().getCode());
      DecimalFormat decimalFormat = new DecimalFormat("0.00");
      final String giftCardAmount = giftCardEmailProcessModel.getGiftcard().getCurrency().getSymbol()
          + decimalFormat.format(giftCardEmailProcessModel.getGiftcard().getAmount().doubleValue());
      setAmount(giftCardAmount);
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

  public void setCustomerEmail(String customerEmail) {
    this.customerEmail = customerEmail;
  }

  public String getCustname() {
    return custname;
  }

  public void setCustname(String custname) {
    this.custname = custname;
  }

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getAmount() {
    return amount;
  }

  public void setAmount(String amount) {
    this.amount = amount;
  }
}