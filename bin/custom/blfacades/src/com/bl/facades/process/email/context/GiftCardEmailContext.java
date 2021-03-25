package com.bl.facades.process.email.context;


import com.bl.core.model.GiftCardEmailProcessModel;
import com.bl.core.model.GiftCardModel;
import de.hybris.platform.acceleratorservices.model.cms2.pages.EmailPageModel;
import de.hybris.platform.acceleratorservices.process.email.context.AbstractEmailContext;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.user.CustomerModel;

import java.text.DecimalFormat;

/**
 * @author Neeraj Singh
 *
 */
public class GiftCardEmailContext extends AbstractEmailContext<GiftCardEmailProcessModel>
{
    //private static final Logger LOG = Logger.getLogger(GiftCardEmailContext.class.getName());

    private GiftCardModel giftcard;
    private String customerEmail;
    private String custname;
    private String code;
    private String amount;

    @Override
    public void init(final GiftCardEmailProcessModel giftCardEmailProcessModel, final EmailPageModel emailPageModel)
    {
        //LOG.info("Gift Card Email Context");
        super.init(giftCardEmailProcessModel, emailPageModel);

        //	emailPageModel.setFromEmail(contactUsEmailProcessModel.getFromEmail());
        //	emailPageModel.setFromName(contactUsEmailProcessModel.getCustomerEmail());

        //setCustomerEmail(giftCardEmailProcessModel.getCustomer().getUid());
        setCustomerEmail(giftCardEmailProcessModel.getCustomer().getContactEmail());
        if (giftCardEmailProcessModel.getGiftcard() != null)
        {
            setGiftcard(giftCardEmailProcessModel.getGiftcard());
            setCustname(giftCardEmailProcessModel.getCustomer().getDisplayName());
            setCode(giftCardEmailProcessModel.getGiftcard().getCode());

            DecimalFormat df = new DecimalFormat("0.00");

            String amt = giftCardEmailProcessModel.getGiftcard().getCurrency().getSymbol()
                    + df.format(giftCardEmailProcessModel.getGiftcard().getAmount().doubleValue());

            setAmount(amt);
        }

    }

    @Override
    protected BaseSiteModel getSite(final GiftCardEmailProcessModel businessProcessModel)
    {
        return businessProcessModel.getSite();

    }

    @Override
    protected CustomerModel getCustomer(final GiftCardEmailProcessModel businessProcessModel)
    {
        return businessProcessModel.getCustomer();
    }

    @Override
    protected LanguageModel getEmailLanguage(final GiftCardEmailProcessModel businessProcessModel)
    {
        return businessProcessModel.getLanguage();
    }

    public void setGiftcard(final GiftCardModel giftcard)
    {
        this.giftcard = giftcard;
    }

    public GiftCardModel getGiftcard()
    {
        return giftcard;
    }

    public String getCustomerEmail()
    {
        return customerEmail;
    }

    public void setCustomerEmail(String customerEmail)
    {
        this.customerEmail = customerEmail;
    }

    public String getCustname()
    {
        return custname;
    }

    public void setCustname(String custname)
    {
        this.custname = custname;
    }

    public String getCode()
    {
        return code;
    }

    public void setCode(String code)
    {
        this.code = code;
    }

    public String getAmount()
    {
        return amount;
    }

    public void setAmount(String amount)
    {
        this.amount = amount;
    }
}

