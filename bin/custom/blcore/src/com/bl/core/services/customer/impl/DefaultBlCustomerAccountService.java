package com.bl.core.services.customer.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.services.customer.BlCustomerAccountService;
import com.bl.esp.dto.forgotPassword.data.ForgotPasswordRequestData;
import com.bl.logging.BlLogger;
import com.braintree.customer.dao.BrainTreeCustomerAccountDao;
import de.hybris.platform.acceleratorservices.urlresolver.SiteBaseUrlResolutionService;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.commerceservices.customer.PasswordMismatchException;
import de.hybris.platform.commerceservices.customer.impl.DefaultCustomerAccountService;
import de.hybris.platform.commerceservices.event.ChangeUIDEvent;
import de.hybris.platform.commerceservices.security.SecureToken;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;

import de.hybris.platform.util.Config;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

/**
 *This is created to override register method and disable registration email.
 *
 * @author vijay vishwakarma
 */

public class DefaultBlCustomerAccountService extends DefaultCustomerAccountService implements BlCustomerAccountService {

    private static final Logger LOG = Logger.getLogger(DefaultBlCustomerAccountService.class);
    @Resource(name="brainTreeCustomerAccountDao")
    private BrainTreeCustomerAccountDao brainTreeCustomerAccountDao;
    private int expiresInMinutes = 30;
    private SiteBaseUrlResolutionService siteBaseUrlResolutionService;
    private BlESPEventService blESPEventService;
    /**
     *This method is overridden to disable register email.
     */
    @Override
    public void register(final CustomerModel customerModel, final String password) throws DuplicateUidException
    {
        registerCustomer(customerModel, password);
    }

    /**
     * This method is responsible for setting default billing addresses with current customers.
     */
    @Override
    public void setDefaultBillingAddress(final CustomerModel customerModel, final AddressModel addressModel)
    {
        validateParameterNotNull(customerModel, "Customer model cannot be null");
        validateParameterNotNull(addressModel, "Address model cannot be null");
        if (customerModel.getAddresses().contains(addressModel))
        {
            customerModel.setDefaultBillingAddress(addressModel);
        }
        else
        {
            final AddressModel cloneBillingAddress = getModelService().clone(addressModel);
            cloneBillingAddress.setOwner(customerModel);
            getModelService().save(cloneBillingAddress);
            final List<AddressModel> customerAddresses = new ArrayList<>();
            customerAddresses.addAll(customerModel.getAddresses());
            customerAddresses.add(cloneBillingAddress);
            customerModel.setAddresses(customerAddresses);
            customerModel.setDefaultBillingAddress(cloneBillingAddress);
        }
        getModelService().save(customerModel);
        getModelService().refresh(customerModel);
    }

    /**
     *  This method used to get address list from db.
     * @param customerModel
     * @return
     */
    @Override
    public List<AddressModel> getAddressBookDeliveryEntries(final CustomerModel customerModel)
    {
        validateParameterNotNull(customerModel, "Customer model cannot be null");
        return brainTreeCustomerAccountDao.findAddressBookDeliveryEntriesForCustomer(customerModel,
            getCommerceCommonI18NService().getAllCountries());
    }
    /**
     * {@inheritDoc}
     */
    @Override
    public List<AddressModel> getShippingAddressBookEntries(final CustomerModel customerModel)
    {
        validateParameterNotNull(customerModel, "Customer model cannot be null");
        return getCustomerAccountDao().findAddressBookDeliveryEntriesForCustomer(customerModel,
            getCommerceCommonI18NService().getAllCountries());
    }

    /**
     * This method is used to change the UID for the current customer
     * @param newUid
     *           given new uid
     * @param currentPassword
     *           password checked for authorization change
     * @throws DuplicateUidException
     * @throws PasswordMismatchException
     */
    @Override
    public void changeUid(final String newUid, final String currentPassword)
        throws DuplicateUidException, PasswordMismatchException
    {
        Assert.hasText(newUid, "The field [newEmail] cannot be empty");
        Assert.hasText(currentPassword, "The field [currentPassword] cannot be empty");

        final String newUidLower = newUid.toLowerCase();
        final CustomerModel currentUser = (CustomerModel) getUserService().getCurrentUser();
        final ChangeUIDEvent event = new ChangeUIDEvent(currentUser.getOriginalUid(), newUid);
        currentUser.setOriginalUid(newUid);
        checkUidUniqueness(newUidLower);
        adjustPassword(currentUser, newUidLower, currentPassword);
        getEventService().publishEvent(initializeEvent(event, currentUser));
    }

    /**
     * This method used to generate reset password url.
     * @param customerModel
     */
    @Override
    public void forgottenPassword(final CustomerModel customerModel)
    {
        validateParameterNotNullStandardMessage("customerModel", customerModel);
        final long timeStamp = getTokenValiditySeconds() > 0L ? new Date().getTime() : 0L;
        final SecureToken data = new SecureToken(customerModel.getUid(), timeStamp);
        final String token = getSecureTokenService().encryptData(data);
        customerModel.setToken(token);
        getModelService().save(customerModel);
        final ForgotPasswordRequestData forgotPasswordRequestData= new ForgotPasswordRequestData();
        try {
            forgotPasswordRequestData.setPasswordLink( getSiteBaseUrlResolutionService()
                .getWebsiteUrlForSite(getBaseSiteService().getCurrentBaseSite(),
                    StringUtils.EMPTY, Boolean.TRUE, BlCoreConstants.UPDATE_PASSWORD_URL,
                    "token=" + URLEncoder.encode(token, BlCoreConstants.DEFAULT_ENCODING_STRING)));
        }catch(Exception e){
            BlLogger.logMessage(LOG, Level.ERROR,"Some error occurs whiling generating reset password link for user {0}:",customerModel.getUid(),e);
        }
        forgotPasswordRequestData.setEmailAddress(customerModel.getUid());
        forgotPasswordRequestData.setTimeout(getExpiresInMinutes());
        getBlESPEventService().sendForgotPasswordRequest(forgotPasswordRequestData);
    }

    private int getExpiresInMinutes()
    {
        final String passwordExpireTime = Config.getParameter("forgotPassword.link.expiry.time");
        try {
            expiresInMinutes = StringUtils.isNotEmpty(passwordExpireTime) ? Integer.parseInt(passwordExpireTime) : expiresInMinutes;
        }catch (NumberFormatException e){
            BlLogger.logMessage(LOG, Level.ERROR,"Some error occurs due to invalid password expiry time {0} :",passwordExpireTime,e);
        }
        return expiresInMinutes;
    }
    public SiteBaseUrlResolutionService getSiteBaseUrlResolutionService() {
        return siteBaseUrlResolutionService;
    }

    public void setSiteBaseUrlResolutionService(
        SiteBaseUrlResolutionService siteBaseUrlResolutionService) {
        this.siteBaseUrlResolutionService = siteBaseUrlResolutionService;
    }
    public BlESPEventService getBlESPEventService() {
        return blESPEventService;
    }

    public void setBlESPEventService(BlESPEventService blESPEventService) {
        this.blESPEventService = blESPEventService;
    }

}

