package com.braintree.facade.impl;


import com.bl.core.services.customer.BlCustomerAccountService;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.dto.BraintreeInfo;
import com.braintree.transaction.service.BrainTreeTransactionService;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.*;

public class BrainTreeUserFacadeImplTest  {

    public static final String NONCE_123456 = "nonce123456";
    public static final String JOHN_DOE_BRAINTREE_COM = "johnDoe@braintree.com";
    public static final String JOHN_DOE = "john Doe";
    public static final String JON_DOE_EMAIL_COM = "Jon_doe@email.com";
    public static final String ADDRESS_ID = "1231";
    public static final String CUSTOMER_ID = "123332131";
    public static final boolean TRUE = true;
    public static final boolean FALSE = false;

    @Mock
    private BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData;

    @Mock
    private UserService userService ;
    @Mock
    private ModelService modelService;
    @Mock
    private BrainTreePaymentService brainTreePaymentService;
    @Mock
    private  BrainTreeTransactionService brainTreeTransactionService;

    @Mock
    private Converter<AddressData, AddressModel> addressReverseConverter;
    @Mock
    private Converter<BrainTreeSubscriptionInfoData, BraintreeInfo> brainTreeSubscriptionInfoConverter;

    @Mock
    private BrainTreePaymentMethodResult creditCardPaymentMethod;
    @Mock
    private BrainTreeConfigService brainTreeConfigService;
    @Mock
    private CustomerModel customerModel;
    @Mock
    private BlCustomerAccountService customerAccountService;
    @Mock
    private Converter<AddressModel, AddressData> addressConverter;

    @InjectMocks
    private  BrainTreeUserFacadeImpl brainTreeUserFacadeImpl = new BrainTreeUserFacadeImpl();

    @Before
    public void init()
    {
        MockitoAnnotations.initMocks(this);

        AddressModel addressModel = mock(AddressModel.class);

        when(customerModel.getBraintreeCustomerId()).thenReturn(CUSTOMER_ID);
        when(customerModel.getContactEmail()).thenReturn(JOHN_DOE_BRAINTREE_COM);
        when(customerModel.getName()).thenReturn(JOHN_DOE);
        when(userService.getCurrentUser()).thenReturn(customerModel);
        when(brainTreeConfigService.getVerifyCard()).thenReturn(TRUE);
        brainTreeUserFacadeImpl.setAddressReverseConverter(addressReverseConverter);
        brainTreeUserFacadeImpl.setBrainTreeSubscriptionInfoConverter(brainTreeSubscriptionInfoConverter);

        when(creditCardPaymentMethod.isSuccess()).thenReturn(TRUE);

        when(brainTreePaymentService.createCreditCardPaymentMethod(Matchers.any())).thenReturn(creditCardPaymentMethod);

        when(brainTreeSubscriptionInfoData.getEmail()).thenReturn(JON_DOE_EMAIL_COM);
        when(brainTreeSubscriptionInfoData.getNonce()).thenReturn(NONCE_123456);
        when(brainTreeSubscriptionInfoData.getCardholder()).thenReturn(JOHN_DOE);

        when(modelService.create(AddressModel.class)).thenReturn(addressModel);

    }

    @Test
    public void shouldAddPaymentMethod()
    {
        // given
        AddressData addressData = mock(AddressData.class);
        when(addressData.getBrainTreeAddressId()).thenReturn(ADDRESS_ID);
        when(brainTreeSubscriptionInfoData.getAddressData()).thenReturn(addressData);
        when(brainTreeConfigService.getVerifyCardOnVaulting()).thenReturn(TRUE);

        //when
        brainTreeUserFacadeImpl.addPaymentMethod(brainTreeSubscriptionInfoData);

        //then
        verify(brainTreeConfigService).getMerchantAccountIdForCurrentSiteAndCurrency();
        verify(brainTreeConfigService).getVerifyCard();
        verify(brainTreeConfigService).getVerifyCardOnVaulting();
    }

    @Test
    public void shouldAddPaymentMethodButNotMErchantIdForCurrentCustomer()
    {
        // given
        AddressData addressData = mock(AddressData.class);
        when(addressData.getBrainTreeAddressId()).thenReturn(ADDRESS_ID);
        when(brainTreeSubscriptionInfoData.getAddressData()).thenReturn(addressData);
        when(brainTreeConfigService.getVerifyCard()).thenReturn(TRUE);
        when(brainTreeConfigService.getVerifyCardOnVaulting()).thenReturn(FALSE);

        //when
        brainTreeUserFacadeImpl.addPaymentMethod(brainTreeSubscriptionInfoData);

        //then
        verify(brainTreeConfigService,never()).getMerchantAccountIdForCurrentSiteAndCurrency();
        verify(brainTreeConfigService,never()).getVerifyCard();
    }

    @Test
    public void shouldAddPaymentMethodAndReturnNull()
    {
        //when
        BrainTreePaymentInfoModel brainTreePaymentInfoModel = brainTreeUserFacadeImpl.addPaymentMethod(brainTreeSubscriptionInfoData);

        //then
        assertNull(brainTreePaymentInfoModel);
        verify(brainTreeConfigService,never()).getMerchantAccountIdForCurrentSiteAndCurrency();
        verify(brainTreeConfigService,never()).getVerifyCard();
    }

    @Test
    public void shouldSetDefaultBillingAddress(){
        AddressData addressData = mock(AddressData.class);
        AddressModel addressModel = mock(AddressModel.class);
        when(addressData.getId()).thenReturn(ADDRESS_ID);
        when(customerAccountService.getAddressForCode(customerModel,addressData.getId())).thenReturn(addressModel);
        brainTreeUserFacadeImpl.setDefaultBillingAddress(addressData);
        verify(customerAccountService,times(1)).setDefaultBillingAddress(customerModel,addressModel);
    }

    @Test
    public void getDefaultBillingAddress(){
        AddressModel addressModel = mock(AddressModel.class);
        AddressData addressData = mock(AddressData.class);
        when(customerModel.getDefaultBillingAddress()).thenReturn(addressModel);
        when(addressConverter.convert(addressModel)).thenReturn(addressData);
        AddressData billingAddress = brainTreeUserFacadeImpl.getDefaultBillingAddress();
        assertNotNull(addressData);
        verify(addressConverter,times(1)).convert(addressModel);
    }

}
