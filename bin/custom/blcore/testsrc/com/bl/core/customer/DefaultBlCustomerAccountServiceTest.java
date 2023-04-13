package com.bl.core.customer;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.user.data.RegisterData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.commerceservices.i18n.CommerceCommonI18NService;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.PasswordEncoderConstants;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.services.customer.impl.DefaultBlCustomerAccountService;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultBlCustomerAccountServiceTest {

  @InjectMocks
  private final DefaultBlCustomerAccountService customerAccountService = Mockito.spy(new DefaultBlCustomerAccountService());

  public static final String EMAIL = "test@gmail.com";
  public static final String PASSWORD = "12345678";
  private final String passwordEncoding = PasswordEncoderConstants.DEFAULT_ENCODING;

  @Mock
  private CustomerModel newCustomer;
  @Mock
  private AddressModel addressModel1;
  @Mock
  private AddressModel addressModel2;
  @Mock
  private UserService userService;
  @Mock
  private ModelService modelService;
  @Mock
  private CommerceCommonI18NService commerceCommonI18NService;

  RegisterData registerData;
  List addressList;

  @Before
  public void prepare(){
	  // MockitoAnnotations.initMocks(this);
    registerData = new RegisterData();
    registerData.setLogin(EMAIL);
    registerData.setPassword(PASSWORD);
    addressList = new ArrayList<AddressModel>();
     }

  @Test
  public void shouldRegisterCustomer() throws DuplicateUidException {
    customerAccountService.register(newCustomer,PASSWORD);
    verify(modelService).save(newCustomer);
    verify(modelService, times(1)).save(newCustomer);
  }

  @Test
  public void shouldSetDefaultBillingAddress(){
    addressList.add(addressModel1);
    addressList.add(addressModel2);
    when(newCustomer.getAddresses()).thenReturn(addressList);
    customerAccountService.setDefaultBillingAddress(newCustomer,addressModel1);
    verify(customerAccountService, times(1)).setDefaultBillingAddress(newCustomer,addressModel1);
    verify(modelService).save(newCustomer);
    verify(modelService, times(1)).save(newCustomer);
    verify(modelService,times(1)).refresh(newCustomer);
  }


}
