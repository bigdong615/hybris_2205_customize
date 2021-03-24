package com.bl.core.customer;

import com.bl.core.services.customer.impl.DefaultBlCustomerAccountService;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.user.data.RegisterData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.PasswordEncoderConstants;
import de.hybris.platform.servicelayer.user.UserService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@UnitTest
public class DefaultBlCustomerAccountServiceTest {

  @InjectMocks
  private final DefaultBlCustomerAccountService customerAccountService = Mockito.spy(new DefaultBlCustomerAccountService());

  public static final String EMAIL = "test@gmail.com";
  public static final String PASSWORD = "12345678";
  private String passwordEncoding = PasswordEncoderConstants.DEFAULT_ENCODING;

  @Mock
  private CustomerModel newCustomer;
  @Mock
  private UserService userService;
  @Mock
  private ModelService modelService;

  RegisterData registerData;

  @Before
  public void prepare(){
    MockitoAnnotations.initMocks(this);
    registerData = new RegisterData();
    registerData.setLogin(EMAIL);
    registerData.setPassword(PASSWORD);
  }

  @Test
  public void shouldRegisterCustomer() throws DuplicateUidException {
    customerAccountService.register(newCustomer,PASSWORD);
  }

}
