package com.bl.facades.customer;

import static org.junit.Assert.assertEquals;

import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.customer.impl.DefaultBlCustomerFacade;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.user.data.RegisterData;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@UnitTest
public class DefaultBlCustomerFacadeTest {

  @InjectMocks
  private final DefaultBlCustomerFacade customerFacade = Mockito.spy(new DefaultBlCustomerFacade());

  public static final String EMAIL = "test@gmail.com";
  public static final String PASSWORD = "12345678";

  @Mock
  private ModelService modelService;

  private CustomerModel newCustomer;
  @Mock
  private CommonI18NService commonI18NService;
  @Mock
  private CustomerAccountService customerAccountService;

  RegisterData registerData;

  @Before
public void prepare(){
    MockitoAnnotations.initMocks(this);
    registerData = new RegisterData();
    registerData.setLogin(EMAIL);
    registerData.setPassword(PASSWORD);
    newCustomer= new CustomerModel();
    BDDMockito.given(modelService.create(CustomerModel.class)).willReturn(newCustomer);
  }

  @Test
  public void shouldRegisterCustomer() throws DuplicateUidException {
    customerFacade.register(registerData);
  }

  @Test
  public void validateCustomerModel(){
    customerFacade.setCommonPropertiesForRegister(registerData,newCustomer);
    assertEquals(BlFacadesConstants.CUSTOMER,newCustomer.getName());
    assertEquals(EMAIL,newCustomer.getUid());
    assertEquals(EMAIL,newCustomer.getOriginalUid());
  }

  @Test
  public void shouldValidateDublicateUid() throws DuplicateUidException {
    BDDMockito.doThrow(new DuplicateUidException("Email has already present")).when(customerAccountService).register(newCustomer,PASSWORD);
    try {
      customerFacade.register(registerData);
    }catch (DuplicateUidException duplicateUidException){
      duplicateUidException.printStackTrace();
    }
  }
}
