package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.forgotPassword.ForgotPasswordRequiredEventRequest;
import com.bl.esp.dto.forgotPassword.data.ForgotPasswordRequestData;
//import com.bl.facades.process.email.context.ForgottenPasswordEmailContext;
import de.hybris.platform.commerceservices.model.process.ForgottenPasswordProcessModel;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.Date;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

public class BlForgotPasswordRequestPopulator implements
    Populator<ForgottenPasswordProcessModel, ForgotPasswordRequiredEventRequest> {

 /* @Autowired
  ForgottenPasswordEmailContext forgottenPasswordEmailContext;
*/
  @Value("${forgotPassword.link.expiry.time}")
  private String passwordExpireTime;

  @Override
  public void populate(final ForgottenPasswordProcessModel passwordProcessModel, final ForgotPasswordRequiredEventRequest passwordRequiredEventRequest)
      throws ConversionException {
    final CustomerModel customer = passwordProcessModel.getCustomer();
    customer.getUid();
    ForgotPasswordRequestData forgotPasswordRequestData= new ForgotPasswordRequestData();
    forgotPasswordRequestData.setSubscriberid("help@borrowlenses.com");
    forgotPasswordRequestData.setEmailAddress(customer.getUid());
    forgotPasswordRequestData.setTemplate("lostpass_request");
    forgotPasswordRequestData.setPasswordLink("/login/pw/change?token=VwcDNnsOd9uG110A%2FFrbodPqjz%2F9Va4HQRpkuPU3IJJzJ1NbIYzckCYh6%2FoTHavuyjcEmnnzXJRXGEdAZELFv39EoNlmXcq3f%2BQr%2FtXDiYXtPzU0zwrZztmA%2BYXSPZok42812D5O2fdeIetmVAD8wPPRXupAwgA9QGBTkJM%3D");
    //forgotPasswordRequestData.setPasswordLink("https://accstorefront.crcndecc9r-shutterfl1-s1-public.model-t.cc.commerce.ondemand.com/login/pw/change?token=UpCMK7bD6SjgLvfA3k2y%2Fn5mZ1vY%2BiUssjXjkLswZDW4IP56Oa3%2FhOw6%2BL6kl9JA8eA2Q2H0HEUDoVD%2FANvqXhNpOmjXiCU3mfK1czq2OHcR4y%2FCU6NdxzZjS9r8kd5DzpdeGttAmAdq6xuPHQZ9MN%2B7W54qAsUZ7D21SJ11aQ%3D%3D");
    Date date = new Date();
    //SimpleDateFormat formatTime = new SimpleDateFormat("MMM dd, YYYY hh.mm aa");
    final SimpleDateFormat formatTime = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    String time = formatTime.format(date);
System.out.println(passwordProcessModel.getToken());
    forgotPasswordRequestData.setRequestedDate(time);
    forgotPasswordRequestData.setTimeout(30);
    passwordRequiredEventRequest.setData(forgotPasswordRequestData);
    passwordRequiredEventRequest.setContactKey(customer.getUid());
    passwordRequiredEventRequest.setEventDefinitionKey("APIEvent-c4c02ed5-f27f-f20e-8568-08789f47637e");

  }



}
