package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.forgotPassword.ForgotPasswordRequiredEventRequest;
import com.bl.esp.dto.forgotPassword.data.ForgotPasswordRequestData;
import de.hybris.platform.converters.Populator;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.springframework.beans.factory.annotation.Value;

public class BlForgotPasswordRequestPopulator implements
    Populator<ForgotPasswordRequestData, ForgotPasswordRequiredEventRequest> {

  @Value("${borrow.lenses.subscriber.id}")
  private String subscriberId;

  @Value("${forgot.password.request.event.definition.key}")
  private String eventDefinitionKey;

  @Value("${forgot.password.request.event.template.key}")
  private String template;

  @Override
  public void populate(final ForgotPasswordRequestData forgotPasswordRequestData, final ForgotPasswordRequiredEventRequest passwordRequiredEventRequest)
       {
    forgotPasswordRequestData.setSubscriberid(subscriberId);
    forgotPasswordRequestData.setTemplate(template);
    final SimpleDateFormat formatTime = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    forgotPasswordRequestData.setRequestedDate(formatTime.format(new Date()));
    passwordRequiredEventRequest.setData(forgotPasswordRequestData);
    passwordRequiredEventRequest.setContactKey(forgotPasswordRequestData.getEmailAddress());
    passwordRequiredEventRequest.setEventDefinitionKey(eventDefinitionKey);
  }
}
