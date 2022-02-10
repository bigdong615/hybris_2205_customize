package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.common.ESPEmailCommonEventRequest;
import com.bl.esp.dto.common.data.ESPEmailCommonRequestData;
import com.bl.esp.dto.forgotPassword.data.ForgotPasswordRequestData;
import com.bl.esp.dto.notify.data.NotifyMeEmailRequestData;
import de.hybris.platform.converters.Populator;
import java.text.SimpleDateFormat;
import java.util.Date;
import org.springframework.beans.factory.annotation.Value;

/**
 * BL-1813,1814,BL-1815:This populator created to populate reset password, notify me and back in stock email related common data.
 * @author vijay vishwakarma
 */
public class BlESPEmailCommonRequestPopulator implements
    Populator<ESPEmailCommonRequestData, ESPEmailCommonEventRequest> {

  @Value("${borrow.lenses.subscriber.id}")
  private String subscriberId;

  @Value("${notify.me.email.request.event.definition.key}")
  private String notifyMeEventDefinitionKey;

  @Value("${notify.me.email.request.event.template.key}")
  private String notifyMeTemplate;

  @Value("${forgot.password.request.event.definition.key}")
  private String resetPasswordEventDefinitionKey;

  @Value("${forgot.password.request.event.template.key}")
  private String resetPasswordTemplate;

  @Override
  public void populate(final ESPEmailCommonRequestData emailRequestData,
      final ESPEmailCommonEventRequest emailRequiredESPEventRequest)
       {
         emailRequestData.setSubscriberid(subscriberId);
         final SimpleDateFormat formatTime = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
         emailRequestData.setRequestedDate(formatTime.format(new Date()));
         emailRequiredESPEventRequest.setData(emailRequestData);
         emailRequiredESPEventRequest.setContactKey(emailRequestData.getEmailAddress());
         emailRequestData.setEmailAddress(subscriberId);
          if(emailRequestData instanceof NotifyMeEmailRequestData) {
            emailRequestData.setTemplate(notifyMeTemplate);
            emailRequiredESPEventRequest.setEventDefinitionKey(notifyMeEventDefinitionKey);
          }
          if(emailRequestData instanceof ForgotPasswordRequestData){
            emailRequestData.setTemplate(resetPasswordTemplate);
            emailRequiredESPEventRequest.setEventDefinitionKey(resetPasswordEventDefinitionKey);
          }
      }
}
