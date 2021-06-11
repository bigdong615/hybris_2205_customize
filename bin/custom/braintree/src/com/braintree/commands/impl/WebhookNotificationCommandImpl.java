package com.braintree.commands.impl;

import com.braintree.commands.BrainTreeWebhookNotificationCommand;
import com.braintreegateway.WebhookNotification;
import de.hybris.platform.braintree.data.BrainTreeWebhookNotificationRequest;

import java.util.Map;


public class WebhookNotificationCommandImpl extends AbstractCommand implements BrainTreeWebhookNotificationCommand
{
	@Override public WebhookNotification perform(BrainTreeWebhookNotificationRequest webhookNotificationRequest)
	{
		WebhookNotification webhookNotification = getBraintreeGateway().webhookNotification()
				.parse(webhookNotificationRequest.getBtSignature(), webhookNotificationRequest.getBtPayload());
		return webhookNotification;
	}
}
