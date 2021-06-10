package com.braintree.commands;

import com.braintreegateway.WebhookNotification;
import de.hybris.platform.braintree.data.BrainTreeWebhookNotificationRequest;
import de.hybris.platform.payment.commands.Command;

import java.util.Map;


public interface BrainTreeWebhookNotificationCommand extends Command<BrainTreeWebhookNotificationRequest, WebhookNotification>
{
}
