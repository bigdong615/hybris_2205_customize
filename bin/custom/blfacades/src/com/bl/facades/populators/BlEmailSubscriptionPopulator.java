/**
 *
 */
package com.bl.facades.populators;

import java.util.ArrayList;
import java.util.List;

import com.bl.core.subscription.models.AttributeSet;
import com.bl.core.subscription.models.ContactRequest;
import com.bl.core.subscription.models.Item;
import com.bl.core.subscription.models.Value;


/**
 * BlEmailSubscriptionPopulator
 *
 * @author Sunil Sahu
 *
 */
public class BlEmailSubscriptionPopulator
{
	/**
	 * This method is created for populating values for create contact request
	 *
	 * @param email
	 *           id to be subscribed
	 * @param contactRequest
	 */
	public void poupulateContactRequest(final String emailId, final ContactRequest contactRequest)
	{
		contactRequest.setContactKey(emailId);
		final List<AttributeSet> attributeSets = new ArrayList<>();
		final AttributeSet attributeSet = new AttributeSet();
		attributeSet.setName("Email Addresses");

		final Item item = new Item();
		final List<Item> items = new ArrayList<>();
		final List<Value> values = new ArrayList<>();

		final Value emailValue = new Value();
		emailValue.setName("Email Address");
		emailValue.setValueData(emailId);

		final Value htmlValue = new Value();
		htmlValue.setName("HTML Enabled");
		htmlValue.setValueData(true);

		values.add(emailValue);
		values.add(htmlValue);

		item.setValues(values);
		items.add(item);

		attributeSet.setItems(items);
		attributeSets.add(attributeSet);
		contactRequest.setAttributeSets(attributeSets);
	}
}
