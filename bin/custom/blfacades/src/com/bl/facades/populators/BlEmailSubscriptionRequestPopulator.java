/**
 *
 */
package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.util.ArrayList;
import java.util.List;

import com.bl.core.subscription.models.AttributeSet;
import com.bl.core.subscription.models.ContactRequest;
import de.hybris.platform.converters.Populator;
import com.bl.core.subscription.models.Item;
import com.bl.core.subscription.models.Value;
import org.springframework.util.Assert;


/**
 * BlEmailSubscriptionPopulator
 *
 * @author Sunil Sahu
 */
public class BlEmailSubscriptionRequestPopulator implements Populator<String, ContactRequest> {

	/**
	 * Populate the contactRequest instance with values from the emailId.
	 *
	 * @param emailId        the source object
	 * @param contactRequest the target to fill
	 * @throws ConversionException if an error occurs
	 */
	@Override
	public void populate(final String emailId, final ContactRequest contactRequest)
			throws ConversionException {
		Assert.notNull(emailId, "Parameter emailId cannot be null.");
		Assert.notNull(contactRequest, "Parameter contactRequest cannot be null.");

		contactRequest.setContactKey(emailId);
		final List<AttributeSet> attributeSets = new ArrayList<>();
		final AttributeSet attributeSet = new AttributeSet();
		attributeSet.setName(BlCoreConstants.EMAIL_ADDRESSES);

		final Item item = new Item();
		final List<Item> items = new ArrayList<>();
		final List<Value> values = new ArrayList<>();

		final Value emailValue = new Value();
		emailValue.setName(BlCoreConstants.EMAIL_ADDRESS);
		emailValue.setValue(emailId);

		final Value htmlValue = new Value();
		htmlValue.setName(BlCoreConstants.HTML_ENABLED);
		htmlValue.setValue(true);

		values.add(emailValue);
		values.add(htmlValue);

		item.setValues(values);
		items.add(item);

		attributeSet.setItems(items);
		attributeSets.add(attributeSet);
		contactRequest.setAttributeSets(attributeSets);
	}

}
