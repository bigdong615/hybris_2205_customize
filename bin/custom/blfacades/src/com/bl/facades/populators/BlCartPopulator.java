package com.bl.facades.populators;

import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.converters.populator.CartPopulator;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Extended OOTB CartPopulator to populate custom attributes
 *
 * @author Ravikumar
 *
 */
public class BlCartPopulator extends CartPopulator<CartData>
{

  private static final Logger LOG = Logger.getLogger(BlCartPopulator.class);

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void populate(final CartModel source, final CartData target)
	{
		super.populate(source, target);
		target.setTotalDamageWaiverCost(createPrice(source, source.getTotalDamageWaiverCost()));
		target.setPickUpPersonFirstName(source.getPickUpPersonFirstName());
		target.setPickUpPersonLastName(source.getPickUpPersonLastName());
		target.setPickUpPersonEmail(source.getPickUpPersonEmail());
		target.setPickUpPersonPhone(source.getPickUpPersonPhone());
		target.setTaxAvalaraCalculated(createPrice(source , source.getTotalAvalaraTaxCalculated()));
	}

	/**
	 * This method overridden to calculate the totalPrice with tax
	 */
	@Override
  protected Double calcTotalWithTax(final AbstractOrderModel source)
  {
    if (source == null)
    {
      BlLogger.logMessage(LOG , Level.ERROR , "source order must not be null" , new IllegalArgumentException());
    }
    // Since we have already calculated the total with Tax , so returning cart total as total price with tax
    return null != source && source.getTotalPrice() != null ? source.getTotalPrice() : 0.0d;
  }
}
