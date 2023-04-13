package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.data.ConsignmentData;
import de.hybris.platform.consignmenttrackingfacades.populators.ConsignmentForTrackingPopulator;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import org.apache.log4j.Logger;


/**
 *
 */
public class BlConsignmentForTrackingPopulator extends ConsignmentForTrackingPopulator
{
	private static final Logger LOG = Logger.getLogger(BlConsignmentForTrackingPopulator.class);

	@Override
	public void populate(final ConsignmentModel source, final ConsignmentData target)
	{
		if (source.getOrder() != null)
		{
			super.populate(source, target);
		}

	}
}
