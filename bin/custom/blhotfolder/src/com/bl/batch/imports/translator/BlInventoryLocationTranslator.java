package com.bl.batch.imports.translator;

import de.hybris.platform.core.Registry;
import de.hybris.platform.impex.jalo.translators.AbstractValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import org.apache.log4j.Logger;

import com.bl.core.model.BlInventoryLocationModel;


public class BlInventoryLocationTranslator extends AbstractValueTranslator
{

	private static final Logger LOG = Logger.getLogger(BlInventoryLocationTranslator.class);
	private static ModelService modelService;
	private static final FlexibleSearchService flexibleSearchService;
	static
	{
		flexibleSearchService = (FlexibleSearchService) getServiceBean("flexibleSearchService");
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String exportValue(final Object value) throws JaloInvalidParameterException
	{
		return value == null ? "" : value.toString();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Object importValue(final String value, final Item item) throws JaloInvalidParameterException
	{
		final BlInventoryLocationModel inventoryLocModel = new BlInventoryLocationModel();
		inventoryLocModel.setInventoryLocationID(value);
			try
			{
				final BlInventoryLocationModel inventoryLoc = flexibleSearchService.getModelByExample(inventoryLocModel);
				if (inventoryLoc != null)
				{
					return inventoryLoc;
				}
			}
			catch (final ModelNotFoundException mfe)
			{
				LOG.error("Inventory Location not found" + mfe.getMessage());
			}
			catch (final Exception e)
			{
				LOG.error("Error in Inventory location model" + e.getMessage());
			}
			return null;
	}

	private static Object getServiceBean(final String beanID)
	{
		return Registry.getApplicationContext().getBean(beanID);
	}

}