package com.bl.batch.imports.translator;

import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.category.CategoryService;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.core.Registry;
import de.hybris.platform.impex.jalo.translators.AbstractValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;


public class BlSuperCategoryTranslator extends AbstractValueTranslator
{

	private static final Logger LOG = Logger.getLogger(BlSuperCategoryTranslator.class);

	private static final String CATALOG_ID = "blProductCatalog";
	private static final String CATALOG_VERSION_NAME = "Staged";
	private static final String CATEGORY_SERVICE_BEAN_ID = "categoryService";
	private static final String CATALOG_VERSION_SERVICE = "catalogVersionService";
	private static final String DEFAULT_SUPER_CATEGORY = "rentalgear";

	private static final CatalogVersionService catalogVersionService;
	private static final CategoryService categoryService;

	static
	{
		catalogVersionService = (CatalogVersionService) getServiceBean(CATALOG_VERSION_SERVICE);
		categoryService = (CategoryService) getServiceBean(CATEGORY_SERVICE_BEAN_ID);
	}

	@Override
	public Object importValue(final String value, final Item item) throws JaloInvalidParameterException
	{
		CategoryModel superCategoryValue = null;
		final CatalogVersionModel catalogVersion = catalogVersionService.getCatalogVersion(CATALOG_ID,
				CATALOG_VERSION_NAME);

		final List<CategoryModel> categoryList = new ArrayList<CategoryModel>();

		try
		{
			if (StringUtils.isNotEmpty(value))
			{
				superCategoryValue = categoryService.getCategoryForCode(catalogVersion, value);
			}
			else
			{
				superCategoryValue = categoryService.getCategoryForCode(catalogVersion, DEFAULT_SUPER_CATEGORY);
			}
		}
		catch (UnknownIdentifierException | AmbiguousIdentifierException exception)
		{
			LOG.warn("Exception occured to get super category :" + value + ":" + exception);
		}
		categoryList.add(superCategoryValue);
		return categoryList;
	}

	@Override
	public String exportValue(final Object value) throws JaloInvalidParameterException {
		return value == null ? "" : value.toString();
	}

	/**
	 * Get the bean instance from application context
	 *
	 * @param beanID
	 * @return Bean instance
	 */
	private static Object getServiceBean(final String beanID)
	{
		return Registry.getApplicationContext().getBean(beanID);
	}
}
