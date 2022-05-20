package com.bl.batch.imports.translator;

import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.KeywordModel;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.impex.jalo.translators.AbstractValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;


public class BlKeywordsTranslator extends AbstractValueTranslator
{

	private static final Logger LOG = Logger.getLogger(BlKeywordsTranslator.class);
	private static final String CATALOG_ID = "blProductCatalog";
	private static final String CATALOG_VERSION_ID = "Staged";
	private static final String LANGUAGE = "en";

	private static ModelService modelService;
	private static CatalogVersionService catalogVersionService;
	private static CommonI18NService commonI18nService;
	private static final FlexibleSearchService flexibleSearchService;
	static
	{
		modelService = (ModelService) getServiceBean("modelService");
		commonI18nService = (CommonI18NService) getServiceBean("commonI18NService");
		catalogVersionService = (CatalogVersionService) getServiceBean("catalogVersionService");
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
		final List<KeywordModel> keywordModelList = new ArrayList<>();
		final CatalogVersionModel catalogVersionModel = catalogVersionService.getCatalogVersion(CATALOG_ID, CATALOG_VERSION_ID);
		final LanguageModel language = commonI18nService.getLanguage(LANGUAGE);
		final String[] keywords = value.split(",");
		for (final String keyword : keywords)
		{

			final KeywordModel keywordModel = new KeywordModel();
			keywordModel.setCatalogVersion(catalogVersionModel);
			keywordModel.setKeyword(keyword);
			keywordModel.setLanguage(language);
			try
			{
				final KeywordModel modelExists = flexibleSearchService.getModelByExample(keywordModel);
				if (modelExists != null)
				{
					keywordModelList.add(modelExists);
				}
			}
			catch (final ModelNotFoundException mfe)
			{
				try
				{
					modelService.save(keywordModel);
					keywordModelList.add(keywordModel);
				}
				catch (final ModelSavingException e)
				{
					LOG.error("Error while saving keywordModel" + e.getMessage());
				}
			}
			catch (final Exception e)
			{
				LOG.error("Error keywordModel" + e.getMessage());
			}

		}
		return keywordModelList;

	}

	private static Object getServiceBean(final String beanID)
	{
		return Registry.getApplicationContext().getBean(beanID);
	}

}