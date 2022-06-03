package com.bl.batch.imports.translator;

import de.hybris.platform.catalog.model.classification.ClassificationClassModel;
import de.hybris.platform.classification.ClassificationService;
import de.hybris.platform.classification.features.Feature;
import de.hybris.platform.classification.features.FeatureList;
import de.hybris.platform.classification.features.FeatureValue;
import de.hybris.platform.classification.features.LocalizedFeature;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.impex.jalo.translators.AbstractSpecialValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

import org.apache.commons.lang.ArrayUtils;
import org.apache.log4j.Logger;



public class BlProductClassFeatureTranslator extends AbstractSpecialValueTranslator
{
	private static final ModelService modelService;
	private static final ClassificationService classificationService;
	private static final EnumerationService enumerationService;
	private static final I18NService i18nService;
	private static final Logger LOG = Logger.getLogger(BlProductClassFeatureTranslator.class);
	private static final String SYSTEM_VERSION = "blClassification/1.0/";
	private static final CommonI18NService commonI18NService;
	private static final String CAP_DELIMITER = "\\^";
	private static final String COLON_DELIMITER = ":";

	static
	{
		modelService = (ModelService) getServiceBean("modelService");
		classificationService = (ClassificationService) getServiceBean("classificationService");
		commonI18NService = (CommonI18NService) getServiceBean("commonI18NService");
		enumerationService = (EnumerationService) getServiceBean("enumerationService");
		i18nService = (I18NService) getServiceBean("i18nService");
	}

	@Override
	public void performImport(final String cellValue, final Item item) throws JaloInvalidParameterException
	{

		final ProductModel product = (ProductModel) modelService.get(item);
		final Pattern ptr = Pattern.compile(CAP_DELIMITER);
		final String[] productFeatureContainersList = ptr.split(cellValue);
		commonI18NService.setCurrentLanguage(commonI18NService.getLanguage("en"));
		final FeatureList existingFeaturesList = classificationService.getFeatures(product);
		classificationService.replaceFeatures(product, existingFeaturesList);
		final List<Feature> newFeatureList = new ArrayList<>();
		FeatureList updatedFeaturesList = null;
		Feature newProductFeature = null;
		if (!existingFeaturesList.isEmpty() && ArrayUtils.isNotEmpty(productFeatureContainersList))
		{
			for (final String feature : productFeatureContainersList)
			{
				final String[] featureContainersList = feature.split(COLON_DELIMITER);
				if (ArrayUtils.isNotEmpty(featureContainersList) && product.getClassificationClasses() != null)
				{
					newProductFeature = getReplacedFeatureList(product, existingFeaturesList, featureContainersList);
					if (newProductFeature != null)
					{
						newFeatureList.add(newProductFeature);
					}
				}
			}
			updatedFeaturesList = new FeatureList(newFeatureList);
			classificationService.replaceFeatures(product, updatedFeaturesList);
			LOG.debug(" Final complete product features " + classificationService.getFeatures(product));
		}
	}

	private Feature getReplacedFeatureList(final ProductModel product, final FeatureList existingFeaturesList,
			final String[] featureContainersList)
	{
		String attributeAuthorCode = null;
		Feature productFeature = null;
		for (final ClassificationClassModel productClass : product.getClassificationClasses())
		{
			attributeAuthorCode = SYSTEM_VERSION + productClass.getCode() + "." + featureContainersList[0].toLowerCase();
			productFeature = existingFeaturesList.getFeatureByCode(attributeAuthorCode);
			if (productFeature != null)
			{
				productFeature.removeAllValues();
				if (productFeature instanceof LocalizedFeature)
				{
					final Locale localeEn = new Locale("en");

					if (featureContainersList[1].contains(","))
					{
						final String[] featuresValues = splitProductFeature(featureContainersList[1]);
						for (final String value : featuresValues)
						{
							((LocalizedFeature) productFeature).addValue(new FeatureValue(value), localeEn);
						}
					}
					else
					{
						((LocalizedFeature) productFeature).addValue(new FeatureValue(featureContainersList[1]), localeEn);
					}
					LOG.info(" Final individual product feature " + productFeature.getName());
					return productFeature;
				}
			}
		}
		return productFeature;
	}



	private String[] splitProductFeature(final String featureContainersList)
	{
		if (featureContainersList.contains(","))
		{
			final String[] featureContainer = featureContainersList.split(",");
			return featureContainer;
		}
		return null;
	}

	private static Object getServiceBean(final String beanID)
	{
		return Registry.getApplicationContext().getBean(beanID);
	}
}
