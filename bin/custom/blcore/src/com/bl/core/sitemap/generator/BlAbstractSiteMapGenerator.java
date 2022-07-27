/**
 *
 */
package com.bl.core.sitemap.generator;

import de.hybris.platform.acceleratorservices.enums.SiteMapPageEnum;
import de.hybris.platform.acceleratorservices.sitemap.data.SiteMapUrlData;
import de.hybris.platform.acceleratorservices.sitemap.generator.SiteMapGenerator;
import de.hybris.platform.acceleratorservices.sitemap.renderer.SiteMapContext;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.cms2.model.site.CMSSiteModel;
import de.hybris.platform.commerceservices.impersonation.ImpersonationContext;
import de.hybris.platform.commerceservices.impersonation.ImpersonationService;
import de.hybris.platform.commons.model.renderer.RendererTemplateModel;
import de.hybris.platform.commons.renderer.RendererService;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;


/**
 * @author srinivas
 *
 */
public abstract class BlAbstractSiteMapGenerator<T> implements SiteMapGenerator<T>, ApplicationContextAware
{
	private static final Logger LOG = Logger.getLogger(BlAbstractSiteMapGenerator.class);
	private ImpersonationService impersonationService;
	private RendererService rendererService;
	private Converter<T, SiteMapUrlData> siteMapUrlDataConverter;
	private ApplicationContext applicationContext;
	private CommonI18NService commonI18NService;
	private FlexibleSearchService flexibleSearchService;
	private SiteMapPageEnum siteMapPageEnum;
	private CatalogVersionService catalogVersionService;
	private static final String SITE_MAP = "sitemap";
	private static final String HOMEPAGE = "Homepage";
	private static final String UNDERSCORE = "_";

	public List<T> getData(final CMSSiteModel site)
	{
		final ImpersonationContext context = new ImpersonationContext();
		context.setSite(site);
		context.setCatalogVersions(getCatalogVersionService().getSessionCatalogVersions());

		return getImpersonationService().executeInContext(context, () -> getDataInternal(site));
	}

	public File render(final CMSSiteModel site, final CurrencyModel currencyModel, final LanguageModel languageModel,
			final RendererTemplateModel rendererTemplateModel, final List<T> models,
			final String pageType,
			final Integer index)
			throws IOException
	{
		final File siteMap = File.createTempFile(SITE_MAP + pageType + UNDERSCORE, ".xml");
		final List<MediaModel> files = (List) site.getSiteMaps();
		final MediaService mediaService = (MediaService) applicationContext.getBean("mediaService");
		final ModelService modelService = (ModelService) applicationContext.getBean("modelService");
		if (!files.isEmpty() && !pageType.equalsIgnoreCase(HOMEPAGE))
		{
			for (final MediaModel file : files)
			{
				if (file.getRealFileName().contains(SITE_MAP))
				{
					LOG.debug("Real File Name" + file.getRealFileName());
					final InputStream inputStream = mediaService.getStreamFromMedia(file);

					try (OutputStream output = new FileOutputStream(siteMap))
					{
						FileUtils.copyInputStreamToFile(inputStream, siteMap);
					}
					catch (final IOException ioException)
					{
						LOG.error("Error while copy file" + ioException.getMessage());
					}
				}
			}
		}
		final ImpersonationContext context = new ImpersonationContext();
		context.setSite(site);
		context.setCurrency(currencyModel);
		context.setLanguage(languageModel);

		return getImpersonationService().executeInContext(context, () -> {
			final List<SiteMapUrlData> siteMapUrlDataList = getSiteMapUrlData(models);
			final SiteMapContext siteMapContext = (SiteMapContext) applicationContext.getBean("siteMapContext");
			siteMapContext.init(site, getSiteMapPageEnum());
			siteMapContext.setSiteMapUrlData(siteMapUrlDataList);
			final BufferedWriter output = new BufferedWriter(new FileWriter(siteMap, true));
			final PrintWriter finalOutput = new PrintWriter(output);
			try
			{
				// the template media is loaded only for english language.
				getCommonI18NService().setCurrentLanguage(getCommonI18NService().getLanguage("en"));
				getRendererService().render(rendererTemplateModel, siteMapContext, finalOutput);
			}
			finally
			{
				IOUtils.closeQuietly(output);
			}
			return siteMap;
		});
	}

	public abstract List<SiteMapUrlData> getSiteMapUrlData(List<T> models);

	protected <T> List<T> doSearch(final String query, final Map<String, Object> params, final Class<T> resultClass)
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(query);
		if (params != null)
		{
			fQuery.addQueryParameters(params);
		}

		fQuery.setResultClassList(Collections.singletonList(resultClass));

		final SearchResult<T> searchResult = getFlexibleSearchService().search(fQuery);
		return searchResult.getResult();
	}

	protected abstract List<T> getDataInternal(final CMSSiteModel siteModel);


	public ImpersonationService getImpersonationService()
	{
		return impersonationService;
	}

	@Required
	public void setImpersonationService(final ImpersonationService impersonationService)
	{
		this.impersonationService = impersonationService;
	}

	public RendererService getRendererService()
	{
		return rendererService;
	}

	@Required
	public void setRendererService(final RendererService rendererService)
	{
		this.rendererService = rendererService;
	}

	public Converter<T, SiteMapUrlData> getSiteMapUrlDataConverter()
	{
		return siteMapUrlDataConverter;
	}

	@Required
	public void setSiteMapUrlDataConverter(final Converter<T, SiteMapUrlData> siteMapUrlDataConverter)
	{
		this.siteMapUrlDataConverter = siteMapUrlDataConverter;
	}

	@Override
	public void setApplicationContext(final ApplicationContext applicationContext) throws BeansException
	{
		this.applicationContext = applicationContext;
	}

	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	@Required
	public void setCommonI18NService(final CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
	}

	public FlexibleSearchService getFlexibleSearchService()
	{
		return flexibleSearchService;
	}

	@Required
	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
	{
		this.flexibleSearchService = flexibleSearchService;
	}

	public SiteMapPageEnum getSiteMapPageEnum()
	{
		return siteMapPageEnum;
	}

	@Required
	public void setSiteMapPageEnum(final SiteMapPageEnum siteMapPageEnum)
	{
		this.siteMapPageEnum = siteMapPageEnum;
	}

	public CatalogVersionService getCatalogVersionService()
	{
		return catalogVersionService;
	}

	@Required
	public void setCatalogVersionService(final CatalogVersionService catalogVersionService)
	{
		this.catalogVersionService = catalogVersionService;
	}

}
