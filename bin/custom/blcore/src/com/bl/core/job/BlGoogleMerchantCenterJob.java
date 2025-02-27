package com.bl.core.job;

import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.media.MediaService;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;

import com.bl.core.google.product.populators.BlGoogleProductFeedXmlPupulator;
import com.bl.core.model.BlGoogleMarketPlaceProductFeedModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.stock.BlStockService;
import com.bl.integration.marketplace.jaxb.Rss;


public class BlGoogleMerchantCenterJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlGoogleMerchantCenterJob.class);
	private BlGoogleProductFeedXmlPupulator blGoogleProductFeedXmlPupulator;
	private BlProductService productService;
	private static final String MIME_TYPE = "xml";
	private MediaService mediaService;
	private BlStockLevelDao blStockLevelDao;
	private BlStockService blStockService;

	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		final List<BlProductModel> blProducts = getProductService().getUsedProductsOnSale();
		if (!blProducts.isEmpty())
		{
			final Rss rss = new Rss();
			getBlGoogleProductFeedXmlPupulator().populate(blProducts, rss);
			try
			{
				convertToXML(rss);
			}
			catch (final FileNotFoundException e)
			{
				LOG.info("Exception occurred during converting to Xml " + e);
				return new PerformResult(CronJobResult.ERROR, CronJobStatus.ABORTED);
			}
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	private void convertToXML(final Object data) throws FileNotFoundException
	{
		try
		{
			final SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyyHH:mm:ss");
			final JAXBContext jaxbContext = JAXBContext.newInstance(data.getClass());
			final Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
			jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			final File productFeedFile = new File("Product_Feed");
			jaxbMarshaller.marshal(data, productFeedFile);
			final MediaModel media = createMediaModel(productFeedFile, formatter);
			final BlGoogleMarketPlaceProductFeedModel googleMarketPlaceFeed = modelService
					.create(BlGoogleMarketPlaceProductFeedModel.class);
			googleMarketPlaceFeed.setName("Product_Feed_XML_File_" + formatter.format(new Date()));
			googleMarketPlaceFeed.setProductFeedFile(media);
			modelService.save(googleMarketPlaceFeed);
		}
		catch (final JAXBException e)
		{
			LOG.info("Exception occurred during Marshalling Xml " + e);
		}
	}

	private MediaModel createMediaModel(final File productFeedFile, final SimpleDateFormat formatter)
	{
		final MediaModel media = modelService.create(MediaModel.class);
		media.setCode(productFeedFile.getName() + "_" + formatter.format(new Date()));
		modelService.save(media);

		try (InputStream productFeedInputStream = new FileInputStream(productFeedFile))
		{
			getMediaService().setStreamForMedia(media, productFeedInputStream, productFeedFile.getName(), MIME_TYPE);
		}
		catch (final IOException e)
		{
			LOG.info("Exception occurred during Media Stream " + e);
		}
		return media;
	}

	public BlGoogleProductFeedXmlPupulator getBlGoogleProductFeedXmlPupulator()
	{
		return blGoogleProductFeedXmlPupulator;
	}

	public void setBlGoogleProductFeedXmlPupulator(final BlGoogleProductFeedXmlPupulator blGoogleProductFeedXmlPupulator)
	{
		this.blGoogleProductFeedXmlPupulator = blGoogleProductFeedXmlPupulator;
	}

	public BlProductService getProductService()
	{
		return productService;
	}

	public void setProductService(final BlProductService productService)
	{
		this.productService = productService;
	}

	protected MediaService getMediaService()
	{
		return mediaService;
	}

	@Required
	public void setMediaService(final MediaService mediaService)
	{
		this.mediaService = mediaService;
	}

	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

	public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao)
	{
		this.blStockLevelDao = blStockLevelDao;
	}
	
	
	public BlStockService getBlStockService()
	{
		return blStockService;
	}

	public void setBlStockService(final BlStockService blStockService)
	{
		this.blStockService = blStockService;
	}

}
