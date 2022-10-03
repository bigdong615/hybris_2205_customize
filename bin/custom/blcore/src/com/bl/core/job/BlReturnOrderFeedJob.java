package com.bl.core.job;

import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.order.ReturnOrderData;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.media.MediaService;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.log4j.Logger;

import com.bl.core.model.BlReturnOrderFeedModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.order.populators.BlReturnOrderPopulator;
import com.opencsv.bean.ColumnPositionMappingStrategy;
import com.opencsv.bean.StatefulBeanToCsv;
import com.opencsv.bean.StatefulBeanToCsvBuilder;
import com.opencsv.exceptions.CsvDataTypeMismatchException;
import com.opencsv.exceptions.CsvRequiredFieldEmptyException;


public class BlReturnOrderFeedJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlReturnOrderFeedJob.class);
	private BlReturnOrderPopulator blReturnOrderPopulator;
	private BlOrderDao orderDao;
	private static final String MIME_TYPE = "csv";
	private MediaService mediaService;

	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		final List<OrderModel> orderModelList = getOrderDao().getOrdersReadyForReturn();
		if (!orderModelList.isEmpty())
		{
			final List<ReturnOrderData> returnOrderList = new ArrayList<ReturnOrderData>();
			getBlReturnOrderPopulator().populate(orderModelList, returnOrderList);
			final File returnOrderFeed = new File("Reurn_Order_Feed.csv");
			FileWriter writer = null;

			final ColumnPositionMappingStrategy mappingStrategy = new ColumnPositionMappingStrategy();
			mappingStrategy.setType(ReturnOrderData.class);

			final String[] columns = new String[]
			{ "CustomerName", "Email", "OrderNumber", "OrderPlacedDate", "OrderDate", "ReturnOrderDate" };
			mappingStrategy.setColumnMapping(columns);

			final StatefulBeanToCsvBuilder<ReturnOrderData> builder = new StatefulBeanToCsvBuilder(writer);
			final StatefulBeanToCsv beanWriter = builder.withMappingStrategy(mappingStrategy).build();
			try
			{
				writer = new FileWriter(returnOrderFeed);
				beanWriter.write(returnOrderList);
				writer.close();
			}
			catch (final CsvDataTypeMismatchException | CsvRequiredFieldEmptyException | IOException e)
			{
				e.printStackTrace();
				LOG.info("Exception occurred during converting to Xml");
				return new PerformResult(CronJobResult.ERROR, CronJobStatus.ABORTED);
			}
			final SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyyHH:mm:ss");
			final MediaModel media = createMediaModel(returnOrderFeed, formatter);
			final BlReturnOrderFeedModel returnOrderFeedModel = modelService.create(BlReturnOrderFeedModel.class);
			returnOrderFeedModel.setName("Reurn_Order_Feed_" + formatter.format(new Date()));
			returnOrderFeedModel.setOrderFeedFile(media);
			modelService.save(returnOrderFeedModel);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	private MediaModel createMediaModel(final File returnOrderFeedFile, final SimpleDateFormat formatter)
	{
		final MediaModel media = modelService.create(MediaModel.class);
		media.setCode(returnOrderFeedFile.getName() + "_" + formatter.format(new Date()));
		modelService.save(media);

		try (InputStream returnOrderFeedInputStream = new FileInputStream(returnOrderFeedFile))
		{
			getMediaService().setStreamForMedia(media, returnOrderFeedInputStream, returnOrderFeedFile.getName(), MIME_TYPE);
		}
		catch (final IOException e)
		{
			LOG.info("Exception occurred during Media Stream " + e);
		}
		return media;
	}

	public BlReturnOrderPopulator getBlReturnOrderPopulator()
	{
		return blReturnOrderPopulator;
	}

	public void setBlReturnOrderPopulator(final BlReturnOrderPopulator blReturnOrderPopulator)
	{
		this.blReturnOrderPopulator = blReturnOrderPopulator;
	}

	protected MediaService getMediaService()
	{
		return mediaService;
	}

	public void setMediaService(final MediaService mediaService)
	{
		this.mediaService = mediaService;
	}

	public BlOrderDao getOrderDao()
	{
		return orderDao;
	}

	public void setOrderDao(final BlOrderDao orderDao)
	{
		this.orderDao = orderDao;
	}

}
