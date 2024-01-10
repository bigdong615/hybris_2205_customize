/**
 *
 */
package com.bl.core.job;

import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.util.Config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;

import com.bl.core.esp.service.impl.DefaultBlOrderFeedFTPService;
import com.bl.core.model.BlCompletedOrderFeedCronJobModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.order.populators.BlCompletedOrderPopulator;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.logging.BlLogger;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;
import com.opencsv.bean.ColumnPositionMappingStrategy;
import com.opencsv.bean.StatefulBeanToCsv;
import com.opencsv.bean.StatefulBeanToCsvBuilder;
import com.opencsv.exceptions.CsvDataTypeMismatchException;
import com.opencsv.exceptions.CsvRequiredFieldEmptyException;

/**
 * @author kumar
 *
 */
public class BlCompletedOrderExportJob extends AbstractJobPerformable<BlCompletedOrderFeedCronJobModel>
{

	private static final Logger LOG = Logger.getLogger(BlCompletedOrderExportJob.class);
	private static final String MIME_TYPE = "xml";
	private BlOrderDao orderDao;
	private DefaultBlOrderFeedFTPService defaultBlOrderFeedFTPService;
	private MediaService mediaService;
	public static final String COMPLETED_ORDER_MEDIA_FILE_NAME = "BlCompletedOrderFeedCronJob";

	private BlCompletedOrderPopulator blCompletedOrderPopulator;

	@Override
	public PerformResult perform(final BlCompletedOrderFeedCronJobModel cronjob)
	{
		List<OrderModel> orderModelList = null;
		final SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyyHH:mm:ss");

		try {
			if (cronjob.getFullLoad())
			{
				orderModelList = getOrderDao().getCompletedOrders();
			}
			else
			{
				orderModelList = getOrderDao().getCompletedOrdersForLastDay();

			}
		}
		catch (final Exception e)
		{
			LOG.info("Exception occurred during fetching return order models");
			e.printStackTrace();
		}
		if (orderModelList != null && !orderModelList.isEmpty())
		{
			final List<OrderData> orderList = new ArrayList<OrderData>();
			getBlCompletedOrderPopulator().populate(orderModelList, orderList);
			LOG.info("order data" + orderList.get(0).getEmail());
			final File completedOrderFeed = getFile();
			FileWriter writer = null;
			try
			{
				writer = new FileWriter(completedOrderFeed);
			}
			catch (final IOException e1)
			{
				LOG.info("Exception occurred during creation of Writer");
				e1.printStackTrace();
			}

			final ColumnPositionMappingStrategy mappingStrategy = new ColumnPositionMappingStrategy();
			mappingStrategy.setType(OrderData.class);
			final String[] columns = new String[]
			{ "page_id", "order_id", "first_name", "last_name", "email", "order_date", "locale" };
			mappingStrategy.setColumnMapping(columns);
			final StatefulBeanToCsvBuilder<OrderData> builder = new StatefulBeanToCsvBuilder(writer);
			final StatefulBeanToCsv beanWriter = builder.withMappingStrategy(mappingStrategy).build();
			try
			{
				writer.append("page_id, order_id, first_name, last_name, email, order_date, locale");
				writer.append("\n");
				beanWriter.write(orderList);
				writer.close();
			}
			catch (final CsvDataTypeMismatchException | CsvRequiredFieldEmptyException | IOException e)
			{
				e.printStackTrace();
				LOG.info("Exception occurred during converting to CSV");
				return new PerformResult(CronJobResult.ERROR, CronJobStatus.ABORTED);
			}
			final MediaModel media = createMediaModel(completedOrderFeed, formatter);

			sendFileToFTPLocation(completedOrderFeed);
			return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);

		}


		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	private MediaModel createMediaModel(final File productFeedFile, final SimpleDateFormat formatter)
	{
		final MediaModel media = modelService.create(MediaModel.class);
		final String logFileName = new SimpleDateFormat(BlespintegrationConstants.COMPLETED_FILE_FORMAT).format(new Date());
		media.setCode(COMPLETED_ORDER_MEDIA_FILE_NAME + "_" + logFileName);
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
	private void sendFileToFTPLocation(final File file)
	{
		Session session = null;
		Channel channel = null;
		ChannelSftp channelSftp = null;
		try
		{
			final JSch jsch = new JSch();
			session = jsch.getSession(Config.getParameter(BlespintegrationConstants.ORDERSFTPUSER),
					Config.getParameter(BlespintegrationConstants.ORDERSFTPHOST),
					Config.getInt(BlespintegrationConstants.SFTPPORT, 22));
			session.setPassword(Config.getParameter(BlespintegrationConstants.ORDERSFTPPASS));
			final Properties config = new Properties();
			config.put(BlespintegrationConstants.STICT_HOST_KEY, BlespintegrationConstants.NO);
			session.setConfig(config);
			session.connect();
			channel = session.openChannel(BlespintegrationConstants.SFTP);
			channel.connect();
			channelSftp = (ChannelSftp) channel;
			channelSftp.cd(Config.getParameter(BlespintegrationConstants.ORDER_CLIENT_FTP_PATH));
			final File f = new File(file.getAbsolutePath());
			try (FileInputStream fileInputStream = new FileInputStream(f))
			{
				channelSftp.put(fileInputStream, f.getName());
			}
		}
		catch (JSchException | SftpException | IOException ex)
		{
			LOG.info(LOG);
		}
		finally
		{
			if (null != channelSftp)
			{
				channelSftp.disconnect();
				channelSftp.exit();
			}
			if (null != channel)
			{
				channel.disconnect();
			}
			if (null != session)
			{
				session.disconnect();
			}
		}
		if (file.exists())
		{
			file.delete();
		}
	}

	private File getFile()
	{
		final String logFileName = new SimpleDateFormat(BlespintegrationConstants.COMPLETED_FILE_FORMAT).format(new Date());
		final String fileName = new StringBuilder(BlespintegrationConstants.COMPLETED_ORDER_FILE_NAME_PREFIX).append(logFileName)
				.append(BlespintegrationConstants.RETURN_ORDER_FILE_SUFFIX).toString();
		final String path = Config.getParameter(BlespintegrationConstants.LOCAL_FTP_PATH);
		createDirectoryForFTPFeed(path);
		LOG.info("Completed order File name " + fileName);
		return new File(new StringBuilder(path).append(BlespintegrationConstants.SLASH).append(fileName).toString());
	}

	private void createDirectoryForFTPFeed(final String path)
	{
		try
		{
			final File directory = new File(path);
			if (!directory.exists())
			{
				directory.mkdirs();
			}
		}
		catch (final Exception e)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Error while creating Directory", e);
		}


	}

	public BlOrderDao getOrderDao()
	{
		return orderDao;
	}

	public void setOrderDao(final BlOrderDao orderDao)
	{
		this.orderDao = orderDao;
	}


	public DefaultBlOrderFeedFTPService getDefaultBlOrderFeedFTPService()
	{
		return defaultBlOrderFeedFTPService;
	}

	public void setDefaultBlOrderFeedFTPService(final DefaultBlOrderFeedFTPService defaultBlOrderFeedFTPService)
	{
		this.defaultBlOrderFeedFTPService = defaultBlOrderFeedFTPService;
	}

	/**
	 * @return the blCompletedOrderPopulator
	 */
	public BlCompletedOrderPopulator getBlCompletedOrderPopulator()
	{
		return blCompletedOrderPopulator;
	}

	/**
	 * @param blCompletedOrderPopulator
	 *           the blCompletedOrderPopulator to set
	 */
	public void setBlCompletedOrderPopulator(final BlCompletedOrderPopulator blCompletedOrderPopulator)
	{
		this.blCompletedOrderPopulator = blCompletedOrderPopulator;
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

}
