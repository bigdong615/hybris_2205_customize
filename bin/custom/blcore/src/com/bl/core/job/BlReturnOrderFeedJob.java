package com.bl.core.job;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.order.ReturnOrderData;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.util.Config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.order.populators.BlReturnOrderPopulator;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.logging.BlLogger;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.SftpException;
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

	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		List<OrderModel> orderModelList = new ArrayList<OrderModel>();
		try
		{
			orderModelList = getOrderDao().getOrdersReadyForReturn();
		}
		catch (final ParseException ex)
		{
			LOG.info("Exception occurred during fetching return order models");
			ex.printStackTrace();
		}
		if (!orderModelList.isEmpty())
		{
			final List<ReturnOrderData> returnOrderList = new ArrayList<ReturnOrderData>();
			getBlReturnOrderPopulator().populate(orderModelList, returnOrderList);
			final File returnOrderFeed = getFile();
			FileWriter writer = null;
			try
			{
				writer = new FileWriter(returnOrderFeed);
			}
			catch (final IOException e1)
			{
				LOG.info("Exception occurred during creation of Writer");
				e1.printStackTrace();
			}

			final ColumnPositionMappingStrategy mappingStrategy = new ColumnPositionMappingStrategy();
			mappingStrategy.setType(ReturnOrderData.class);
			final String[] columns = new String[]
			{ "CustomerName", "Email", "OrderNumber", "OrderPlacedDate", "ReturnOrderDate", "ActualReturnOrderDate" };
			mappingStrategy.setColumnMapping(columns);
			final StatefulBeanToCsvBuilder<ReturnOrderData> builder = new StatefulBeanToCsvBuilder(writer);
			final StatefulBeanToCsv beanWriter = builder.withMappingStrategy(mappingStrategy).build();
			try
			{
				writer.append("Customer Name, Email, Order Number, Order Placed Date, Return Order Date, Actual Return Order Date");
				writer.append("\n");
				beanWriter.write(returnOrderList);
				writer.close();
			}
			catch (final CsvDataTypeMismatchException | CsvRequiredFieldEmptyException | IOException e)
			{
				e.printStackTrace();
				LOG.info("Exception occurred during converting to CSV");
				return new PerformResult(CronJobResult.ERROR, CronJobStatus.ABORTED);
			}
			sendFileToFTPLocation(returnOrderFeed);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	private void sendFileToFTPLocation(final File file)
	{
		Session session = null;
		Channel channel = null;
		ChannelSftp channelSftp = null;
		try
		{
			final JSch jsch = new JSch();
			session = jsch.getSession(Config.getParameter(BlespintegrationConstants.SFTPUSER),
					Config.getParameter(BlespintegrationConstants.SFTP_RETURN_ORDER_HOST),
					Config.getInt(BlespintegrationConstants.SFTPPORT, 22));
			session.setPassword(Config.getParameter(BlespintegrationConstants.SFTP_RETURN_ORDER_PASS));
			final Properties config = new Properties();
			config.put(BlespintegrationConstants.STICT_HOST_KEY, BlespintegrationConstants.NO);
			session.setConfig(config);
			session.connect();
			channel = session.openChannel(BlespintegrationConstants.SFTP);
			channel.connect();
			channelSftp = (ChannelSftp) channel;
			channelSftp.cd(Config.getParameter(BlespintegrationConstants.CLIENT_FTP_PATH));
			final File f = new File(file.getAbsolutePath());
			try (FileInputStream fileInputStream = new FileInputStream(f))
			{
				channelSftp.put(fileInputStream, f.getName());
			}
		}
		catch (JSchException | SftpException | IOException ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Error while sending file to FTP location.:-", ex);
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
		final String logFileName = new SimpleDateFormat(BlespintegrationConstants.FILE_FORMAT).format(new Date());
		final String fileName = new StringBuilder(BlespintegrationConstants.RETUNR_ORDER_FILE_NAME_PREFIX).append(BlespintegrationConstants.RETURN_ORDER_FILE_SUFFIX).toString();
		final String path = Config.getParameter(BlespintegrationConstants.LOCAL_FTP_PATH);
		createDirectoryForFTPFeed(path);
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

	public BlReturnOrderPopulator getBlReturnOrderPopulator()
	{
		return blReturnOrderPopulator;
	}

	public void setBlReturnOrderPopulator(final BlReturnOrderPopulator blReturnOrderPopulator)
	{
		this.blReturnOrderPopulator = blReturnOrderPopulator;
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
