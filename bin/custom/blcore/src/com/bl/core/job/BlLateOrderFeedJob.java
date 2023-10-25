package com.bl.core.job;

import com.bl.core.esp.service.impl.DefaultBlOrderFeedFTPService;
import com.bl.core.model.BlOrderFeedCronJobModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.order.populators.BlLateOrderPopulator;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.logging.BlLogger;
import com.jcraft.jsch.*;
import com.opencsv.bean.ColumnPositionMappingStrategy;
import com.opencsv.bean.StatefulBeanToCsv;
import com.opencsv.bean.StatefulBeanToCsvBuilder;
import com.opencsv.exceptions.CsvDataTypeMismatchException;
import com.opencsv.exceptions.CsvRequiredFieldEmptyException;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.order.LateOrderData;
import de.hybris.platform.order.ReturnOrderData;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.util.Config;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * This class created to feed the late order to ESP Feed
 * @author Jyoti Swamy
 */
public class BlLateOrderFeedJob extends AbstractJobPerformable<CronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlLateOrderFeedJob.class);

  private BlOrderDao orderDao;
  private BlLateOrderPopulator blLateOrderPopulator;



  /**
   * This method created to perform the ESP order feed
   *
   * @param cronJobModel cronjob instance
   * @return result
   */
  @Override
  public PerformResult perform(final CronJobModel cronJobModel) {
    List<AbstractOrderModel> orderModelList = new ArrayList<>();
    try {
      orderModelList = getOrderDao().getOrdersForLateOrderFeedToFTP();
    } catch (final ParseException ex) {
      LOG.info("Exception occurred during fetching return order models");
      ex.printStackTrace();
    }
    if (!orderModelList.isEmpty()) {
      final List<LateOrderData> lateOrderDataList = new ArrayList<>();
      getBlLateOrderPopulator().populate(orderModelList, lateOrderDataList);
      final File lateOrderFeed = getFile();
      FileWriter writer = null;
      try {
        writer = new FileWriter(lateOrderFeed.getAbsoluteFile());
      } catch (final IOException e1) {
        LOG.info("Exception occurred during creation of Writer");
        e1.printStackTrace();
      }

      final ColumnPositionMappingStrategy mappingStrategy = new ColumnPositionMappingStrategy();
      mappingStrategy.setType(LateOrderData.class);
      final String[] columns = new String[]
              {"subscriberId","emailAddress", "orderNumber", "customerName", "rentalStartDate", "rentalEndDate", "optimizedReturnDate","daysLate","shippingMethod","shippingMethodType"};
      mappingStrategy.setColumnMapping(columns);
      final StatefulBeanToCsvBuilder<ReturnOrderData> builder = new StatefulBeanToCsvBuilder(writer);
      final StatefulBeanToCsv beanWriter = builder.withMappingStrategy(mappingStrategy).build();
      try {
        writer.append("Subscriber_ID,Email_Address, Order_Number, Customer_Name, Rental_Start_Date, Rental_End_Date, Optimized_Return_Date,Days_Late,Shipping_Method,Shipping_Type");
        writer.append("\n");
        beanWriter.write(lateOrderDataList);
        writer.close();
      } catch (final CsvDataTypeMismatchException | CsvRequiredFieldEmptyException | IOException e) {
        e.printStackTrace();
        LOG.info("Exception occurred during converting to CSV");
        return new PerformResult(CronJobResult.ERROR, CronJobStatus.ABORTED);
      }
      sendFileToFTPLocation(lateOrderFeed);
    }
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  private void sendFileToFTPLocation(final File file) {
    Session session = null;
    Channel channel = null;
    ChannelSftp channelSftp = null;
    try {
      final JSch jsch = new JSch();
      session = jsch.getSession(Config.getParameter(BlespintegrationConstants.SFTPUSER),
              Config.getParameter(BlespintegrationConstants.SFTPHOST),
              Config.getInt(BlespintegrationConstants.SFTPPORT, 22));
      session.setPassword(Config.getParameter(BlespintegrationConstants.SFTPPASS));
      final Properties config = new Properties();
      config.put(BlespintegrationConstants.STICT_HOST_KEY, BlespintegrationConstants.NO);
      session.setConfig(config);
      session.connect();
      channel = session.openChannel(BlespintegrationConstants.SFTP);
      channel.connect();
      channelSftp = (ChannelSftp) channel;
      channelSftp.cd(Config.getParameter(BlespintegrationConstants.CLIENT_FTP_PATH));
      final File f = new File(file.getAbsolutePath());
      try (FileInputStream fileInputStream = new FileInputStream(f)) {
        channelSftp.put(fileInputStream, f.getName());
      }
    } catch (JSchException | SftpException | IOException ex) {
      BlLogger.logMessage(LOG, Level.ERROR, "Error while sending file to FTP location.:-", ex);
    }
    finally {
      if (null != channelSftp) {
        channelSftp.disconnect();
        channelSftp.exit();
      }
      if (null != channel) {
        channel.disconnect();
      }
      if (null != session) {
        session.disconnect();
      }
    }
    if(file.exists()) {
      file.delete();
    }
  }

  private File getFile() {
    final String logFileName = new SimpleDateFormat(BlespintegrationConstants.FILE_FORMAT).format(new Date());
    final String fileName = new StringBuilder(BlespintegrationConstants.LATE_ORDER_FILE_NAME_PREFIX + logFileName + BlespintegrationConstants.RETURN_ORDER_FILE_SUFFIX).toString();
    final String path = Config.getParameter(BlespintegrationConstants.LOCAL_FTP_PATH_LATE_ORDER);
    createDirectoryForFTPFeed(path);
    LOG.info("Late order File name " + fileName);
    return new File(new StringBuilder(path).append(BlespintegrationConstants.SLASH).append(fileName).toString());
  }

  private void createDirectoryForFTPFeed(final String path) {
    try {
      final File directory = new File(path);
      if (!directory.exists()) {
        directory.mkdirs();
      }
    } catch (final Exception e) {
      BlLogger.logMessage(LOG, Level.ERROR, "Error while creating Directory", e);
    }


  }

  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }

  public BlLateOrderPopulator getBlLateOrderPopulator() {
    return blLateOrderPopulator;
  }

  public void setBlLateOrderPopulator(BlLateOrderPopulator blLateOrderPopulator) {
    this.blLateOrderPopulator = blLateOrderPopulator;
  }

}