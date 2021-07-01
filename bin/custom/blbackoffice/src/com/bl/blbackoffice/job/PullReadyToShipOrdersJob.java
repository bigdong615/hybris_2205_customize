package com.bl.blbackoffice.job;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.enums.HomeBaseEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.PullReadyToShipOrdersCronJobModel;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.io.FileOutputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;


/**
 * This cron job will extract the excel for ready to ship consignments
 *
 * @author Sunil
 */
public class PullReadyToShipOrdersJob extends AbstractJobPerformable<PullReadyToShipOrdersCronJobModel> {

  private static final Logger LOG = Logger.getLogger(PullReadyToShipOrdersJob.class);

  private BlConsignmentDao blConsignmentDao;
  private ConfigurationService configurationService;

  /**
   * It extracts the ready to ship consignments.
   *
   * @param pullReadyToShipOrdersCronJob
   * @return PerformResult
   */
  @Override
  public PerformResult perform(
      final PullReadyToShipOrdersCronJobModel pullReadyToShipOrdersCronJob) {

    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing PullReadyToShipOrdersJob...");
    try {

      List<ConsignmentModel> consignmentModels = blConsignmentDao
          .getReadyToShipConsignmentsForDate(pullReadyToShipOrdersCronJob.getShipDate());

      writeToExcel(consignmentModels, pullReadyToShipOrdersCronJob.getMembers());

    } catch (final Exception ex) {

      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex,
          "Error occurred while performing PullReadyToShipOrdersJob");

      return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
    }

    resetParameters(pullReadyToShipOrdersCronJob);
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  /**
   * It populates the data into excel file.
   *
   * @param consignmentModels
   * @return members
   */
  private void writeToExcel(final List<ConsignmentModel> consignmentModels,
      final Set<String> members) throws Exception {

    try {

      final String FILE_PATH = getConfigurationService().getConfiguration()
          .getString("pull.ready.to.ship.orders.file.path");    //D:/PullReadyToShip

      final String FILE_EXTENSION = "xlsx";
      DateFormat df = new SimpleDateFormat("yyyyMMddhhmmss"); // add S if you need milliseconds
      final String filename = FILE_PATH + df.format(new Date()) + "."
          + FILE_EXTENSION;  // filename = "D:/PullReadyToShip20210615152301.pdf"

      XSSFWorkbook workbook = new XSSFWorkbook();

      XSSFSheet sheet = workbook.createSheet();

      //creating the 0th row using the createRow() method
      XSSFRow rowHeader = sheet.createRow((short) 0);
      populateRowHead(rowHeader);

      populateDataRows(consignmentModels, sheet, members);

      FileOutputStream fileOut = new FileOutputStream(filename);
      workbook.write(fileOut);

      fileOut.close();

      workbook.close();
    } catch (Exception ex) {
      throw new Exception(ex);
    }
  }

  /**
   * It populates the data into excel row header.
   *
   * @param rowHeader
   */
  private void populateRowHead(final XSSFRow rowHeader) {

    rowHeader.createCell(0).setCellValue(BlCoreConstants.ORDER_NUMBER);
    rowHeader.createCell(1).setCellValue(BlCoreConstants.ORDER_TYPE);
    rowHeader.createCell(2).setCellValue(BlCoreConstants.ORDER_NOTES);

    rowHeader.createCell(3).setCellValue(BlCoreConstants.PRODUCT_ID);
    rowHeader.createCell(4).setCellValue(BlCoreConstants.PRODUCT_NAME);
    rowHeader.createCell(5).setCellValue(BlCoreConstants.PRODUCT_COUNT);

    rowHeader.createCell(6).setCellValue(BlCoreConstants.SERIAL_NUMBER);
    rowHeader.createCell(7).setCellValue(BlCoreConstants.LOCATION_CODE);
    rowHeader.createCell(8).setCellValue(BlCoreConstants.SHIPPING_METHOD);
    rowHeader.createCell(9).setCellValue(BlCoreConstants.NEED);
    rowHeader.createCell(10).setCellValue(BlCoreConstants.FOUND);

  }

  /**
   * It populates the data into excel row header.
   *
   * @param consignmentModels
   * @param sheet
   * @param members
   */
  private void populateDataRows(final List<ConsignmentModel> consignmentModels,
      final XSSFSheet sheet, final Set<String> members) {

    List<String> membersList = new ArrayList<>(members);

    int rowNum = 0;
    int counter = 0;
    for (ConsignmentModel consignmentModel : consignmentModels) {

      String memberName = membersList.get(counter);
      counter++;

      for (ConsignmentEntryModel entry : consignmentModel.getConsignmentEntries()) {

        if (membersList.size() <= counter) {
          counter = 0;
        }

        for (BlSerialProductModel serialProduct : entry.getSerialProducts()) {

          XSSFRow dataRow = sheet.createRow((short) rowNum++);

          dataRow.createCell(0)
              .setCellValue(consignmentModel.getOrder().getCode());           //ORDER_NUMBER

          if (null != consignmentModel.getOrderType()) {
            dataRow.createCell(1).setCellValue(
                null != consignmentModel.getOrderType().getCode() ? consignmentModel.getOrderType()
                    .getCode() : "");                                // ORDER_TYPE
          } else {
            dataRow.createCell(1).setCellValue("");
          }

          dataRow.createCell(2).setCellValue(BlCoreConstants.ORDER_NOTES); //need to check  ORDER_NOTES

          dataRow.createCell(3)
              .setCellValue(entry.getOrderEntry().getProduct().getCode());      //  PRODUCT_ID
          dataRow.createCell(4)
              .setCellValue(entry.getOrderEntry().getProduct().getName());      //  PRODUCT_NAME
          dataRow.createCell(5)
              .setCellValue(entry.getQuantity());                               // PRODUCT_COUNT

          dataRow.createCell(6)
              .setCellValue(serialProduct.getCode());                                 // SERIAL_NUMBER
          dataRow.createCell(7)
              .setCellValue(consignmentModel.getWarehouse()
                  .getCode());    //LOCATION_CODE to be taken from serial prod model.

          dataRow.createCell(8)
              .setCellValue(consignmentModel.getDeliveryMode().getName());     // SHIPPING_METHOD

          dataRow.createCell(9).setCellValue(memberName);

          dataRow.createCell(10).setCellValue(BlCoreConstants.EMPTY_STRING);
          dataRow.createCell(11).setCellValue(BlCoreConstants.EMPTY_STRING);
        } //serial product loop
      }  //entry loop
    }

  }


  /**
   * It resets the parameters once the cron job is successfully run
   *
   * @param pullReadyToShipOrdersCronJob
   */
  private void resetParameters(final PullReadyToShipOrdersCronJobModel pullReadyToShipOrdersCronJob)
  {
    pullReadyToShipOrdersCronJob.setShipDate(new Date());
    pullReadyToShipOrdersCronJob.setHomeBaseCode(HomeBaseEnum.ALL);
    pullReadyToShipOrdersCronJob.setMembers(null);
    this.modelService.save(pullReadyToShipOrdersCronJob);
  }

  /**
   * @return the blConsignmentDao
   */
  public BlConsignmentDao getBlConsignmentDao() {
    return blConsignmentDao;
  }

  /**
   * @param blConsignmentDao
   *           the blConsignmentDao to set
   */
  public void setBlConsignmentDao(final BlConsignmentDao blConsignmentDao) {
    this.blConsignmentDao = blConsignmentDao;
  }

  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  public void setConfigurationService(
      final ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }
}
