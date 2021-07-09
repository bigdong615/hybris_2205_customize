package com.bl.blbackoffice.job;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.enums.HomeBaseEnum;
import com.bl.core.enums.NotesEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.model.PullReadyToShipOrdersCronJobModel;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
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
  private EnumerationService enumerationService;

  /**
   * It extracts the ready to ship consignments.
   *
   * @param pullReadyToShipOrdersCronJob
   * @return PerformResult
   */
  @Override
  public PerformResult perform(final PullReadyToShipOrdersCronJobModel pullReadyToShipOrdersCronJob) {

    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing PullReadyToShipOrdersJob...");

    try {

      List<ConsignmentModel> consignmentModels = null;
      if (CollectionUtils.isNotEmpty(pullReadyToShipOrdersCronJob.getMembers())){

        consignmentModels = blConsignmentDao
          .getReadyToShipConsignmentsForDate(pullReadyToShipOrdersCronJob.getShipDate());

      } else {

        BlLogger.logFormattedMessage(LOG, Level.DEBUG, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
            "Member list should not be empty before performing PullReadyToShipOrdersJob");
        return resetAndReturnResult(pullReadyToShipOrdersCronJob, CronJobResult.FAILURE);
      }

      List<ConsignmentModel> filteredConsignmentModels = null;
      if (pullReadyToShipOrdersCronJob.getHomeBaseCode().getCode().equalsIgnoreCase("All")) {

        filteredConsignmentModels = consignmentModels;
      } else {

      String warehouseCode =
          pullReadyToShipOrdersCronJob.getHomeBaseCode().getCode().equalsIgnoreCase("SFO")
              ? BlCoreConstants.SFO : BlCoreConstants.BOS;

        filteredConsignmentModels = consignmentModels.stream().filter(
          consignment -> consignment.getWarehouse().getCode().equalsIgnoreCase(warehouseCode))
          .collect(Collectors.toList());
      }

      PerformResult result = null;
      if (CollectionUtils.isNotEmpty(filteredConsignmentModels)) {

        result = writeToExcel(filteredConsignmentModels, pullReadyToShipOrdersCronJob.getMembers());
      } else {

        BlLogger.logFormattedMessage(LOG, Level.DEBUG, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
            "No matching consignments found while performing PullReadyToShipOrdersJob");
        return resetAndReturnResult(pullReadyToShipOrdersCronJob, CronJobResult.FAILURE);
      }

      if (result.getResult() == CronJobResult.FAILURE) {

        BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
            BlCoreConstants.PULL_JOB_ERROR_OCCURRED);
        return resetAndReturnResult(pullReadyToShipOrdersCronJob, CronJobResult.FAILURE);
      }
    } catch (final Exception ex) {

      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex,
          BlCoreConstants.PULL_JOB_ERROR_OCCURRED);
      return resetAndReturnResult(pullReadyToShipOrdersCronJob, CronJobResult.FAILURE);
    }

    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Successfully executed PullReadyToShipOrdersJob...");
    return resetAndReturnResult(pullReadyToShipOrdersCronJob, CronJobResult.SUCCESS);
  }

  private PerformResult resetAndReturnResult(
      final PullReadyToShipOrdersCronJobModel pullReadyToShipOrdersCronJob,
      final CronJobResult result) {

    resetParameters(pullReadyToShipOrdersCronJob);
    return new PerformResult(result, CronJobStatus.FINISHED);
  }

  /**
   * It populates the data into excel file.
   *
   * @param consignmentModels
   * @return members
   */
  private PerformResult writeToExcel(final List<ConsignmentModel> consignmentModels,
      final Set<String> members) throws IOException {

    XSSFSheet sheet = null;
    FileOutputStream fileOut = null;

    try (XSSFWorkbook workbook = new XSSFWorkbook()) {

      final String FILE_PATH = getConfigurationService().getConfiguration()
          .getString("pull.ready.to.ship.orders.file.path");    //D:/PullReadyToShip

      final String FILE_EXTENSION = "xlsx";
      DateFormat df = new SimpleDateFormat("yyyyMMddhhmmss"); // add S if you need milliseconds
      final String filename = FILE_PATH + df.format(new Date()) + "."
          + FILE_EXTENSION;  // filename = "D:/PullReadyToShip20210615152301.pdf"

      sheet = workbook.createSheet();


      CellStyle alignTopCellStyle = workbook.createCellStyle();
      alignTopCellStyle.setVerticalAlignment(VerticalAlignment.TOP);

      populateHeaderRow(sheet, workbook);

      populateDataRows(consignmentModels, sheet, members, workbook);

      fileOut = new FileOutputStream(filename);
      workbook.write(fileOut);

    } catch (Exception ex) {

      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex,
          BlCoreConstants.PULL_JOB_ERROR_OCCURRED);
      return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
    } finally {

      if (null != fileOut) {
        fileOut.close();
      }
    }

    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  /**
   * It populates the data into excel row header.
   *
   * @param sheet
   */
  private void populateHeaderRow(final XSSFSheet sheet, final XSSFWorkbook workbook) {

    XSSFRow rowHeader = sheet.createRow((short) 0);

    int columnIndex = 0;
    rowHeader.createCell(columnIndex).setCellValue(BlCoreConstants.SERIAL_NUMBER);

    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.PRODUCT_ID);
    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.PRODUCT_NAME);
    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.PRODUCT_COUNT);

    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.LOCATION_CODE);
    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.SHIPPING_METHOD);

    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.ORDER_NUMBER);
    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.ORDER_TYPE);

    List<NotesEnum>  notesEnums = getEnumerationService().getEnumerationValues(NotesEnum._TYPECODE);
    for (NotesEnum notesEnum : notesEnums){
      rowHeader.createCell(++columnIndex).setCellValue(notesEnum.getCode());
    }

    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.MEMBER_NAME);
    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.NEED);
    rowHeader.createCell(++columnIndex).setCellValue(BlCoreConstants.FOUND);

    CellStyle alignTopCellStyle = workbook.createCellStyle();
    alignTopCellStyle.setVerticalAlignment(VerticalAlignment.TOP);
    rowHeader.getPhysicalNumberOfCells();

    for (int i = 0; i < rowHeader.getPhysicalNumberOfCells(); i++ ) {
      sheet.autoSizeColumn(i);
      rowHeader.getCell(i).setCellStyle(alignTopCellStyle);
    }

  }

  /**
   * It populates the data into excel row header.
   *
   * @param consignmentModels
   * @param sheet
   * @param members
   */
  private void populateDataRows(final List<ConsignmentModel> consignmentModels,
      final XSSFSheet sheet, final Set<String> members, final XSSFWorkbook workbook) {

    List<String> membersList = new ArrayList<>(members);

    int rowNum = 1;
    int counter = 0;
    for (ConsignmentModel consignmentModel : consignmentModels) {

      if (membersList.size() <= counter) {
        counter = 0;
      }
      String memberName = membersList.get(counter);
      counter++;

      for (ConsignmentEntryModel entry : consignmentModel.getConsignmentEntries()) {

        for (BlSerialProductModel serialProduct : entry.getSerialProducts()) {

          XSSFRow dataRow = sheet.createRow((short) rowNum++);

          populateDataRow(consignmentModel, memberName, entry, serialProduct, dataRow, workbook);

          dataRow.setHeightInPoints(50);
        } //serial product loop

      }  //entry loop
    }

  }

  /**
   * It populates the data into excel row for data.
   *
   * @param consignmentModel
   * @param memberName
   * @param entry
   * @param serialProduct
   * @param dataRow
   */
  private void populateDataRow(final ConsignmentModel consignmentModel, final String memberName,
      final ConsignmentEntryModel entry, final BlSerialProductModel serialProduct,
      final XSSFRow dataRow, final XSSFWorkbook workbook) {

    dataRow.createCell(0)
        .setCellValue(serialProduct.getCode());                                 // SERIAL_NUMBER

    dataRow.createCell(1)
        .setCellValue(entry.getOrderEntry().getProduct().getCode());      //  PRODUCT_ID
    dataRow.createCell(2)
        .setCellValue(entry.getOrderEntry().getProduct().getName());      //  PRODUCT_NAME
    dataRow.createCell(3)
        .setCellValue(entry.getQuantity());                               // PRODUCT_COUNT

    dataRow.createCell(4).setCellValue(serialProduct
        .getOcLocation());                                                //LOCATION_CODE

    dataRow.createCell(5)
        .setCellValue(consignmentModel.getDeliveryMode().getName());     // SHIPPING_METHOD

    dataRow.createCell(6)
        .setCellValue(consignmentModel.getOrder().getCode());           //ORDER_NUMBER

    if (null != consignmentModel.getOrderType()) {
      dataRow.createCell(7).setCellValue(
          null != consignmentModel.getOrderType().getCode() ? consignmentModel.getOrderType()
              .getCode() : "");                                // ORDER_TYPE
    } else {
      dataRow.createCell(7).setCellValue("");
    }

    int columnIndex = 7;

    columnIndex = populateOrderNotes(consignmentModel, dataRow, workbook, columnIndex);

    dataRow.createCell(++columnIndex).setCellValue(memberName);

    dataRow.createCell(++columnIndex).setCellValue(BlCoreConstants.EMPTY_STRING);
    dataRow.createCell(++columnIndex).setCellValue(BlCoreConstants.EMPTY_STRING);

    CellStyle alignTopCellStyle = workbook.createCellStyle();
    alignTopCellStyle.setVerticalAlignment(VerticalAlignment.TOP);
    alignTopCellStyle.setWrapText(true);

    for (int i = 0; i < dataRow.getPhysicalNumberOfCells(); i++) {
      dataRow.getCell(i).setCellStyle(alignTopCellStyle);
    }

  }

  private int populateOrderNotes(final ConsignmentModel consignmentModel, final XSSFRow dataRow,
      final XSSFWorkbook workbook, int columnIndex) {

    CellStyle wrapCellStyle = workbook.createCellStyle();
    wrapCellStyle.setWrapText(true);

    List<NotesModel> orderNotes = consignmentModel.getOrderNotes();
    List<NotesEnum>  notesEnums = getEnumerationService().getEnumerationValues(NotesEnum._TYPECODE); // ORDER_NOTES
    for (NotesEnum notesEnum : notesEnums) {

      final XSSFCell cell = dataRow.createCell(++columnIndex);

      for (NotesModel notesModel : orderNotes) {
        if (notesModel.getType().getCode().equalsIgnoreCase(notesEnum.getCode())) {

          String valueToSet = getOrderNoteStringToSet(cell, notesModel);
          cell.setCellValue(valueToSet);
        }
      }

    }
    return columnIndex;
  }

  private String getOrderNoteStringToSet(final XSSFCell cell, final NotesModel notesModel) {

    final String valueToSet;
    if (StringUtils.isEmpty(cell.getStringCellValue())) {
      if (StringUtils.isNotEmpty(notesModel.getNote())) {
        valueToSet = notesModel.getNote();
      } else{
        valueToSet = "";
      }
    } else {
      String initialValue = cell.getStringCellValue();
      if (StringUtils.isNotEmpty(notesModel.getNote())) {
        valueToSet = initialValue + " \n  \n "+ notesModel.getNote();
      } else{
        valueToSet = initialValue;
      }
    }
    return valueToSet;
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

  public EnumerationService getEnumerationService() {
    return enumerationService;
  }

  public void setEnumerationService(final EnumerationService enumerationService) {
    this.enumerationService = enumerationService;
  }

}
