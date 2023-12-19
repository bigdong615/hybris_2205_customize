package com.bl.core.esp.service.impl;

import de.hybris.platform.core.enums.ExportStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.UpsScrapeOrderData;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;

import javax.xml.bind.JAXBException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.bl.core.esp.populators.BlOrderBillFeedPopulator;
import com.bl.core.esp.populators.BlOrderFeedPopulator;
import com.bl.core.esp.service.BlOrderFeedFTPService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.order.populators.BlLateOrderPopulator;
import com.bl.core.order.populators.BlUpsScrapeOrderPopulator;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.stock.impl.DefaultBlStockLevelDao;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.dto.OrderFeedData;
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
 * This class created to send file to FTP
 * @author Manikandan
 */
public class DefaultBlOrderFeedFTPService implements BlOrderFeedFTPService {

  private static final Logger LOG = Logger.getLogger(DefaultBlOrderFeedFTPService.class);

  private BlOrderFeedPopulator blOrderFeedPopulator;
  private BlOrderBillFeedPopulator blOrderBillFeedPopulator;
  private ModelService modelService;
  private BlLateOrderPopulator blLateOrderPopulator;
  private BlUpsScrapeOrderPopulator blUpsScrapeOrderPopulator;
  private BlOrderDao orderDao;
  private BlStockLevelDao blStockLevelDao;

/**
   * This method created to convert order into XML
   *
   * @param abstractOrderModels
   *           list of orders
   * @throws ParserConfigurationException
   *            ParserConfigurationException
   */
  @Override
  public void convertOrderIntoXML(final List<AbstractOrderModel> abstractOrderModels , final List<AbstractOrderModel> unExportedOrderList)
      throws ParserConfigurationException {
    final Document orderItemsInXMLDocument = createNewXMLDocument();
    final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlespintegrationConstants.ORDERS);
    final OrderFeedData  orderFeedData = new OrderFeedData();
    orderFeedData.setData(orderItemsInXMLDocument);
    orderFeedData.setElement(rootOrderItems);
    final List<AbstractOrderModel> exportedOrderList = new ArrayList<>();

    abstractOrderModels.forEach(abstractOrderModel -> {
      try {
        getBlOrderFeedPopulator().populate(abstractOrderModel , orderFeedData);
        exportedOrderList.add(abstractOrderModel);
      }
      catch (final Exception e) {
        BlLogger.logFormattedMessage(LOG , Level.ERROR , "DefaultBlESPFTPService :- Error while converting order code {}"  , abstractOrderModel.getCode());
        unExportedOrderList.add(abstractOrderModel);
      }
    });
    final String xmlString = covertXMLIntoString(orderFeedData);
    final String logFileName = new SimpleDateFormat(BlespintegrationConstants.FILE_FORMAT).format(new Date());
    final String fileName = BlespintegrationConstants.FILE_NAME_PREFIX + logFileName + BlespintegrationConstants.FILE_SUFFIX;
    final String path = Config.getParameter(BlespintegrationConstants.LOCAL_FTP_PATH);
    createDirectoryForFTPFeed(path);
    final File file = new File(path + BlespintegrationConstants.SLASH + fileName);
    writeFeedRequestToFile(file , xmlString);
    sendFileToFTPLocation(file);
    updatedExportedOrderStatusForOrders(exportedOrderList);
  }

  @Override
  public void convertOrderTOFeed(final List<AbstractOrderModel> abstractOrderModels) throws ParserConfigurationException
  {
	  final ArrayList<AbstractOrderModel> filteredOrdersList = new ArrayList<AbstractOrderModel>();
	  final List<UpsScrapeOrderData> orderDataList = new ArrayList<>();
	  abstractOrderModels.forEach(abstractOrderModel -> abstractOrderModel.getConsignments().forEach(consignmentModel -> {
		  if (CollectionUtils.isEmpty(consignmentModel.getPackaginginfos())
				  && BooleanUtils.isTrue(abstractOrderModel.getIsExtendedOrder()))
		  {
			  final OrderModel originalOrder = getOrderDao().getOriginalOrderFromExtendedOrderCode(abstractOrderModel.getCode());
			  if (Objects.nonNull(originalOrder) && CollectionUtils.isNotEmpty(originalOrder.getConsignments()))
			  {
				  originalOrder.getConsignments().forEach(origConsignment -> {
					  if (CollectionUtils.isNotEmpty(origConsignment.getPackaginginfos()))
					  {
						  origConsignment.getPackaginginfos().forEach(packagingInfoModel -> {
							  if (BooleanUtils.isFalse(packagingInfoModel.isIsScrapeScanCompleted()))
							  {
								  filteredOrdersList.add(abstractOrderModel);
								  setReservedStatusForSerials(packagingInfoModel);
							  }
						  });
					  }
				  });
			  }
		  }
		  else
		  {
			  consignmentModel.getPackaginginfos().forEach(packagingInfoModel -> {
				  if (BooleanUtils.isFalse(packagingInfoModel.isIsScrapeScanCompleted()))
				  {
					  filteredOrdersList.add(abstractOrderModel);
					  setReservedStatusForSerials(packagingInfoModel);
				  }
			  });
		  }
	  }));
	  getBlUpsScrapeOrderPopulator().populate(filteredOrdersList, orderDataList);
	  final File lateOrderFeed = getUpsScrapeFile();
	  FileWriter writer = null;
	  try
	  {
		  writer = new FileWriter(lateOrderFeed.getAbsoluteFile());
	  }
	  catch (final IOException e1)
	  {
		  LOG.info("Exception occurred during creation of Writer");
		  e1.printStackTrace();
	  }
	  final ColumnPositionMappingStrategy mappingStrategy = new ColumnPositionMappingStrategy();
	  mappingStrategy.setType(UpsScrapeOrderData.class);
	  final String[] columns = new String[]
	  { "subscriberId", "emailAddress", "orderNumber", "customerName", "shippingMethod", "rentalStartDate", "rentalEndDate" };
	  mappingStrategy.setColumnMapping(columns);
	  final StatefulBeanToCsvBuilder<UpsScrapeOrderData> builder = new StatefulBeanToCsvBuilder(writer);
	  final StatefulBeanToCsv beanWriter = builder.withMappingStrategy(mappingStrategy).build();
	  try
	  {
		  writer.append(
				  "Subscriber_ID, Email_Address, Order_Number, Customer_Name, Shipping_Method, Rental_Start_Date, Rental_End_Date");
		  writer.append("\n");
		  beanWriter.write(orderDataList);
		  writer.close();
	  }
	  catch (final CsvDataTypeMismatchException | CsvRequiredFieldEmptyException | IOException e)
	  {
		  e.printStackTrace();
		  LOG.info("Exception occurred during converting to CSV");
	  }
	  sendFileToFTPLocation(lateOrderFeed);
  }

/**
 * @param packagingInfoModel
 */
private void setReservedStatusForSerials(final PackagingInfoModel packagingInfoModel)
{
	final Date startDay = new Date();
	final Date endDay = DateUtils.addDays(startDay, 1);
	  final Set<String>serialCodes = new HashSet<String>();
	  for(final BlProductModel serial : packagingInfoModel.getSerialProducts()) {
		  serialCodes.add(serial.getCode());
	  }
	  final Collection<StockLevelModel> stockLevels = getBlStockLevelDao().findALLSerialStockLevelsForDateAndCodes(serialCodes,
			  startDay, endDay);
	  if (CollectionUtils.isNotEmpty(stockLevels)) {
		  stockLevels.forEach(stockLevel -> {
			  stockLevel.setReservedStatus(true);
		  });
	  }
}




  /**
   * This method created to convert XML into string
   * @param orderFeedData order feed data
   * @return string
   */
  private String covertXMLIntoString(final OrderFeedData orderFeedData) {
    final StreamResult result = new StreamResult(new StringWriter());
    try {
      final Transformer transformer = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, BlespintegrationConstants.YES);
      transformer.setOutputProperty(BlespintegrationConstants.XML_INDENT,
          Config.getParameter(BlespintegrationConstants.XML_INDENT_SIZE));
      final DOMSource source = new DOMSource(orderFeedData.getData());
      transformer.transform(source, result);
    }
    catch (final TransformerException e) {
      BlLogger.logMessage(LOG , Level.ERROR , "DefaultBlESPFTPService :- Error while executing covertXMLIntoString method " , e);
    }
    return result.getWriter().toString();
  }

  /**
   * This method created to write xml into local file
   * @param file local file to send to FTP feed
   * @param xmlString xml
   */
  private void writeFeedRequestToFile(final File file, final String xmlString) {
    try{
      final FileWriter fw = new FileWriter(file.getAbsoluteFile());
      try (BufferedWriter bw = new BufferedWriter(fw)) {
        bw.write(xmlString);
      }
      fw.close();
    }
    catch (final IOException e){
      BlLogger.logMessage(LOG , Level.ERROR , "Error while writingFeedRequestToFile method" , e);
    }
  }

  /**
   * This method created to create specific directory
   * @param path path to be craete
   */
  private void createDirectoryForFTPFeed(final String path) {
    try {
      final File directory = new File(path);
      if (!directory.exists()){
        directory.mkdirs();
      }
    }
    catch (final Exception e) {
      BlLogger.logMessage(LOG , Level.ERROR , "Error while creating Directory" , e);
    }


  }


  /**
   * This method created to populate data
   * @return data which converted
   * @throws ParserConfigurationException parserConfigurationException
   */
  protected Document createNewXMLDocument() throws ParserConfigurationException {
    final DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
    final DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
    return documentBuilder.newDocument();
  }

  /**
   * This method created to add the root element
   * @param document document to be add
   * @param rootElement root element to be set
   * @return element which append
   */
  protected Element createRootElementForDocument(final Document document, final String rootElement) {
    final Element root = document.createElement(rootElement);
    document.appendChild(root);
    return root;
  }


  /**
   * This method created to send file to FTP location
   * @param file file which needs to drop at FTP location
   */
  private void sendFileToFTPLocation(final File file){
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

  /**
   * This method created to update the sentOrderFeedToSalesforce
   * @param exportedOrderList list  of orders
   */
  private void updatedExportedOrderStatusForOrders(final List<AbstractOrderModel> exportedOrderList) {
    if(CollectionUtils.isNotEmpty(exportedOrderList)){
      exportedOrderList.forEach(abstractOrderModel -> {
        abstractOrderModel.setSentOrderFeedToSalesforce(ExportStatus.EXPORTED);
        getModelService().save(abstractOrderModel);
        getModelService().refresh(abstractOrderModel);
      });
    }
  }


  public BlOrderFeedPopulator getBlOrderFeedPopulator() {
    return blOrderFeedPopulator;
  }

  public void setBlOrderFeedPopulator(final BlOrderFeedPopulator blOrderFeedPopulator) {
    this.blOrderFeedPopulator = blOrderFeedPopulator;
  }

  /**
   * This method created to convert order bill related data into XML
   * @param abstractOrderModels list of orders
   * @param unExportedOrderList list of unExportedOrderList
   * @throws ParserConfigurationException ParserConfigurationException
   * @throws JAXBException JAXBException
   */
  public void convertOrderBillIntoXML(final List<AbstractOrderModel> abstractOrderModels,final List<AbstractOrderModel> unExportedOrderList)
      throws ParserConfigurationException {
    final OrderFeedData  orderFeedData = getOrderFeedData();
    final List<AbstractOrderModel> exportedOrderList = new ArrayList<>();
   abstractOrderModels.forEach(abstractOrderModel -> {
     try {
       getBlOrderBillFeedPopulator()
           .populate(abstractOrderModel, orderFeedData);
       exportedOrderList.add(abstractOrderModel);
     }catch (final Exception e){
       unExportedOrderList.add(abstractOrderModel);
       BlLogger.logFormattedMessage(LOG , Level.ERROR , "Error while converting order {} bill to xml"  , abstractOrderModel.getCode());
     }
   });
    final String xmlString = covertXMLIntoString(orderFeedData);
    final File file = getFile();
    writeFeedRequestToFile(file, xmlString);
    sendFileToFTPLocation(file);
    updatedExportedOrderStatusForOrders(exportedOrderList);
  }

  /**
   *  this method use to create document and feed data DTO.
   * @return
   * @throws ParserConfigurationException
   */
  private OrderFeedData getOrderFeedData() throws ParserConfigurationException {
    final Document billItemsInXMLDocument = createNewXMLDocument();
    final Element rootBillItems = createRootElementForDocument(billItemsInXMLDocument, BlespintegrationConstants.ORDERS);
    final OrderFeedData  orderFeedData = new OrderFeedData();
    orderFeedData.setData(billItemsInXMLDocument);
    orderFeedData.setElement(rootBillItems);
    return orderFeedData;
  }

  /**
   * This method used to create file.
   */
private File getFile(){
  final String logFileName = new SimpleDateFormat(BlespintegrationConstants.FILE_FORMAT).format(new Date());
  final String fileName =new StringBuilder(BlespintegrationConstants.BILL_FILE_NAME_PREFIX).append(logFileName).append(BlespintegrationConstants.FILE_SUFFIX).toString() ;
  final String path = Config.getParameter(BlespintegrationConstants.LOCAL_FTP_PATH);
  createDirectoryForFTPFeed(path);
  return new File(new StringBuilder(path).append(BlespintegrationConstants.SLASH).append(fileName).toString());
}

private File getUpsScrapeFile()
{
	final String logFileName = new SimpleDateFormat(BlespintegrationConstants.FILE_FORMAT).format(new Date());
	final String fileName = new StringBuilder(BlespintegrationConstants.UPS_SCRAPE_FILE_NAME_PREFIX).append(logFileName).append(BlespintegrationConstants.UPS_SCRAPE_FILE_SUFFIX).toString();
	final String path = Config.getParameter(BlespintegrationConstants.LOCAL_FTP_PATH);
	createDirectoryForFTPFeed(path);
	return new File(new StringBuilder(path).append(BlespintegrationConstants.SLASH).append(fileName).toString());
}

  public BlOrderBillFeedPopulator getBlOrderBillFeedPopulator() {
    return blOrderBillFeedPopulator;
  }

  public void setBlOrderBillFeedPopulator(
      final BlOrderBillFeedPopulator blOrderBillFeedPopulator) {
    this.blOrderBillFeedPopulator = blOrderBillFeedPopulator;
  }
  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

  public BlLateOrderPopulator getBlLateOrderPopulator()
  {
	  return blLateOrderPopulator;
  }

  public void setBlLateOrderPopulator(final BlLateOrderPopulator blLateOrderPopulator)
  {
	  this.blLateOrderPopulator = blLateOrderPopulator;
  }

  public BlUpsScrapeOrderPopulator getBlUpsScrapeOrderPopulator()
  {
	  return blUpsScrapeOrderPopulator;
  }

  public void setBlUpsScrapeOrderPopulator(final BlUpsScrapeOrderPopulator blUpsScrapeOrderPopulator)
  {
	  this.blUpsScrapeOrderPopulator = blUpsScrapeOrderPopulator;
  }

  public BlOrderDao getOrderDao()
  {
	  return orderDao;
  }

  public void setOrderDao(final BlOrderDao orderDao)
  {
	  this.orderDao = orderDao;
  }
  
  public BlStockLevelDao getBlStockLevelDao()
  {
	  return blStockLevelDao;
  }

  public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao)
  {
	  this.blStockLevelDao = blStockLevelDao;
  }

}
