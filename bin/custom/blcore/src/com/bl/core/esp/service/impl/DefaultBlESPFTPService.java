package com.bl.core.esp.service.impl;

import com.bl.core.esp.populators.BlOrderFeedPopulator;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.dto.OrderFeedData;
import com.bl.esp.service.BlFTPService;
import com.bl.logging.BlLogger;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.util.Config;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Properties;
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
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This class created to send file to FTP
 * @author Manikandan
 */
public class DefaultBlESPFTPService implements BlFTPService {

  private static final Logger LOG = Logger.getLogger(DefaultBlESPFTPService.class);

  private BlOrderFeedPopulator blOrderFeedPopulator;

  /**
   * This method created to convert order into XML
   * @param abstractOrderModels list of orders
   * @throws ParserConfigurationException ParserConfigurationException
   */
  public void convertOrderIntoXML(final List<AbstractOrderModel> abstractOrderModels)
      throws ParserConfigurationException, JAXBException {
    final Document orderItemsInXMLDocument = createNewXMLDocument();

    final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlespintegrationConstants.ORDERS);

    final OrderFeedData  orderFeedData = new OrderFeedData();
    orderFeedData.setData(orderItemsInXMLDocument);
    orderFeedData.setElement(rootOrderItems);
    try {
      abstractOrderModels.forEach(abstractOrderModel -> getBlOrderFeedPopulator().populate(abstractOrderModel ,orderFeedData));
      Transformer transformer = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, BlespintegrationConstants.YES);
      transformer.setOutputProperty(BlespintegrationConstants.XML_INDENT, Config.getParameter(BlespintegrationConstants.XML_INDENT_SIZE));
      StreamResult result = new StreamResult(new StringWriter());
      DOMSource source = new DOMSource(orderFeedData.getData());
      transformer.transform(source, result);
      String xmlString = result.getWriter().toString();
      final String logFileName = new SimpleDateFormat(BlespintegrationConstants.FILE_FORMAT).format(new Date());
      final String fileName = BlespintegrationConstants.FILE_NAME_PREFIX + logFileName + BlespintegrationConstants.FILE_SUFFIX;
      Path completefileName = Path.of(Config.getParameter(BlespintegrationConstants.LOCAL_FTP_PATH) + fileName);
      Files.writeString(completefileName, xmlString);

      Session session = null;
        Channel channel = null;
        ChannelSftp channelSftp = null;
      try {
        JSch jsch = new JSch();
        session = jsch.getSession(Config.getParameter(BlespintegrationConstants.SFTPUSER),
            BlespintegrationConstants.SFTPHOST,
            BlespintegrationConstants.SFTPPORT);
        session.setPassword(Config.getParameter(BlespintegrationConstants.SFTPPASS));
        Properties config = new Properties();
        config.put(BlespintegrationConstants.STICT_HOST_KEY, BlespintegrationConstants.NO);
        session.setConfig(config);
        session.connect();
        channel = session.openChannel(BlespintegrationConstants.SFTP);
        channel.connect();
        channelSftp = (ChannelSftp) channel;
        channelSftp.cd(Config.getParameter(BlespintegrationConstants.CLIENT_FTP_PATH));
        File f = new File(completefileName.toAbsolutePath().toString());
        channelSftp.put(new FileInputStream(f), f.getName());
      } catch (JSchException | SftpException |FileNotFoundException ex) {
        BlLogger.logMessage(LOG, Level.ERROR, "Error while performing sendFileTOFTP:-", ex);
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

    }
    catch (final TransformerException | IOException e) {
      BlLogger.logMessage(LOG, Level.ERROR, "Error while performing convertOrderIntoXML" , e );

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


  public BlOrderFeedPopulator getBlOrderFeedPopulator() {
    return blOrderFeedPopulator;
  }

  public void setBlOrderFeedPopulator(BlOrderFeedPopulator blOrderFeedPopulator) {
    this.blOrderFeedPopulator = blOrderFeedPopulator;
  }


}
