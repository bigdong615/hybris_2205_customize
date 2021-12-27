package com.bl.esp.service;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import java.util.List;
import javax.xml.bind.JAXBException;
import javax.xml.parsers.ParserConfigurationException;

/**
 * This interface created to send file
 * @author Manikandan
 */
public interface BlFTPService {

  /**
   * This method created to convert order into XML
   */
  void convertOrderIntoXML(final List<AbstractOrderModel> abstractOrderModels)
      throws ParserConfigurationException, JAXBException;
}
