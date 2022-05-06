package com.bl.core.esp.service;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import java.util.List;
import javax.xml.parsers.ParserConfigurationException;

/**
 * This interface created to send file
 * @author Manikandan
 */
public interface BlOrderFeedFTPService {

  /**
   * This method created to convert order into XML
   */
  void convertOrderIntoXML(final List<AbstractOrderModel> abstractOrderModels ,  final List<AbstractOrderModel> unExportedOrderList)
      throws ParserConfigurationException;
}
