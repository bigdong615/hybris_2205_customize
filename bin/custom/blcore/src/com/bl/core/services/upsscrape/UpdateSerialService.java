package com.bl.core.services.upsscrape;

import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Date;

/**
 * This interface created to update the serial products
 * @author Manikandan
 */
public interface UpdateSerialService {

  /**
   * This method created to update the serial product status
   * @param packageCode package code to be updated
   * @param orderCode orderCode order to be updated
   * @param upsDeliveryDate delivery date from UPS scrape service
   * @param numberOfRepetition total number of repetitions for package
   * @param packagingInfoModel package to be updated
   */
   void updateSerialProducts(final String packageCode , final String orderCode , final Date upsDeliveryDate , final int numberOfRepetition , final
   PackagingInfoModel packagingInfoModel , final Date trackDate);


  }
