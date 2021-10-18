package com.bl.integration.upsscrape;

import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Date;

/**
 * This interface created to update the serial products
 */
public interface UpdateSerialService {

  /**
   * This method created to update the serial product status
   * @param packageCode packageCode
   * @param orderCode orderCode
   * @param upsDeliveryDate upsDeliveryDate
   * @param numberOfRepetition numberOfRepetition
   * @param packagingInfoModel packagingInfoModel
   */
   void updateSerialProducts(final String packageCode , final String orderCode , final Date upsDeliveryDate , final int numberOfRepetition , final
   PackagingInfoModel packagingInfoModel);


  }
