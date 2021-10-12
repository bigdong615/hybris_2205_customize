package com.bl.integration.upsscrape;

import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Date;

public interface UpdateSerialService {

   void updateSerialProducts(final String packageCode , final String orderCode , final Date upsDeliveryDate , final int numberOfRepetition , final
   PackagingInfoModel packagingInfoModel);


  }
