
package com.bl.integration.services;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Map;

public interface BlTrackWebService {

  /**
   * This method created to track service
   * @param abstractOrderModel abstractOrderModel
   * @param packagingInfoModel packagingInfoModel
   * @return Map
   */
  Map trackService(final AbstractOrderModel abstractOrderModel , final PackagingInfoModel packagingInfoModel);

  }

