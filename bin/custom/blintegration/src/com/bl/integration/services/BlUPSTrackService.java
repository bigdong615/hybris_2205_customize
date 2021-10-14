package com.bl.integration.services;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Map;

/**
 * This interface created for UPS service
 */
public interface BlUPSTrackService  {

  /**
   * This method created  track the  UPS service
   */
  Map<String, Object> trackUPSService(final AbstractOrderModel abstractOrderModel , final
  PackagingInfoModel packagingInfoModel);

}
