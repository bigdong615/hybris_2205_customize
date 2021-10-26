package com.bl.integration.services;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.Map;

/**
 * This interface created to track UPS service for UPS scrape
 * @author Manikandan
 */
public interface BlUPSTrackService  {

  /**
   * This method created to track UPS service for UPS scrape
   * @param abstractOrderModel order to be send for ups scrape
   * @param packagingInfoModel packages to be send for UPS scrape
   * @return  Map<String, Object> response
   */
  Map<String, Object> trackUPSService(final AbstractOrderModel abstractOrderModel , final
  PackagingInfoModel packagingInfoModel);

}
