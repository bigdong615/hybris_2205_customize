package com.bl.integration.dao;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import java.util.List;

/**
 * This interface created for Order DAO
 */
public interface BlOrderDao {

  /**
   * This method created to get the order based on order code
   * @param orderNumber orderNumber
   * @return AbstractOrderModel
   */
   AbstractOrderModel getOrderByCode(final String orderNumber);

  /**
   * This method created to get the list of order
   * @return List<AbstractOrderModel>
   */
   List<AbstractOrderModel> getOrdersForUPSScrape();

  /**
   * This method created to get the list of packges
   * @return List<PackagingInfoModel>
   */
   List<PackagingInfoModel> getRescheduledPackagesForUPSScrape();
  }
