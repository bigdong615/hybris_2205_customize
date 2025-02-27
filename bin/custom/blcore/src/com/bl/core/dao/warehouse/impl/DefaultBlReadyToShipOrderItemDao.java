package com.bl.core.dao.warehouse.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlReadyToShipOrderItemDao;
import com.bl.core.model.ReadyToShipOrderItemModel;
import com.bl.core.utils.BlDateTimeUtils;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * It is used to get ReadyToShipOrderItem for morning pull orders view.
 *
 * @author Sunil
 */
public class DefaultBlReadyToShipOrderItemDao implements BlReadyToShipOrderItemDao {

  private FlexibleSearchService flexibleSearchService;
  private ModelService modelService;

  private static final String FIND_READY_TO_SHIP_ORDER_ITEMS_FOR_DATE_AND_WAREHOUSE =
      "SELECT {pk} FROM {" + ReadyToShipOrderItemModel._TYPECODE + "} WHERE {"
          + ReadyToShipOrderItemModel.WAREHOUSE + "} = ?warehouse AND {"
          + ReadyToShipOrderItemModel.SHIPDATE + "} BETWEEN ?startDate AND ?endDate ";

  private static final String FIND_READY_TO_SHIP_ORDER_ITEMS_FOR_WAREHOUSE =
      "SELECT {pk} FROM {" + ReadyToShipOrderItemModel._TYPECODE + "} WHERE {"
          + ReadyToShipOrderItemModel.WAREHOUSE + "} = ?warehouse ";

  /**
   * {@inheritDoc}
   */
  @Override
  public List<ReadyToShipOrderItemModel> getReadyToShipOrderItemsForDateAndWarehouse(final Date shipDate, final
  WarehouseModel warehouse) {

    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(FIND_READY_TO_SHIP_ORDER_ITEMS_FOR_DATE_AND_WAREHOUSE);

    addQueryParameter(shipDate, warehouse, fQuery);

    final SearchResult<ReadyToShipOrderItemModel> result = getFlexibleSearchService().search(fQuery);

   return result.getResult();

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<ReadyToShipOrderItemModel> getReadyToShipOrderItemsForWarehouse(final WarehouseModel warehouse) {

    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(FIND_READY_TO_SHIP_ORDER_ITEMS_FOR_WAREHOUSE);

    addQueryParameter(null, warehouse, fQuery);

    final SearchResult<ReadyToShipOrderItemModel> result = getFlexibleSearchService().search(fQuery);

    return result.getResult();

  }

  /**
   * It adds the parameters value into query
   * @param shipDate
   * @param warehouse
   * @param fQuery
   */
  private void addQueryParameter(final Date shipDate, final
  WarehouseModel warehouse, final FlexibleSearchQuery fQuery) {

    if (null != shipDate) {

      final Calendar startDate = BlDateTimeUtils.getFormattedStartDay(shipDate);
      final Calendar endDate = BlDateTimeUtils.getFormattedEndDay(shipDate);
      fQuery.addQueryParameter(BlCoreConstants.START_DATE, startDate.getTime());
      fQuery.addQueryParameter(BlCoreConstants.END_DATE, endDate.getTime());
    }

    fQuery.addQueryParameter(BlCoreConstants.WAREHOUSE, warehouse);
  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(
      final FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

}
