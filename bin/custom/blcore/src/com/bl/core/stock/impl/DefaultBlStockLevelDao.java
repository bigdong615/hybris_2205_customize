package com.bl.core.stock.impl;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.stock.impl.DefaultStockLevelDao;

import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;


/**
 * This class is used to get the inventory for a product
 *
 * @author Moumita
 */
public class DefaultBlStockLevelDao extends DefaultStockLevelDao implements BlStockLevelDao {

  private static final Logger LOG = Logger.getLogger(DefaultBlStockLevelDao.class);
  private static final String SELECT = "SELECT {";
  private static final String FROM = "} from {";
  private static final String AND = "AND {";
  private static final String WHERE = "} WHERE {";
  private static final String PRODUCT_CODE_PARAM = "} = ?productCode ";
  private static final String DATE_PARAM = "} BETWEEN ?startDate AND ?endDate ";

  private static final String STOCK_LEVEL_FOR_DATE_QUERY = SELECT + ItemModel.PK + FROM
      + StockLevelModel._TYPECODE + WHERE + StockLevelModel.PRODUCTCODE + PRODUCT_CODE_PARAM +
      AND + StockLevelModel.DATE + DATE_PARAM +
      AND + StockLevelModel.SERIALSTATUS + "} IN ({{SELECT {sse:PK} FROM {"
      + SerialStatusEnum._TYPECODE +
      " as sse} WHERE {sse:CODE} = (?active)}}) " +
      AND + StockLevelModel.WAREHOUSE + "} IN (?warehouses)";

  private static final String STOCK_LEVELS_FOR_PRODUCTS_AND_DATE_QUERY =
      SELECT + ItemModel.PK + FROM
          + StockLevelModel._TYPECODE + WHERE + StockLevelModel.PRODUCTCODE
          + "} IN (?productCodes) " +
          AND + StockLevelModel.DATE + DATE_PARAM +
          AND + StockLevelModel.SERIALSTATUS + "} IN ({{SELECT {sse:PK} FROM {"
          + SerialStatusEnum._TYPECODE +
          " as sse} WHERE {sse:CODE} = (?active)}}) " +
          AND + StockLevelModel.RESERVEDSTATUS + "} = ?reservedStatus " +
          AND + StockLevelModel.WAREHOUSE + "} = ?warehouse ";

  private static final String SERIAL_STOCK_LEVEL_FOR_DATE_QUERY = SELECT + ItemModel.PK + FROM
      + StockLevelModel._TYPECODE + WHERE + StockLevelModel.DATE + DATE_PARAM +
      AND + StockLevelModel.SERIALPRODUCTCODE + "} = ?serialProductCode";

  private static final String SERIAL_STOCK_LEVELS_FOR_DATE_AND_CODES_QUERY =
      SELECT + ItemModel.PK + FROM
          + StockLevelModel._TYPECODE + WHERE + StockLevelModel.DATE + DATE_PARAM +
          AND + StockLevelModel.SERIALPRODUCTCODE + "} IN (?serialProductCodes) " +
          AND + StockLevelModel.RESERVEDSTATUS + "} = ?reservedStatus ";

  private static final String USED_GEAR_SERIAL_STOCK_LEVEL = SELECT + ItemModel.PK + FROM
      + StockLevelModel._TYPECODE + WHERE + StockLevelModel.SERIALPRODUCTCODE
      + "} = ?serialProductCode";

  private static final String USED_GEAR_SERIAL_ASSIGNED_TO_RENTAL_ORDER_QUERY =
      SELECT + ItemModel.PK + FROM
          + StockLevelModel._TYPECODE + WHERE + StockLevelModel.DATE
          + DATE_PARAM + AND + StockLevelModel.SERIALPRODUCTCODE + "} = ?serialProductCode " + AND
          + StockLevelModel.RESERVEDSTATUS + "} = ?reservedStatus";


  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<StockLevelModel> findStockLevelForDate(final String productCode,
      final Collection<WarehouseModel> warehouseModels,
      final Date startDay, final Date endDay) {
    if (CollectionUtils.isEmpty(warehouseModels)) {
      throw new IllegalArgumentException("warehouses cannot be null.");
    } else {
      //Filters warehouse list to remove null's and duplicate elements.
      final Set<WarehouseModel> warehouses = new HashSet<>(warehouseModels);
      final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(STOCK_LEVEL_FOR_DATE_QUERY);

      fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CODE, productCode);
      addQueryParameter(startDay, endDay, fQuery);

      fQuery.addQueryParameter(BlCoreConstants.ACTIVE, SerialStatusEnum.ACTIVE.getCode());
      fQuery.addQueryParameter(BlCoreConstants.WAREHOUSES, warehouses);
      final SearchResult result = getFlexibleSearchService().search(fQuery);
      final List<StockLevelModel> stockLevels = result.getResult();
      if (CollectionUtils.isEmpty(stockLevels)) {
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "No Stock Levels found for product : {} and date between : {} and {}",
            productCode, startDay, endDay);
        return Collections.emptyList();
      }
      return stockLevels;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<StockLevelModel> findSerialStockLevelForDate(final String serialProductCode,
      final Date startDay, final Date endDay) {
    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(SERIAL_STOCK_LEVEL_FOR_DATE_QUERY);
    fQuery.addQueryParameter(BlCoreConstants.SERIAL_PRODUCT_CODE, serialProductCode);

    addQueryParameter(startDay, endDay, fQuery);

    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<StockLevelModel> stockLevels = result.getResult();
    if (CollectionUtils.isEmpty(stockLevels)) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "No Stock Levels found for serial product {} with date between : {} and {}",
          serialProductCode, startDay, endDay);
      return Collections.emptyList();
    }
    return stockLevels;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<StockLevelModel> findSerialStockLevelsForDateAndCodes(
      final Set<String> serialProductCodes,
      final Date startDay, final Date endDay) {
    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
        SERIAL_STOCK_LEVELS_FOR_DATE_AND_CODES_QUERY);
    fQuery.addQueryParameter("serialProductCodes", serialProductCodes);
    addQueryParameter(startDay, endDay, fQuery);
    fQuery.addQueryParameter("reservedStatus", Boolean.FALSE);
    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<StockLevelModel> stockLevels = result.getResult();
    if (CollectionUtils.isEmpty(stockLevels)) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "No Stock Levels found for serial products {} with date between : {} and {}",
          serialProductCodes, startDay, endDay);
      return Collections.emptyList();
    }
    return stockLevels;
  }

  /**
   * It adds the parameters value into query
   *
   * @param startDay
   * @param endDay
   * @param fQuery
   */
  private void addQueryParameter(final Date startDay, final Date endDay,
      final FlexibleSearchQuery fQuery) {
    final Calendar startDate = BlDateTimeUtils.getFormattedStartDay(startDay);
    fQuery.addQueryParameter(BlCoreConstants.START_DATE, startDate.getTime());

    final Calendar endDate = BlDateTimeUtils.getFormattedEndDay(endDay);
    fQuery.addQueryParameter(BlCoreConstants.END_DATE, endDate.getTime());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public StockLevelModel findStockLevelForUsedGearSerial(final String serialProductCode) {
    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(USED_GEAR_SERIAL_STOCK_LEVEL);
    fQuery.addQueryParameter(BlCoreConstants.SERIAL_PRODUCT_CODE, serialProductCode);

    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<StockLevelModel> stockLevels = result.getResult();
    if (CollectionUtils.isEmpty(stockLevels)) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "No Stock Levels found for serial product code {} ",
          serialProductCode);
      return null;
    }
    return stockLevels.get(0);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isUsedGearSerialNotAssignedToAnyRentalOrders(final String serialProductCode,
      final Date startDay, final Date endDay) {
    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
        USED_GEAR_SERIAL_ASSIGNED_TO_RENTAL_ORDER_QUERY);
    fQuery.addQueryParameter(BlCoreConstants.SERIAL_PRODUCT_CODE, serialProductCode);
    fQuery.addQueryParameter(BlCoreConstants.RESERVED_STATUS, Boolean.TRUE);

    addQueryParameter(startDay, endDay, fQuery);

    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<StockLevelModel> stockLevels = result.getResult();
    if (CollectionUtils.isEmpty(stockLevels)) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "The used gear serial product : {} is not assigned to any rental orders with "
              + "date between : {} and {}",
          serialProductCode, startDay, endDay);
      return Boolean.TRUE;
    }
    return Boolean.FALSE;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<StockLevelModel> findStockLevelsForProductCodesAndDate(
      final List<String> productCodes,
      final List<WarehouseModel> warehouse, final Date startDate, final Date endDate) {
    if (null == warehouse) {
      throw new IllegalArgumentException("warehouses cannot be null.");
    } else {
      final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
          STOCK_LEVELS_FOR_PRODUCTS_AND_DATE_QUERY);
      fQuery.addQueryParameter("productCodes", productCodes);
      addQueryParameter(startDate, endDate, fQuery);
      fQuery.addQueryParameter(BlCoreConstants.ACTIVE, SerialStatusEnum.ACTIVE.getCode());
      fQuery.addQueryParameter("warehouses", warehouse);

      final SearchResult result = getFlexibleSearchService().search(fQuery);
      final List<StockLevelModel> stockLevels = result.getResult();
      if (CollectionUtils.isEmpty(stockLevels)) {
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "No Stock Levels found for product codes : {} and date between : {} and {}",
            productCodes, startDate, endDate);
        return Collections.emptyList();
      }
      return stockLevels;
    }
  }

  /**
   * It finds the stock levels for the given product codes and date range.
   *
   * @param productCodes the product codes
   * @param warehouse    the warehouse
   * @param startDate    the start date
   * @param endDate      the end date
   * @return list of stock levels
   */
  @Override
  public Collection<StockLevelModel> findStockLevelsForProductCodesAndDate(Set<String> productCodes,
      WarehouseModel warehouse, Date startDate, Date endDate) {
    if (null == warehouse) {
      throw new IllegalArgumentException("warehouse cannot be null.");
    } else {

      final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
          STOCK_LEVELS_FOR_PRODUCTS_AND_DATE_QUERY);
      fQuery.addQueryParameter("productCodes", productCodes);
      fQuery.addQueryParameter(BlCoreConstants.START_DATE,
          BlDateTimeUtils.getFormattedStartDay(startDate).getTime());
      fQuery.addQueryParameter(BlCoreConstants.END_DATE,
          BlDateTimeUtils.getFormattedEndDay(endDate).getTime());
      fQuery.addQueryParameter(BlCoreConstants.ACTIVE, SerialStatusEnum.ACTIVE.getCode());
      fQuery.addQueryParameter("reservedStatus", Boolean.FALSE);
      fQuery.addQueryParameter("warehouse", warehouse);

      final List<StockLevelModel> stockLevels = (List<StockLevelModel>)(List<?>)getFlexibleSearchService().search(fQuery).getResult();
      if (CollectionUtils.isEmpty(stockLevels)) {
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "No Stock Levels found for product codes : {} and date between : {} and {}",
            productCodes, startDate, endDate);
        return Collections.emptyList();
      }
      return stockLevels;
    }
  }

}
