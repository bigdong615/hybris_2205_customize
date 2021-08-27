package com.bl.core.order.dao.impl;

import static de.hybris.platform.core.model.ItemModel.MODIFIEDTIME;
import static de.hybris.platform.core.model.ItemModel.PK;
import static de.hybris.platform.core.model.order.AbstractOrderModel.CODE;
import static de.hybris.platform.core.model.order.AbstractOrderModel.STATUS;
import static de.hybris.platform.core.model.order.AbstractOrderModel.STORE;
import static de.hybris.platform.core.model.order.AbstractOrderModel.USER;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.commerceservices.customer.dao.impl.DefaultCustomerAccountDao;
import de.hybris.platform.commerceservices.search.flexiblesearch.data.SortQueryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.util.Config;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;

/**
 * This DAO is created to override OOB method to get list of orders based on modification time
 * @author Manikandan
 */
public class DefaultBlCustomerAccountDao extends DefaultCustomerAccountDao {

  private static final String FIND_ORDERS_BY_CUSTOMER_STORE_QUERY = "SELECT {" + PK + "}, {"
      + MODIFIEDTIME + "}, {" + CODE + "} FROM {" + OrderModel._TYPECODE + "} WHERE {" + USER
      + "} = ?customer AND {" + OrderModel.VERSIONID + "} IS NULL AND {" + STORE + "} = ?store "
      + Config.getString("commerceservices.additional.order.status.filter", "");

  private static final String FIND_ORDERS_BY_CUSTOMER_STORE_QUERY_AND_STATUS = FIND_ORDERS_BY_CUSTOMER_STORE_QUERY + " AND {"
      + STATUS + "} IN (?statusList)";

  private static final String FILTER_ORDER_STATUS = " AND {" + STATUS + "} NOT IN (?filterStatusList)";

  private static final String SORT_ORDERS_BY_DATE = " ORDER BY {" + MODIFIEDTIME + "} DESC, {" + PK + "}";

  private static final String SORT_ORDERS_BY_CODE = " ORDER BY {" + CODE + "},{" + MODIFIEDTIME
      + "} DESC, {" + PK + "}";


  /**
   * This DAO is created to override OOB method to get list of orders based on modification time
   * @param customerModel curren customer
   * @param store current store
   * @param status order status
   * @param pageableData pageable date
   * @return SearchPageData<OrderModel>
   */
  @Override
  public SearchPageData<OrderModel> findOrdersByCustomerAndStore(final CustomerModel customerModel, final BaseStoreModel store, // NOSONAR
      final OrderStatus[] status, final PageableData pageableData)  // NOSONAR
  {
    validateParameterNotNull(customerModel, "Customer must not be null");
    validateParameterNotNull(store, "Store must not be null");

    final Map<String, Object> queryParams = new HashMap<>();
    queryParams.put(BlCoreConstants.CUSTOMER, customerModel);
    queryParams.put(BlCoreConstants.STORE, store);

    String filterClause = StringUtils.EMPTY;
    if (CollectionUtils.isNotEmpty(getFilterOrderStatusList()))
    {
      queryParams.put(BlCoreConstants.FILTER_STATUS_LIST, getFilterOrderStatusList());
      filterClause = FILTER_ORDER_STATUS;
    }

    final List<SortQueryData> sortQueries;

    if (ArrayUtils.isNotEmpty(status))
    {
      queryParams.put(BlCoreConstants.STATUS_LIST, Arrays.asList(status));
      sortQueries = Arrays.asList(
          createSortQueryData(BlCoreConstants.BY_DATE,
              createQuery(FIND_ORDERS_BY_CUSTOMER_STORE_QUERY_AND_STATUS, filterClause, SORT_ORDERS_BY_DATE)),
          createSortQueryData(BlCoreConstants.BY_ORDER_NUMBER,
              createQuery(FIND_ORDERS_BY_CUSTOMER_STORE_QUERY_AND_STATUS, filterClause, SORT_ORDERS_BY_CODE)));
    }
    else
    {
      sortQueries = Arrays
          .asList(
              createSortQueryData(BlCoreConstants.BY_DATE,
                  createQuery(FIND_ORDERS_BY_CUSTOMER_STORE_QUERY, filterClause, SORT_ORDERS_BY_DATE)),
              createSortQueryData(BlCoreConstants.BY_ORDER_NUMBER,
                  createQuery(FIND_ORDERS_BY_CUSTOMER_STORE_QUERY, filterClause, SORT_ORDERS_BY_CODE)));
    }

    return getPagedFlexibleSearchService().search(sortQueries, BlCoreConstants.BY_DATE, queryParams, pageableData);
  }


}
