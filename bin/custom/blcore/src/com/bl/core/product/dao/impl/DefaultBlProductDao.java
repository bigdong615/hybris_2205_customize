package com.bl.core.product.dao.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.catalog.enums.ArticleApprovalStatus;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.product.daos.impl.DefaultProductDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.BlSubpartsModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.logging.BlLogger;


/**
 * It is used to fetch the products
 *
 * @author Moumita
 */
public class DefaultBlProductDao extends DefaultProductDao implements BlProductDao {

  private static final Logger LOG = Logger.getLogger(DefaultBlProductDao.class);

  private static final String GET_ALL_ACTIVE_SKU_PRODUCTS_QUERY =
		  "SELECT {" + BlProductModel.PK + "} from {"
          + BlProductModel._TYPECODE
				  + " as p join EnumerationValue as enum  on {enum.pk}={p.productType}"
          + "} WHERE {" + BlProductModel.DISCONTINUED + "} = ?discontinued "
			 + " AND {enum.code}!= ?subParts"
          + " AND {" + BlProductModel.APPROVALSTATUS + "} IN ({{SELECT {aas:PK} FROM {"
          + ArticleApprovalStatus._TYPECODE +
          " as aas} WHERE {aas:CODE} = (?approved)}}) "
          + "AND {" + BlProductModel.CATALOGVERSION + "} IN ({{SELECT {cv:PK} FROM {"
          + CatalogVersionModel._TYPECODE +
          " as cv} WHERE {cv:VERSION} = ?version AND {cv:catalog} in ({{SELECT {c:pk} FROM {"
          + CatalogModel._TYPECODE +
          " as c} WHERE {c:id} = ?catalog}})}})";

  private static final String GET_BLSERIALPRODUCTS_FOR_CODES_QUERY =
      "SELECT {pk} from {"
          + BlSerialProductModel._TYPECODE
          + " as p} WHERE {p:code} IN (?codes)"
          + " AND {p:approvalStatus} IN ({{SELECT {aas:PK} FROM {" + ArticleApprovalStatus._TYPECODE
          + " as aas} WHERE {aas:CODE} = (?approved)}}) "
          + " AND {p:catalogVersion} IN ({{SELECT {cv:PK} FROM {" + CatalogVersionModel._TYPECODE +
          " as cv} WHERE {cv:VERSION} = ?version AND {cv:catalog} in ({{SELECT {c:pk} FROM {"
          + CatalogModel._TYPECODE +
          " as c} WHERE {c:id} = ?catalog}})}})";

  private static final String GET_BLSERIALPRODUCTS_FOR_BARCODE_QUERY = "SELECT {pk} from {" + BlSerialProductModel._TYPECODE
			+ " as p} WHERE {p:" + BlSerialProductModel.BARCODE + "} = ?barcode" + " AND {p:" + BlSerialProductModel.CATALOGVERSION
			+ "} IN ({{SELECT {cv:PK} FROM {" + CatalogVersionModel._TYPECODE + " as cv} WHERE {cv:" + CatalogVersionModel.VERSION
			+ "} = ?version AND {cv:" + CatalogVersionModel.CATALOG + "} in ({{SELECT {c:pk} FROM {" + CatalogModel._TYPECODE
			+ " as c} WHERE {c:" + CatalogModel.ID + "} = ?catalog}})}})";

  private static final String GET_BLSERIALPRODUCTS_FOR_CODE_QUERY = "SELECT {pk} from {" + BlSerialProductModel._TYPECODE
			+ " as p} WHERE {p:" + BlSerialProductModel.CODE + "} = ?serialCode" + " AND {p:" + BlSerialProductModel.CATALOGVERSION
			+ "} IN ({{SELECT {cv:PK} FROM {" + CatalogVersionModel._TYPECODE + " as cv} WHERE {cv:" + CatalogVersionModel.VERSION
			+ "} = ?version AND {cv:" + CatalogVersionModel.CATALOG + "} in ({{SELECT {c:pk} FROM {" + CatalogModel._TYPECODE
			+ " as c} WHERE {c:" + CatalogModel.ID + "} = ?catalog}})}})";

  private static final String GET_SUBPARTS_FOR_PRODUCT_MAPPING_QUERY = "select {sp.pk} from {BlSubparts as sp join blProduct as p on {sp.subpartProduct}={p.pk}} where {p.code}=?productCode and {sp.quantity} =?quantity";

  private static final String GET_USED_PRODUCTS_ON_SALE_QUERY = "SELECT {p.pk} from {blProduct as p JOIN Catalogversion as cv ON {cv.pk}={p.catalogversion} and {cv.version} like ?version JOIN Catalog as c ON {c.pk}={cv.catalog} and {c.id} like ?catalogCode join EnumerationValue as enum  on {enum.pk}={p.approvalStatus}} where {p.forSale} = ?isForSale and {p.isNew} = ?isNew and {enum.code}=?approved";

  private static final String GET_STOCKLEVELS_FOR_SERIAL_QUERY = "SELECT {s.pk} from {stockLevel as s} where {s.serialProductCode} = ?serialCode";

  private static final String GET_BLSERIALPRODUCTS_FOR_SERIAL_ID_QUERY = "SELECT {pk} from {" + BlSerialProductModel._TYPECODE
            + " as p} WHERE {p:" + BlSerialProductModel.PRODUCTID + "} = ?productId" + " AND {p:" + BlSerialProductModel.CATALOGVERSION
            + "} IN ({{SELECT {cv:PK} FROM {" + CatalogVersionModel._TYPECODE + " as cv} WHERE {cv:" + CatalogVersionModel.CATALOG + "} in ({{SELECT {c:pk} FROM {" + CatalogModel._TYPECODE
            + " as c} WHERE {c:" + CatalogModel.ID + "} = ?catalog}})}})";

  private static final String GET_ORDER_COUNT_FOR_PRODUCT = "select {o.pk} from {order as o "
		  + "join AbstractOrderEntry as ao on {o.pk} = {ao.order} " 
		  + "join Product as p on {p.pk} = {ao.product} "
		  + "} where {p.code} = ?code and {o.creationtime} > ?formattedDate ";

    /**
   * @param typecode
   */
  public DefaultBlProductDao(final String typecode) {
    super(typecode);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<BlProductModel> getAllActiveSkuProducts() {
    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ALL_ACTIVE_SKU_PRODUCTS_QUERY);
    fQuery.addQueryParameter(BlCoreConstants.DISCONTINUED, false);
    fQuery.addQueryParameter(BlCoreConstants.APPROVED, ArticleApprovalStatus.APPROVED.getCode());
    fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CATALOG, BlCoreConstants.CATALOG_VALUE);
    fQuery.addQueryParameter(BlCoreConstants.VERSION, BlCoreConstants.STAGED);
	 fQuery.addQueryParameter("subParts", "SUBPARTS");
    final SearchResult result = getFlexibleSearchService().search(fQuery);
    final List<BlProductModel> skuProducts = result.getResult();
    if (CollectionUtils.isEmpty(skuProducts)) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No Active sku products found");
      return Collections.emptyList();
    }
    return skuProducts;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<BlSerialProductModel> getBlSerialProductsForCodes(
      final Set<String> serialProductCodes) {

    final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
        GET_BLSERIALPRODUCTS_FOR_CODES_QUERY);
    fQuery.addQueryParameter(BlCoreConstants.CODES, serialProductCodes);
    fQuery.addQueryParameter(BlCoreConstants.APPROVED, ArticleApprovalStatus.APPROVED.getCode());
    fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CATALOG, BlCoreConstants.CATALOG_VALUE);
    fQuery.addQueryParameter(BlCoreConstants.VERSION, BlCoreConstants.ONLINE);

    final SearchResult<BlSerialProductModel> result = getFlexibleSearchService().search(fQuery);
    final List<BlSerialProductModel> serialProducts = result.getResult();

    if (CollectionUtils.isEmpty(serialProducts)) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No serial products found for codes: {}",
          serialProductCodes);
      return Collections.emptyList();
    }

    return serialProducts;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BlSerialProductModel getSerialByBarcode(final String serialBarcode)
  {
	  final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_BLSERIALPRODUCTS_FOR_BARCODE_QUERY);
	  fQuery.addQueryParameter(BlCoreConstants.BARCODE, serialBarcode);
	  fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CATALOG, BlCoreConstants.CATALOG_VALUE);
	  fQuery.addQueryParameter(BlCoreConstants.VERSION, BlCoreConstants.ONLINE);
	  final SearchResult<BlSerialProductModel> result = getFlexibleSearchService().search(fQuery);
	  final List<BlSerialProductModel> serialProducts = result.getResult();
	  if (CollectionUtils.isEmpty(serialProducts))
	  {
		  BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No serial product found for barcode: {}",
				  serialBarcode);
		  return null;
	  }
	  return serialProducts.get(0);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BlSerialProductModel getSerialByID(final String serialID)
  {
	  final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_BLSERIALPRODUCTS_FOR_SERIAL_ID_QUERY);
	  fQuery.addQueryParameter(BlCoreConstants.PRODUCT_ID, serialID);
	  fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CATALOG, BlCoreConstants.CATALOG_VALUE);
	  final SearchResult<BlSerialProductModel> result = getFlexibleSearchService().search(fQuery);
	  final List<BlSerialProductModel> serialProducts = result.getResult();
	  if (CollectionUtils.isEmpty(serialProducts))
	  {
		  BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No serial product found for serialId: {}",
				  serialID);
		  return null;
	  }
	  return serialProducts.iterator().next();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BlSerialProductModel getSerialBySerialCode(final String serialCode)
  {
	  final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_BLSERIALPRODUCTS_FOR_CODE_QUERY);
	  fQuery.addQueryParameter(BlCoreConstants.SERIAL_CODE, serialCode);
	  fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CATALOG, BlCoreConstants.CATALOG_VALUE);
	  fQuery.addQueryParameter(BlCoreConstants.VERSION, BlCoreConstants.ONLINE);
	  final SearchResult<BlSerialProductModel> result = getFlexibleSearchService().search(fQuery);
	  final List<BlSerialProductModel> serialProducts = result.getResult();
	  if (CollectionUtils.isEmpty(serialProducts))
	  {
		  BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No serial product found for serialCode: {}",
				  serialCode);
		  return null;
	  }
	  return serialProducts.iterator().next();
  }

  @Override
  public BlSubpartsModel getBlSubPartsPk(final String productCode, final Integer quantity)
  {
	  Validate.notNull(productCode, "Product must not be null", null);
	  Validate.notNull(quantity, "Quantity must not be null", null);

	  final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_SUBPARTS_FOR_PRODUCT_MAPPING_QUERY);
	  fQuery.addQueryParameter("productCode", productCode);
	  fQuery.addQueryParameter("quantity", quantity);
	  final SearchResult<BlSubpartsModel> result = getFlexibleSearchService().search(fQuery);
	  return result.getResult().get(0);
  }

  @Override
  public List<BlProductModel> getUsedProductsOnSale()
  {
	  final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_USED_PRODUCTS_ON_SALE_QUERY);
	  fQuery.addQueryParameter("isForSale", Boolean.TRUE);
	  fQuery.addQueryParameter("isNew", Boolean.FALSE);
	  fQuery.addQueryParameter("catalogCode", "blProductCatalog");
	  fQuery.addQueryParameter("version", "Online");
	  fQuery.addQueryParameter("approved", "approved");
	  final SearchResult<BlProductModel> result = getFlexibleSearchService().search(fQuery);
	  return result.getResult();
  }

  @Override
  public List<StockLevelModel> getStockLevelsForSerial(final String serialCode)
  {
	  final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_STOCKLEVELS_FOR_SERIAL_QUERY);
	  fQuery.addQueryParameter("serialCode", serialCode);
	  final SearchResult<StockLevelModel> result = getFlexibleSearchService().search(fQuery);
	  return result.getResult();
  }

  @Override
  public List<ProductModel> findProductsByPK(final String code)
  {
	  validateParameterNotNull(code, "Product code must not be null!");
	  return find(Collections.singletonMap(ProductModel.PK, (Object) code));
  }

  @Override
  public int getOrderCountByProduct(final String code)
  {

	  final LocalDate today = LocalDate.now();
	  final LocalDate ninetyDaysAgo = today.minusDays(90);
	  final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
	  final String formattedDate = ninetyDaysAgo.format(formatter);
	  final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDER_COUNT_FOR_PRODUCT);
	  fQuery.addQueryParameter("code", code);
	  fQuery.addQueryParameter("formattedDate", formattedDate);
	  final SearchResult<OrderModel> result = getFlexibleSearchService().search(fQuery);
	  final int orderCount;
	  if (result.getResult() != null)
	  {
		  return result.getResult().size();
	  }
	  else
	  {
		  return 0;
	  }
  }

}
