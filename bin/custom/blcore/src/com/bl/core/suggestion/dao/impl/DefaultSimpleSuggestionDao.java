/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.suggestion.dao.impl;

import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.internal.dao.AbstractItemDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import com.bl.core.suggestion.dao.SimpleSuggestionDao;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.assertj.core.util.Preconditions;
import org.springframework.util.Assert;


/**
 * Default implementation of {@link SimpleSuggestionDao}.
 * <p>
 * Finds products that are related products that the user has bought.
 */
public class DefaultSimpleSuggestionDao extends AbstractItemDao implements SimpleSuggestionDao {

  private static final int DEFAULT_LIMIT = 100;
  private static final String REF_QUERY_PARAM_CATEGORY = "category";
  private static final String REF_QUERY_PARAM_PRODUCTS = "products";
  private static final String REF_QUERY_PARAM_USER = "user";
  private static final String REF_QUERY_PARAM_TYPE = "referenceType";
  private static final String REF_QUERY_PARAM_TYPES = "referenceTypes";

  private static final String REF_QUERY_CATEGORY_START = new StringBuilder("SELECT {p.PK}")
      .append(" FROM {Product AS p")
      .append(" LEFT JOIN ProductReference AS r ON {p.PK}={r.target}")
      .append(" LEFT JOIN OrderEntry AS e ON {r.source}={e.product}")
      .append(" LEFT JOIN Order AS o ON {e.order}={o.PK}")
      .append(" LEFT JOIN CategoryProductRelation AS c2p ON {r.source}={c2p.target}")
      .append(" LEFT JOIN Category AS c ON {c2p.source}={c.PK} }")
      .append(" WHERE {o.user}=?user AND {c.PK}=?category").toString();

  private static final String REF_QUERY_PRODUCT_START =
      new StringBuilder("SELECT DISTINCT {p.PK}, COUNT({p.PK}) AS NUM")
          .append(" FROM {Product AS p")
          .append(" LEFT JOIN ProductReference AS r ON {p.PK}={r.target} }")
          .append(" WHERE {r.source} IN (?products) AND {r.target} NOT IN (?products)").toString();

  private static final String REF_QUERY_TYPE = " AND {r.referenceType} IN (?referenceType)";
  private static final String REF_QUERY_TYPES = " AND {r.referenceType} IN (?referenceTypes)";
  private static final String REF_QUERY_SUB = new StringBuilder(" AND NOT EXISTS ({{")
      .append(" SELECT 1 FROM {OrderEntry AS e2 LEFT JOIN Order AS o2 ON {e2.order}={o2.PK} } ")
      .append(" WHERE {e2.product}={r.target} AND {o2.user}=?user }})").toString();

  private static final String REF_QUERY_CATEGORY_ORDER = " ORDER BY {o.creationTime} DESC";

  private static final String REF_QUERY_PRODUCT_GROUP = " GROUP BY {p.PK}";
  private static final String REF_QUERY_PRODUCT_ORDER = " ORDER BY NUM DESC";


  @Override
  public List<ProductModel> findProductsRelatedToPurchasedProductsByCategory(
      final CategoryModel category, final List<ProductReferenceTypeEnum> referenceTypes,
      final UserModel user, final boolean excludePurchased, final Integer limit) {
		Preconditions.checkNotNull(category);
		Preconditions.checkNotNull(user);
    final int maxResultCount = limit == null ? DEFAULT_LIMIT : limit;

    final Map<String, Object> params = new HashMap<>();
    final StringBuilder builder = new StringBuilder(REF_QUERY_CATEGORY_START);
    if (excludePurchased) {
      builder.append(REF_QUERY_SUB);
    }
    if (CollectionUtils.isNotEmpty(referenceTypes)) {
      builder.append(REF_QUERY_TYPES);
      params.put(REF_QUERY_PARAM_TYPES, referenceTypes);
    }
    builder.append(REF_QUERY_CATEGORY_ORDER);

    params.put(REF_QUERY_PARAM_USER, user);
    params.put(REF_QUERY_PARAM_CATEGORY, category);

    final FlexibleSearchQuery query = new FlexibleSearchQuery(builder.toString());
    query.addQueryParameters(params);
    query.setNeedTotal(false);
    query.setCount(maxResultCount);

    final SearchResult<ProductModel> result = getFlexibleSearchService().search(query);
    return result.getResult();
  }

  @Override
  public List<ProductModel> findProductsRelatedToProducts(final List<ProductModel> products,
      final List<ProductReferenceTypeEnum> referenceTypes, final UserModel user,
      final boolean excludePurchased,
      final Integer limit) {
    Preconditions.checkNotNull(products);
    Preconditions.checkNotNull(user);

    final int maxResultCount = limit == null ? DEFAULT_LIMIT : limit;

    final Map<String, Object> params = new HashMap<>();
    final StringBuilder builder = new StringBuilder(REF_QUERY_PRODUCT_START);
    if (excludePurchased) {
      builder.append(REF_QUERY_SUB);
    }
    if (CollectionUtils.isNotEmpty(referenceTypes)) {
      builder.append(REF_QUERY_TYPES);
      params.put(REF_QUERY_PARAM_TYPES, referenceTypes);
    }
    builder.append(REF_QUERY_PRODUCT_GROUP);
    builder.append(REF_QUERY_PRODUCT_ORDER);

    params.put(REF_QUERY_PARAM_USER, user);
    params.put(REF_QUERY_PARAM_PRODUCTS, products);

    final FlexibleSearchQuery query = new FlexibleSearchQuery(builder.toString());
    query.addQueryParameters(params);
    query.setNeedTotal(false);
    query.setCount(maxResultCount);

    final SearchResult<ProductModel> result = getFlexibleSearchService().search(query);
    return result.getResult();
  }

  /**
   * @deprecated Since 5.0. Use {@link #findProductsRelatedToPurchasedProductsByCategory(CategoryModel,
   * List, UserModel, boolean, Integer)}
   */
  @Deprecated(since = "5.0", forRemoval = true)
  @Override
  public List<ProductModel> findProductsRelatedToPurchasedProductsByCategory(
      final CategoryModel category, final UserModel user,
      final ProductReferenceTypeEnum referenceType, final boolean excludePurchased,
      final Integer limit) {
    Assert.notNull(category);
    Assert.notNull(user);

    final int maxResultCount = limit == null ? DEFAULT_LIMIT : limit;

    final Map<String, Object> params = new HashMap<>();
    final StringBuilder builder = new StringBuilder(REF_QUERY_CATEGORY_START);
    if (excludePurchased) {
      builder.append(REF_QUERY_SUB);
    }
    if (referenceType != null) {
      builder.append(REF_QUERY_TYPE);
      params.put(REF_QUERY_PARAM_TYPE, referenceType);
    }
    builder.append(REF_QUERY_CATEGORY_ORDER);

    params.put(REF_QUERY_PARAM_USER, user);
    params.put(REF_QUERY_PARAM_CATEGORY, category);

    final FlexibleSearchQuery query = new FlexibleSearchQuery(builder.toString());
    query.addQueryParameters(params);
    query.setNeedTotal(false);
    query.setCount(maxResultCount);

    final SearchResult<ProductModel> result = getFlexibleSearchService().search(query);
    return result.getResult();
  }
}
