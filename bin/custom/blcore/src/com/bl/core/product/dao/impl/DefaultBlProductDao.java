package com.bl.core.product.dao.impl;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.catalog.enums.ArticleApprovalStatus;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.product.daos.impl.DefaultProductDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.logging.BlLogger;


/**
 * It is used to fetch the products
 *
 * @author Moumita
 */
public class DefaultBlProductDao extends DefaultProductDao implements BlProductDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlProductDao.class);

	private static final String GET_ALL_ACTIVE_SKU_PRODUCTS_QUERY = "SELECT {" + BlProductModel.PK + "} from {"
			+ BlProductModel._TYPECODE
			+ "} WHERE {" + BlProductModel.DISCONTINUED + "} = ?discontinued "
			+" AND {" + BlProductModel.APPROVALSTATUS + "} IN ({{SELECT {aas:PK} FROM {" + ArticleApprovalStatus._TYPECODE +
			" as aas} WHERE {aas:CODE} = (?approved)}}) "
			+ "AND {" + BlProductModel.CATALOGVERSION + "} IN ({{SELECT {cv:PK} FROM {" + CatalogVersionModel._TYPECODE +
			" as cv} WHERE {cv:VERSION} = ?version AND {cv:catalog} in ({{SELECT {c:pk} FROM {" + CatalogModel._TYPECODE +
			" as c} WHERE {c:id} = ?catalog}})}})";

	/**
	 * @param typecode
	 */
	public DefaultBlProductDao(final String typecode)
	{
		super(typecode);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<BlProductModel> getAllActiveSkuProducts()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ALL_ACTIVE_SKU_PRODUCTS_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.DISCONTINUED, false);
		fQuery.addQueryParameter(BlCoreConstants.APPROVED, ArticleApprovalStatus.APPROVED.getCode());
		fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CATALOG, BlCoreConstants.CATALOG_VALUE);
		fQuery.addQueryParameter(BlCoreConstants.VERSION, BlCoreConstants.STAGED);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<BlProductModel> skuProducts = result.getResult();
		if (CollectionUtils.isEmpty(skuProducts))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No Active sku products found");
			return Collections.emptyList();
		}
		return skuProducts;
	}
}
