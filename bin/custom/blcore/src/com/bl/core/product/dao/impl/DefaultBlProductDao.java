package com.bl.core.product.dao.impl;

import de.hybris.platform.catalog.enums.ArticleApprovalStatus;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

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
public class DefaultBlProductDao implements BlProductDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlProductDao.class);

	private static final String GET_ALL_ACTIVE_SKU_PRODUCTS_QUERY = "SELECT {" + BlProductModel.PK + "} from {"
			+ BlProductModel._TYPECODE
			+ "} WHERE {" + BlProductModel.DISCONTINUED + "} = ?discontinued "
			+" AND {" + BlProductModel.APPROVALSTATUS + "} IN ({{SELECT {aas:PK} FROM {" + ArticleApprovalStatus._TYPECODE +
			" as aas} WHERE {aas:CODE} = (?approved)}}) ";

	private FlexibleSearchService flexibleSearchService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<BlProductModel> getAllActiveSkuProducts()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ALL_ACTIVE_SKU_PRODUCTS_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.DISCONTINUED, false);
		fQuery.addQueryParameter(BlCoreConstants.APPROVED, ArticleApprovalStatus.APPROVED.getCode());
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<BlProductModel> skuProducts = result.getResult();
		if (CollectionUtils.isEmpty(skuProducts))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No Active sku products found");
			return Collections.emptyList();
		}
		return skuProducts;
	}

	/**
	 * @return the flexibleSearchService
	 */
	public FlexibleSearchService getFlexibleSearchService()
	{
		return flexibleSearchService;
	}

	/**
	 * @param flexibleSearchService
	 *           the flexibleSearchService to set
	 */
	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
	{
		this.flexibleSearchService = flexibleSearchService;
	}

}
