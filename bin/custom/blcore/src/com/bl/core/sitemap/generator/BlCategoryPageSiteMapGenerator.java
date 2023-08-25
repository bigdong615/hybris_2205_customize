/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.sitemap.generator;

import de.hybris.platform.acceleratorservices.sitemap.data.SiteMapUrlData;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.cms2.model.site.CMSSiteModel;
import de.hybris.platform.converters.Converters;
import org.apache.commons.collections4.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;


public class BlCategoryPageSiteMapGenerator extends BlAbstractSiteMapGenerator<CategoryModel>
{
	@Override
	public List<SiteMapUrlData> getSiteMapUrlData(final List<CategoryModel> models)
	{
		return Converters.convertAll(models, getSiteMapUrlDataConverter());
	}

	@Override
	protected List<CategoryModel> getDataInternal(final CMSSiteModel siteModel) {
		final String query = "SELECT {c.pk} FROM {Category AS c JOIN CatalogVersion AS cv ON {c.catalogVersion}={cv.pk} "
				+ " JOIN Catalog AS cat ON {cv.pk}={cat.activeCatalogVersion} "
				+ " JOIN CMSSite AS site ON {cat.pk}={site.defaultCatalog}}  WHERE {site.pk} = ?site "
				+ " AND {c.active} = '1' "
				+ " AND NOT exists ({{select {cr.pk} from {CategoriesForRestriction as cr} where {cr.target} = {c.pk} }})";

		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("site", siteModel);
		List<CategoryModel> categoryModels = doSearch(query, params, CategoryModel.class);
		return filterAndCollectCategories(categoryModels);
	}

		private List<CategoryModel> filterAndCollectCategories(List<CategoryModel> categoryModels) {
		List<CategoryModel> updatedCatModels = new ArrayList<>();

		for (CategoryModel categoryModel : categoryModels) {
			if (shouldBeIncluded(categoryModel)) {
				updatedCatModels.add(categoryModel);
			}
		}

		return updatedCatModels;
	}

		private boolean shouldBeIncluded(CategoryModel categoryModel) {
		if (!categoryModel.isRentalCategory()) {
			return true;
		} else if (CollectionUtils.isNotEmpty(categoryModel.getCategories())) {
			return categoryModel.getCategories().stream().anyMatch(cat -> cat.getProducts().size() > 0);
		} else {
			return categoryModel.getProducts().size() > 0;
		}
	}

}
