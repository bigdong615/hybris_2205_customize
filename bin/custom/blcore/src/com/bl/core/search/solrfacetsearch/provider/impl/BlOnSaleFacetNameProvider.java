package com.bl.core.search.solrfacetsearch.provider.impl;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.provider.FacetDisplayNameProvider;
import de.hybris.platform.solrfacetsearch.provider.FacetValueDisplayNameProvider;
import de.hybris.platform.solrfacetsearch.search.SearchQuery;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Manikandan
 * This class created for setting localized name on storefront
 */
public class BlOnSaleFacetNameProvider  implements FacetDisplayNameProvider, //NOSONAR
    FacetValueDisplayNameProvider {

  /**
   * This method created to display localized name for On Sale Facet
   */
  @Override
  public String getDisplayName(final SearchQuery searchQuery, final String facetValue) {
     return StringUtils.equals(BlCoreConstants.TRUE, facetValue) ? BlCoreConstants.ITEMS_ON_SALE : facetValue;
  }

  /**
   * This overloaded method created to display localized name for On Sale Facet
   */
  @Override
  public String getDisplayName(final SearchQuery searchQuery, final IndexedProperty indexedProperty, final String facetValue) {
    return StringUtils.equals(BlCoreConstants.TRUE, facetValue) ? BlCoreConstants.ITEMS_ON_SALE : facetValue;
  }
}
