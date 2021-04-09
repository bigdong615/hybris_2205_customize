package com.bl.facades.populators;

import de.hybris.platform.commercefacades.search.data.SearchQueryData;
import de.hybris.platform.commercefacades.search.solrfacetsearch.converters.populator.SolrSearchQueryDecoderPopulator;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;

public class BlSolrSearchQueryDecoderPopulator  extends SolrSearchQueryDecoderPopulator {

  /**
   * BL -80 Since it's a OOB Populate Method override for adding one property
   */
  @Override
  public void populate(final SearchQueryData source, final SolrSearchQueryData target)
  {
    super.populate(source,target);
    target.setBlPage(source.getBlPage());
  }

}
