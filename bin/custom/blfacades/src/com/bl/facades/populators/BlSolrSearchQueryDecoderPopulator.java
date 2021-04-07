package com.bl.facades.populators;

import de.hybris.platform.commercefacades.search.data.SearchQueryData;
import de.hybris.platform.commercefacades.search.solrfacetsearch.converters.populator.SolrSearchQueryDecoderPopulator;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryTermData;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang.StringUtils;

public class BlSolrSearchQueryDecoderPopulator  extends SolrSearchQueryDecoderPopulator {

  /**
   * BL -80 Since it's a OOB Populate Method override for adding one property
   */

  private static final int TWO = 2;
  @Override
  public void populate(final SearchQueryData source, final SolrSearchQueryData target)
  {
    if (source == null)
    {
      return;
    }

    if (StringUtils.isNotEmpty(source.getValue()))
    {
      final String[] split = source.getValue().split(":");

      if (split.length > 0)
      {
        target.setFreeTextSearch(split[0]);
      }
      if (split.length > 1)
      {
        target.setSort(split[1]);
      }

      final List<SolrSearchQueryTermData> terms = new ArrayList<>();

      for (int i = TWO; (i + 1) < split.length; i += TWO)
      {
        final SolrSearchQueryTermData termData = new SolrSearchQueryTermData();
        termData.setKey(split[i]);
        try
        {
          termData.setValue(URLDecoder.decode(split[i + 1], "UTF-8"));
        }
        catch (final UnsupportedEncodingException e)
        {
          // UTF-8 is supported encoding, so it shouldn't come here
        }
        terms.add(termData);
      }

      target.setFilterTerms(terms);
    }

    target.setFilterQueries(createSolrSearchFilterQueries(source));

    target.setSearchQueryContext(source.getSearchQueryContext());
    target.setBlPage(source.getBlPage());
  }

}
