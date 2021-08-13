package com.bl.core.search.solrfacetsearch.services;

import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.solrfacetsearch.config.FacetSearchConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedType;
import de.hybris.platform.solrfacetsearch.config.exceptions.FacetConfigServiceException;
import de.hybris.platform.solrfacetsearch.model.SolrIndexModel;
import de.hybris.platform.solrfacetsearch.model.config.SolrIndexedTypeModel;
import de.hybris.platform.solrfacetsearch.solr.Index;
import de.hybris.platform.solrfacetsearch.solr.SolrSearchProvider;
import de.hybris.platform.solrfacetsearch.solr.exceptions.SolrServiceException;
import de.hybris.platform.solrfacetsearch.suggester.SolrSuggestion;
import de.hybris.platform.solrfacetsearch.suggester.exceptions.SolrAutoSuggestException;
import de.hybris.platform.solrfacetsearch.suggester.impl.DefaultSolrAutoSuggestService;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.SpellCheckResponse;
import org.apache.solr.client.solrj.response.SuggesterResponse;

public class DefaultBlSolrAutoSuggestService extends DefaultSolrAutoSuggestService {

  @Override
  public SolrSuggestion getAutoSuggestionsForQuery(
      LanguageModel language, SolrIndexedTypeModel solrIndexedType, String queryInput) throws SolrAutoSuggestException {
    SolrClient solrClient = null;
    if (StringUtils.isNotBlank(queryInput)) {
      try {
        String dictionary = language.getIsocode();
        String configName = solrIndexedType.getSolrFacetSearchConfig().getName();
        FacetSearchConfig facetSearchConfig = this.facetSearchConfigService.getConfiguration(configName);
        IndexedType indexedType = (IndexedType)facetSearchConfig.getIndexConfig().getIndexedTypes().get(getSolrIndexedTypeCodeResolver().resolveIndexedTypeCode(solrIndexedType));
        SolrSearchProvider solrSearchProvider = getSolrSearchProviderFactory().getSearchProvider(facetSearchConfig, indexedType);
        SolrIndexModel solrIndex =getSolrIndexService().getActiveIndex(facetSearchConfig.getName(), indexedType.getIdentifier());
        Index index = solrSearchProvider.resolveIndex(facetSearchConfig, indexedType, solrIndex.getQualifier());
        solrClient = solrSearchProvider.getClient(index);
        SolrQuery query = new SolrQuery();
        query.setQuery(queryInput);
        query.setRequestHandler("/suggest");
        query.set("suggest.q", new String[]{queryInput});
        query.set("suggest.dictionary", new String[]{dictionary});
        query.set("spellcheck.q", new String[]{queryInput});
        query.set("spellcheck.dictionary", new String[]{dictionary});

        QueryResponse response = solrClient.query(index.getName(), query);
        SuggesterResponse suggesterResponse = response.getSuggesterResponse();
        SolrSuggestion var17;
        if (suggesterResponse != null) {
          var17 = this.createResultFromSuggesterResponse(suggesterResponse, dictionary);
          return var17;
        }

        SpellCheckResponse spellCheckResponse = response.getSpellCheckResponse();
        if (spellCheckResponse != null) {
          var17 = this.createResultFromSpellCheckResponse(spellCheckResponse);
          return var17;
        }
      } catch (FacetConfigServiceException | SolrServerException | IOException | SolrServiceException var20) {
        throw new SolrAutoSuggestException("Error issuing suggestion query", var20);
      } finally {
        IOUtils.closeQuietly(solrClient);
      }
    }

    return new SolrSuggestion(new HashMap(), new ArrayList());
  }
}
