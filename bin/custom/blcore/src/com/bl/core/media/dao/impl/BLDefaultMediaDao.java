package com.bl.core.media.dao.impl;

import com.bl.core.media.dao.BLMediaDao;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class BLDefaultMediaDao implements BLMediaDao {

  private FlexibleSearchService flexibleSearchService;

  @Override
  public List<MediaModel> findMediaListByFormat(MediaContainerModel container, MediaFormatModel format) {
    try {
      Map<String, Object> params = new TreeMap<>();
      params.put("container", container);
      params.put("format", format);
      FlexibleSearchQuery query = new FlexibleSearchQuery("SELECT {pk} FROM {Media} WHERE {mediaContainer} = ?container AND {mediaFormat} = ?format", params);
      final SearchResult result = getFlexibleSearchService().search(query);
      return result.getResult();
    } catch (AmbiguousIdentifierException var5) {
      throw new ModelNotFoundException("Data inconsistency: Multiple medias with format '" + format + "' reside in container '" + container + "'.", var5);
    }
  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(
      FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }


}
