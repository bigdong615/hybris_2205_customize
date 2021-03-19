package com.bl.core.media.impl;

import com.bl.core.media.BLFlexibleSearchService;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.search.impl.DefaultFlexibleSearchService;
import java.util.List;
import org.apache.poi.ss.formula.functions.T;

public class BLDefaultFlexibleSearchService  extends DefaultFlexibleSearchService implements BLFlexibleSearchService {

  @Override
  public List<MediaModel> searchUniqueList(FlexibleSearchQuery searchQuery) {
    SearchResult<T> searchResult = this.search(String.valueOf(searchQuery));
    List<T> result = searchResult.getResult();
    if (result.isEmpty()) {
      throw new ModelNotFoundException("No result for the given query");
    } else {
      List resultingModel = result;
      if (resultingModel == null) {
        throw new ModelNotFoundException("No result for the given query");
      } else {
        return resultingModel;
      }
    }
  }


}
