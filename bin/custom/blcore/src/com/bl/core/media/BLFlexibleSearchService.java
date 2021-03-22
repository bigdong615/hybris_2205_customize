package com.bl.core.media;

import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import java.util.List;

public interface BLFlexibleSearchService {

  List<MediaModel> searchUniqueList(FlexibleSearchQuery searchQuery);
}
