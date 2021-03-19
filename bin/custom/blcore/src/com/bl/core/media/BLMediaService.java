package com.bl.core.media;

import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import java.util.List;

public interface BLMediaService {

  List<MediaModel> getMediaListByFormat(MediaContainerModel container, MediaFormatModel format);
}
