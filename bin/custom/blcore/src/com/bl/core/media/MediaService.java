package com.bl.core.media;

import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import java.util.List;

/**
 * @author Manikandan
 * This interface created for getting list of media model
 */
public interface MediaService {

  List<MediaModel> getMediaListByFormat(final MediaContainerModel container, final MediaFormatModel format);
}
