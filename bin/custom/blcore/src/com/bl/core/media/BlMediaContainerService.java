package com.bl.core.media;

import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import java.util.List;

/**
 * @author Manikandan
 * Interface creted for media container service
 */
public interface BlMediaContainerService {

  List<MediaModel> getMediaForFormatList(final MediaContainerModel mediaContainerModel, final MediaFormatModel mediaFormatModel);

}
