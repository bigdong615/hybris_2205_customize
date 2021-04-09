package com.bl.core.media;

import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import java.util.List;

/**
 *  BlMediaContainerService interface created for getting list of media
 *   @author Manikandan
 */
public interface BlMediaContainerService {

  /**
   *  This is created to get list of media
   * @param mediaContainerModel product specific media container
   * @param mediaFormatModel   format of media model
   * @return  List<MediaModel> associate with media container
   */
  List<MediaModel> getMediaForFormatList(final MediaContainerModel mediaContainerModel, final MediaFormatModel mediaFormatModel);

}
