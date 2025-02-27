package com.bl.core.media.impl;

import com.bl.core.media.BlMediaContainerService;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.List;

/**
 * This class is Created to get list of media model from database
 * @author Manikandan
 *
 */
public class DefaultBlMediaContainerService implements BlMediaContainerService {

  private DefaultBlMediaService defaultBlMediaService;

  /**
   * {@inheritdoc}
   */
  @Override
  public List<MediaModel> getMediaForFormatList(final
      MediaContainerModel mediaContainerModel, final MediaFormatModel mediaFormatModel) {
    ServicesUtil
        .validateParameterNotNull(mediaContainerModel, "Media container model cannot be null");
    ServicesUtil.validateParameterNotNull(mediaFormatModel, "Media format model cannot be null");
    return this.getDefaultBlMediaService().getMediaListByFormat(mediaContainerModel, mediaFormatModel);
  }


  public DefaultBlMediaService getDefaultBlMediaService() {
    return defaultBlMediaService;
  }

  public void setDefaultBlMediaService(DefaultBlMediaService defaultBlMediaService) {
    this.defaultBlMediaService = defaultBlMediaService;
  }

}
