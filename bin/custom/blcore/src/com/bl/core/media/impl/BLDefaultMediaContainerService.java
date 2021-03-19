package com.bl.core.media.impl;

import com.bl.core.media.BLMediaContainerService;
import com.bl.core.media.BLMediaService;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.List;

public class BLDefaultMediaContainerService implements BLMediaContainerService {

  private BLDefaultMediaService blDefaultMediaService;

  @Override
  public List<MediaModel> getMediaForFormatList(
      MediaContainerModel mediaContainerModel, MediaFormatModel mediaFormatModel) {
    ServicesUtil
        .validateParameterNotNull(mediaContainerModel, "Media container model cannot be null");
    ServicesUtil.validateParameterNotNull(mediaFormatModel, "Media format model cannot be null");
    return this.getBlDefaultMediaService().getMediaListByFormat(mediaContainerModel, mediaFormatModel);
  }

  public BLDefaultMediaService getBlDefaultMediaService() {
    return blDefaultMediaService;
  }

  public void setBlDefaultMediaService(BLDefaultMediaService blDefaultMediaService) {
    this.blDefaultMediaService = blDefaultMediaService;
  }


}
