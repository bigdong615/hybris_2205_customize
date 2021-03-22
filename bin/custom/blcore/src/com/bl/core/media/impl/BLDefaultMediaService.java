package com.bl.core.media.impl;

import com.bl.core.media.BLMediaService;
import com.bl.core.media.dao.BLMediaDao;
import com.bl.core.media.dao.impl.BLDefaultMediaDao;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.List;

public class BLDefaultMediaService  implements BLMediaService {

  private BLDefaultMediaDao blDefaultMediaDao;
  @Override
  public List<MediaModel> getMediaListByFormat(MediaContainerModel container, MediaFormatModel format) {
    ServicesUtil.validateParameterNotNull(container, "Argument container must not be null.");
    ServicesUtil.validateParameterNotNull(format, "Argument format must not be null.");
    return this.getBlDefaultMediaDao().findMediaListByFormat(container, format);
  }

  public BLDefaultMediaDao getBlDefaultMediaDao() {
    return blDefaultMediaDao;
  }

  public void setBlDefaultMediaDao(BLDefaultMediaDao blDefaultMediaDao) {
    this.blDefaultMediaDao = blDefaultMediaDao;
  }


}
