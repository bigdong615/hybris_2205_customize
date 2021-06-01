package com.bl.core.media.impl;

import com.bl.core.media.BlMediaService;
import com.bl.core.media.dao.impl.DefaultBlMediaDao;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.List;

/**
 * This class is created for getting media model
 * @author Manikandan
 *
 */

public class DefaultBlMediaService implements BlMediaService {
  private DefaultBlMediaDao defaultBlMediaDao;

  /**
   * @inheritdoc
   */
  @Override
  public List<MediaModel> getMediaListByFormat(final MediaContainerModel container, final MediaFormatModel format) {
    ServicesUtil.validateParameterNotNull(container, "Argument container must not be null.");
    ServicesUtil.validateParameterNotNull(format, "Argument format must not be null.");
    return this.getDefaultBlMediaDao().findMediaListByFormat(container, format);
  }


  public DefaultBlMediaDao getDefaultBlMediaDao() {
    return defaultBlMediaDao;
  }

  public void setDefaultBlMediaDao(DefaultBlMediaDao defaultBlMediaDao) {
    this.defaultBlMediaDao = defaultBlMediaDao;
  }

}
