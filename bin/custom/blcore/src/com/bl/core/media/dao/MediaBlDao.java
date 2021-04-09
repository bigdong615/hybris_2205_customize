package com.bl.core.media.dao;


import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import java.util.List;

/**
 * This interface created for getting list of Media model from DataBase
 */
public interface MediaBlDao
{
  List<MediaModel> findMediaListByFormat(MediaContainerModel container, MediaFormatModel format);
}
