package com.bl.core.media.dao;


import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import java.util.List;

/**
 * @author Manikandan
 * This interface created for getting list of Media model from DataBase
 */
public interface BlMediaDao
{
  List<MediaModel> findMediaListByFormat(final MediaContainerModel container, final MediaFormatModel format);
}
