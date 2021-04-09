package com.bl.core.media.dao;


import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import java.util.List;

/**
 * This interface created for getting list of Media model from DataBase
 *  @author Manikandan
 */
public interface BlMediaDao
{

  /**
   * methdod is created to get the list of media model
   * @param container media container for specific product
   * @param format media format
   * @return List<MediaModel> associate media container
   */
  List<MediaModel> findMediaListByFormat(final MediaContainerModel container, final MediaFormatModel format);
}
