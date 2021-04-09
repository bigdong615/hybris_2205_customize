package com.bl.core.media;

import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import java.util.List;

 /**
 * This interface created for getting list of media model
 *  @author Manikandan
 */
public interface BlMediaService {

  /**
   * this is created to get list of media
   * @param container media container for specific product
   * @param format media fomat
   * @return  List<MediaModel> associate with media container
   */

  List<MediaModel> getMediaListByFormat(final MediaContainerModel container, final MediaFormatModel format);
}
