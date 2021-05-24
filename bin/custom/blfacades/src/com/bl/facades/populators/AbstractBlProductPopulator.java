/**
 *
 */
package com.bl.facades.populators;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.time.DurationFormatUtils;

import com.bl.core.model.ProductVideoModel;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.ProductVideoData;


/**
 * @author Ravikumar
 *
 */
public class AbstractBlProductPopulator
{
	/*
	 * This method used to populate video related information.
	 */
	protected List<ProductVideoData> populateVideo(final Collection<ProductVideoModel> populateVideos)
	{
		final List<ProductVideoData> videoDataList = new ArrayList<>();
		populateVideos.forEach(productVideoModel -> {
			final ProductVideoData productVideoData = new ProductVideoData();
			productVideoData.setVideoName(productVideoModel.getVideoTitle());
			productVideoData.setVideoUrl(productVideoModel.getVideoLink());
			productVideoData.setVideoDuration(DurationFormatUtils.formatDuration(productVideoModel.getVideoDuration() * 1000,
					BlFacadesConstants.TIME_FORMAT_STRING));
			videoDataList.add(productVideoData);
		});
		return videoDataList;
	}

}
