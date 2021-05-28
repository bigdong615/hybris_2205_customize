/**
 *
 */
package com.bl.integration.services;

import java.net.URISyntaxException;

import com.bl.facades.fexEx.data.SameDayCityReqData;
import com.bl.facades.fexEx.data.SameDayCityResData;


/**
 * @author Dell
 *
 */
public interface BlFedExSameDayService
{

	public SameDayCityResData getAvailability(final SameDayCityReqData sameDayCityReqData) throws URISyntaxException;

}
