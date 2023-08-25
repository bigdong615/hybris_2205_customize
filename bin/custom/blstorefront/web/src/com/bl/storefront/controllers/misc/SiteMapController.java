/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.misc;

import de.hybris.platform.acceleratorservices.urlresolver.SiteBaseUrlResolutionService;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.AbstractController;
import de.hybris.platform.cms2.model.site.CMSSiteModel;
import de.hybris.platform.cms2.servicelayer.services.CMSSiteService;
import de.hybris.platform.core.model.media.MediaModel;
import com.bl.storefront.controllers.ControllerConstants;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import de.hybris.platform.servicelayer.media.MediaService;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@Controller
public class SiteMapController extends AbstractController
{
	@Resource(name = "cmsSiteService")
	private CMSSiteService cmsSiteService;

	@Resource(name = "siteBaseUrlResolutionService")
	private SiteBaseUrlResolutionService siteBaseUrlResolutionService;
	@Resource(name = "mediaService")
	private MediaService mediaService;

	@RequestMapping(value = "assets/sitemap.xml", method = RequestMethod.GET, produces = "application/xml")
	public void getSitemapXml(final Model model, final HttpServletResponse response)
	{
		final CMSSiteModel currentSite = cmsSiteService.getCurrentSite();

		final String mediaUrlForSite = siteBaseUrlResolutionService.getMediaUrlForSite(currentSite, false, "");

		final List<String> siteMapUrls = new ArrayList<>();

		final Collection<MediaModel> siteMaps = currentSite.getSiteMaps();
		MediaModel mediaModel = null;
		for (final MediaModel siteMap : siteMaps)
		{
			siteMapUrls.add(mediaUrlForSite + siteMap.getURL());
			mediaModel = siteMap;
		}
		model.addAttribute("siteMapUrls", siteMapUrls);

		try (PrintWriter out = response.getWriter();
			 InputStream inputStream = mediaService.getStreamFromMedia(mediaModel);
			 BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {

			String line;
			while ((line = br.readLine()) != null) {
				out.println(line);
				out.flush();
			}

		} catch (IOException e) {
			e.printStackTrace();
		}
		//return ControllerConstants.Views.Pages.Misc.MiscSiteMapPage;
	}
}
