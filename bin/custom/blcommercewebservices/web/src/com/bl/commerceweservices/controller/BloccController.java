package com.bl.commerceweservices.controller;

import de.hybris.platform.webservicescommons.cache.CacheControl;
import de.hybris.platform.webservicescommons.cache.CacheControlDirective;
import de.hybris.platform.webservicescommons.swagger.ApiBaseSiteIdParam;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;


@Controller
@RequestMapping(value = "/{baseSiteId}/bltest")
@CacheControl(directive = CacheControlDirective.PRIVATE)
@Api(tags = "bltest")
public class BloccController
{

	@RequestMapping(method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getTestResponse", value = "Test response", notes = "Test response")
	@ApiBaseSiteIdParam
	public String getTestResponse()
	{
		return "test response";
	}

}
