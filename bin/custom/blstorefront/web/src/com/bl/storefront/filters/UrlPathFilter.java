/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.filters;

import java.io.IOException;
import java.util.Map;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.bl.logging.BlLogger;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.util.UrlPathHelper;
import org.apache.log4j.Logger;

/**
 */
public class UrlPathFilter extends OncePerRequestFilter
{
	private static final Logger LOG = Logger.getLogger(UrlPathFilter.class);
	private UrlPathHelper urlPathHelper;
	private Map<String, Filter> urlPathMapping;
	private Filter defaultFilter;

	public UrlPathHelper getUrlPathHelper()
	{
		return urlPathHelper;
	}

	@Required
	public void setUrlPathHelper(final UrlPathHelper urlPathHelper)
	{
		this.urlPathHelper = urlPathHelper;
	}

	public Map<String, Filter> getUrlPathMapping()
	{
		return urlPathMapping;
	}

	public void setUrlPathMapping(final Map<String, Filter> urlPathMapping)
	{
		this.urlPathMapping = urlPathMapping;
	}

	public Filter getDefaultFilter()
	{
		return defaultFilter;
	}

	@Required
	public void setDefaultFilter(final Filter defaultFilter)
	{
		this.defaultFilter = defaultFilter;
	}

	@Override
	protected void doFilterInternal(final HttpServletRequest request, final HttpServletResponse response, final FilterChain filterChain) throws ServletException, IOException
	{
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Request Url before cononical link for pdf 1: {}",request.getRequestURL());
		if(StringUtils.isNotBlank(request.getRequestURL().toString()) && request.getRequestURL().toString().contains("pdf")){
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Request Url Inside before set cononical link for pdf 2: {}",request.getRequestURL());
			response.setHeader("Link", "<"+request.getRequestURL()+"?context="+request.getParameter("context")+";> rel='canonical'");
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Request Url inside after cononical link for pdf 3: {}",response.getHeader("Link"));
		}
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Request Url After cononical link for pdf 4: {}",request.getRequestURL());

		final Map<String, Filter> mapping = getUrlPathMapping();
		if (mapping != null && !mapping.isEmpty())
		{
			final String servletPath = getUrlPathHelper().getServletPath(request);
			for (final Map.Entry<String, Filter> entry : mapping.entrySet())
			{
				if (servletPath.startsWith(entry.getKey()))
				{
					entry.getValue().doFilter(request, response, filterChain);
					return;
				}
			}
		}

		getDefaultFilter().doFilter(request, response, filterChain);
	}
}
