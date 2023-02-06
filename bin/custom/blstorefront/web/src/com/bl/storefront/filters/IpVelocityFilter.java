/**
 *
 */
package com.bl.storefront.filters;

import de.hybris.platform.store.services.BaseStoreService;

import java.io.IOException;
import java.util.List;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.web.filter.OncePerRequestFilter;


/**
 * @author Admin
 *
 */
public class IpVelocityFilter extends OncePerRequestFilter
{

	private static final Logger LOG = Logger.getLogger(IpVelocityFilter.class.getName());

	private BaseStoreService baseStoreService;

	@Override
	protected void doFilterInternal(final HttpServletRequest request, final HttpServletResponse response,
			final FilterChain filterChain) throws ServletException, IOException
	{


		final String userIp = getUserIp(request);
		if (null != getBaseStoreService().getCurrentBaseStore())
		{
			final List<String> ipList = getBaseStoreService().getCurrentBaseStore().getBlockedIpList();
			if (CollectionUtils.isNotEmpty(ipList) && ipList.contains(userIp))
			{

				response.sendError(HttpServletResponse.SC_FORBIDDEN, "That means goodbye forever!");

			}
		}
		LOG.info("Current User Ip is " + userIp);
		filterChain.doFilter(request, response);

	}


	/**
	 * @param request
	 * @return
	 */
	private String getUserIp(final HttpServletRequest request)
	{

		final String trueClient = "True-Client-IP";
		final String cfConnectingIP = "CF-Connecting-IP";
		final String forwardFor = "X-Forwarded-For";
		String shopperIP = request.getHeader(trueClient);
		String headerUsed = trueClient;
		if (shopperIP == null)
		{
			headerUsed = cfConnectingIP;
			shopperIP = request.getHeader(cfConnectingIP);
		}
		if (shopperIP == null)
		{
			headerUsed = forwardFor;
			final String xfowardedHeader = request.getHeader(forwardFor);
			// to be extra sure I don't want this to throw an exception for any reason, so I'll add a try catch
			try
			{
				if (StringUtils.isNotBlank(xfowardedHeader))
				{
					final String[] headerParts = xfowardedHeader.split(",");
					if (headerParts.length > 0)
					{
						shopperIP = request.getHeader(headerParts[0]);
					}
				}
			}
			catch (final Exception ex)
			{
				LOG.info("Could not assign shopper IP from X-Forward-For header ", ex);
			}
		}
		if (shopperIP == null)
		{
			headerUsed = "Remote Address";
			shopperIP = request.getRemoteAddr();
		}
		if (shopperIP == null)
		{
			shopperIP = "";
		}
		return shopperIP;

	}


	/**
	 * @return the baseStoreService
	 */
	protected BaseStoreService getBaseStoreService()
	{
		return baseStoreService;
	}

	/**
	 * @param baseStoreService
	 *           the baseStoreService to set
	 */
	public void setBaseStoreService(final BaseStoreService baseStoreService)
	{
		this.baseStoreService = baseStoreService;
	}

}
