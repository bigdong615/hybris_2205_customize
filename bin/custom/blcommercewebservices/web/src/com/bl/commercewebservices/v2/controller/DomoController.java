/**
 *
 */
package com.bl.commercewebservices.v2.controller;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;
import de.hybris.platform.webservicescommons.cache.CacheControl;
import de.hybris.platform.webservicescommons.cache.CacheControlDirective;
import de.hybris.platform.webservicescommons.swagger.ApiBaseSiteIdAndUserIdParam;

import java.util.Map;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.bl.facades.domo.BlDomoFacade;
import com.bl.facades.packageinfo.data.PackagingInfoListData;
import com.bl.facades.packageinfo.dto.PackagingInfoListWsDTO;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;


@Controller
@RequestMapping(value = "/{baseSiteId}")
@Api(tags = "Domo")
public class DomoController extends BaseCommerceController
{
	private static final Logger LOG = LoggerFactory.getLogger(DomoController.class);

	@Resource(name = "blDomoFacade")
	private BlDomoFacade blDomoFacade;


	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/packagingInfos", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getPackagingInfos", value = "Get PackagingInfo", notes = "Returns PackagingInfo")
	@ApiBaseSiteIdAndUserIdParam
	public PackagingInfoListWsDTO getPackagingInfos(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final PackagingInfoListData packagingInfoListData;
		packagingInfoListData = createPackginfInfoListData(blDomoFacade.getPackagingInfos(pageableData));
		setTotalCountHeader(response, packagingInfoListData.getPagination());
		return getDataMapper().map(packagingInfoListData, PackagingInfoListWsDTO.class, fields);
	}

	protected PageableData createPageableData(final int currentPage, final int pageSize)
	{
		final PageableData pageable = new PageableData();

		pageable.setCurrentPage(currentPage);
		pageable.setPageSize(pageSize);
		return pageable;
	}

	protected PackagingInfoListData createPackginfInfoListData(final SearchPageData<PackagingInfoData> result)
	{
		final PackagingInfoListData packagingInfoListData = new PackagingInfoListData();

		packagingInfoListData.setPackageinfos(result.getResults());
		packagingInfoListData.setSorts(result.getSorts());
		packagingInfoListData.setPagination(result.getPagination());

		return packagingInfoListData;
	}
}
