/**
 *
 */
package com.bl.commercewebservices.v2.controller;

import de.hybris.platform.commercefacades.order.data.ConsignmentData;
import de.hybris.platform.commercefacades.order.data.ConsignmentEntryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.webservicescommons.cache.CacheControl;
import de.hybris.platform.webservicescommons.cache.CacheControlDirective;
import de.hybris.platform.webservicescommons.swagger.ApiBaseSiteIdAndUserIdParam;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.bl.facades.consignment.BLConsignmentFacade;
import com.bl.facades.consignment.data.ConsignmentEntryListData;
import com.bl.facades.consignment.data.ConsignmentListData;
import com.bl.facades.consignment.dto.ConsignmentEntryListWsDTO;
import com.bl.facades.consignment.dto.ConsignmentListWsDTO;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;


@Controller
@RequestMapping(value = "/{baseSiteId}")
@Api(tags = "Consignment")
public class ConsignmentController extends BaseCommerceController
{
	private static final Logger LOG = LoggerFactory.getLogger(ConsignmentController.class);

	@Resource(name = "blconsignmentFacade")
	private BLConsignmentFacade blconsignmentFacade;

	@Resource(name = "productService")
	private ProductService productService;

	@Resource(name = "sessionService")
	private SessionService sessionService;

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/consignmententries", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getConsignmentEntries", value = "Get consignment entries", notes = "Returns order history data for all orders placed by a specified user for a specified base store. The response can display the results across multiple pages, if required.")
	@ApiBaseSiteIdAndUserIdParam
	public ConsignmentEntryListWsDTO getConsignmentEntries(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	final String date, @ApiParam(value = "Input date like today-1")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		sessionService.setAttribute("isApiCall", true);
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final ConsignmentEntryListData consignmentEntryListData;
		consignmentEntryListData = createConsignmentEntryListData(
				blconsignmentFacade.getConsignmentEntries(pageableData, convertDate(date)));
		setTotalCountHeader(response, consignmentEntryListData.getPagination());
		return getDataMapper().map(consignmentEntryListData, ConsignmentEntryListWsDTO.class, fields);
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/consignments", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getConsignments", value = "Get consignments", notes = "Returns order history data for all orders placed by a specified user for a specified base store. The response can display the results across multiple pages, if required.")
	@ApiBaseSiteIdAndUserIdParam
	public ConsignmentListWsDTO getConsignments(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	final String date, @ApiParam(value = "Input date like today-1")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		sessionService.setAttribute("isApiCall", true);
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final ConsignmentListData consignmentListData;
		consignmentListData = createConsignmentListData(blconsignmentFacade.getConsignments(pageableData, convertDate(date)));
		setTotalCountHeader(response, consignmentListData.getPagination());
		return getDataMapper().map(consignmentListData, ConsignmentListWsDTO.class, fields);
	}


	protected PageableData createPageableData(final int currentPage, final int pageSize)
	{
		final PageableData pageable = new PageableData();

		pageable.setCurrentPage(currentPage);
		pageable.setPageSize(pageSize);
		return pageable;
	}

	protected ConsignmentEntryListData createConsignmentEntryListData(final SearchPageData<ConsignmentEntryData> result)
	{
		final ConsignmentEntryListData consignmentEntryListData = new ConsignmentEntryListData();

		consignmentEntryListData.setConsignmentEntries(result.getResults());
		consignmentEntryListData.setSorts(result.getSorts());
		consignmentEntryListData.setPagination(result.getPagination());

		return consignmentEntryListData;
	}

	protected ConsignmentListData createConsignmentListData(final SearchPageData<ConsignmentData> result)
	{
		final ConsignmentListData consignmentListData = new ConsignmentListData();

		consignmentListData.setConsignments(result.getResults());
		consignmentListData.setSorts(result.getSorts());
		consignmentListData.setPagination(result.getPagination());

		return consignmentListData;
	}


	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/consignmententriesserials", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getConsignmentEntriesSerials", value = "Get consignment entries", notes = "Returns order history data for all orders placed by a specified user for a specified base store. The response can display the results across multiple pages, if required.")
	@ApiBaseSiteIdAndUserIdParam
	public ConsignmentEntryListWsDTO getConsignmentEntriesSerials(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	final String date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		sessionService.setAttribute("isApiCall", true);
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final ConsignmentEntryListData consignmentEntryListData;
		final SearchPageData<ConsignmentEntryData> ce = blconsignmentFacade.getConsignmentEntries(pageableData, convertDate(date));
		final List<ConsignmentEntryData> cl = new ArrayList<ConsignmentEntryData>();
		for (final ConsignmentEntryData cons : ce.getResults())
		{
			for (final String str : cons.getSerialproducts().split(","))
			{
				LOG.debug(str);
				final ConsignmentEntryData consignmentEntryData = getDataMapper().map(cons, ConsignmentEntryData.class);

				if (StringUtils.isNumeric(str))
				{
					for (final String itm : cons.getItems().split(","))
					{
						if (itm.contains(str))
						{
							consignmentEntryData.setItems(itm);
						}
					}
					for (final String csts : cons.getConsignmententrystatus().split(","))
					{
						if (csts.contains(str))
						{
							consignmentEntryData.setConsignmententrystatus(csts);
						}
					}
					consignmentEntryData.setSerialproducts(str);
					cl.add(consignmentEntryData);
				}
				else
				{
					final ProductModel productModel = productService.getProductForCode(str);
					final List<String> tempList = new ArrayList<>();
					int count = 1;
					if (!tempList.contains(str))
					{
						final String pname = productModel.getName() + "--" + count;
						for (final String itm : cons.getItems().split(","))
						{
							if (itm.contains(pname))
							{
								consignmentEntryData.setItems(itm);
							}
						}
						for (final String csts : cons.getConsignmententrystatus().split(","))
						{
							if (csts.contains(pname))
							{
								consignmentEntryData.setConsignmententrystatus(csts);
							}
						}
						tempList.add(str);
					}
					else
					{
						count++;
						tempList.add(str);
						final String pname = productModel.getName() + "--" + count;
						for (final String itm : cons.getItems().split(","))
						{
							if (itm.contains(pname))
							{
								consignmentEntryData.setSerialproducts(itm);
							}
						}
						for (final String csts : cons.getConsignmententrystatus().split(","))
						{
							if (csts.contains(pname))
							{
								consignmentEntryData.setConsignmententrystatus(csts);
							}
						}
					}
					consignmentEntryData.setSerialproducts(str);
					cl.add(consignmentEntryData);
				}
			}
		}
		ce.getResults().removeAll(ce.getResults());
		ce.getResults().addAll(cl);
		consignmentEntryListData = createConsignmentEntryListData(ce);
		setTotalCountHeader(response, consignmentEntryListData.getPagination());
		return getDataMapper().map(consignmentEntryListData, ConsignmentEntryListWsDTO.class, fields);
	}
}
