/**
 *
 */
package com.bl.commercewebservices.v2.controller;

import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.commercefacades.order.data.ConsignmentData;
import de.hybris.platform.commercefacades.order.data.ConsignmentEntryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.webservicescommons.cache.CacheControl;
import de.hybris.platform.webservicescommons.cache.CacheControlDirective;
import de.hybris.platform.webservicescommons.swagger.ApiBaseSiteIdAndUserIdParam;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.access.annotation.Secured;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.bl.core.product.service.BlProductService;
import com.bl.facades.consignment.BLConsignmentFacade;
import com.bl.facades.consignment.data.ConsignmentEntryListData;
import com.bl.facades.consignment.data.ConsignmentListData;
import com.bl.facades.consignment.dto.ConsignmentEntryListWsDTO;
import com.bl.facades.consignment.dto.ConsignmentListWsDTO;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;


@Controller
@Secured("ROLE_CLIENT")
@RequestMapping(value = "/{baseSiteId}")
@Api(tags = "Consignment")
public class ConsignmentController extends BaseCommerceController
{
	private static final Logger LOG = LoggerFactory.getLogger(ConsignmentController.class);

	@Resource(name = "blconsignmentFacade")
	private BLConsignmentFacade blconsignmentFacade;

	@Resource(name = "productService")
	private BlProductService productService;

	@Resource(name = "sessionService")
	private SessionService sessionService;

	@Resource
	private SearchRestrictionService searchRestrictionService;

	@Resource
	private CatalogVersionService catalogVersionService;

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
		final Map<String, Integer> nameCount = new HashMap<>();
		for (final ConsignmentEntryData cons : ce.getResults())
		{
			for (final String str : cons.getSerialproducts().split(","))
			{
				LOG.info(str);
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

				}
				else if (occursOnlyOnce(cons.getSerialproducts(), str))
				{

					final ProductModel productModel = getProductForCode(str);

					if (productModel != null)
					{
						final String pname = productModel.getName();
						for (final String itm : cons.getItems().split(","))
						{
							if (itm.contains(pname) || itm.contains(str))
							{
								consignmentEntryData.setItems(itm);
							}
						}
						for (final String csts : cons.getConsignmententrystatus().split(","))
						{
							if (csts.contains(pname) || csts.contains(str))
							{
								consignmentEntryData.setConsignmententrystatus(csts);
							}
						}
					}
				}
				else
				{
					final ProductModel productModel = getProductForCode(str);
					if (productModel != null)
					{
						if (nameCount.containsKey(str))
						{
							final int count = nameCount.get(str) + 1;
							nameCount.put(str, count);
							final String pname = productModel.getName() + "--" + count;
							LOG.info(pname);
							for (final String itm : cons.getItems().split(","))
							{
								if (itm.contains(pname) || itm.contains(str))
								{
									consignmentEntryData.setItems(itm);
								}
							}
							for (final String csts : cons.getConsignmententrystatus().split(","))
							{
								if (csts.contains(pname) || csts.contains(str))
								{
									consignmentEntryData.setConsignmententrystatus(csts);
								}
							}
						}
						else
						{
							nameCount.put(str, 1);
							final String pname = productModel.getName() + "--" + 1;
							LOG.info(pname);
							for (final String itm : cons.getItems().split(","))
							{
								if (itm.contains(pname) || itm.contains(str))
								{
									consignmentEntryData.setItems(itm);
								}
							}
							for (final String csts : cons.getConsignmententrystatus().split(","))
							{
								if (csts.contains(pname) || csts.contains(str))
								{
									consignmentEntryData.setConsignmententrystatus(csts);
								}
							}
						}
					}

				}
				consignmentEntryData.setSerialproducts(str);
				cl.add(consignmentEntryData);
			}
		}
		ce.getResults().removeAll(ce.getResults());
		ce.getResults().addAll(cl);
		consignmentEntryListData = createConsignmentEntryListData(ce);
		setTotalCountHeader(response, consignmentEntryListData.getPagination());
		return getDataMapper().map(consignmentEntryListData, ConsignmentEntryListWsDTO.class, fields);
	}

	/**
	 * @param str
	 * @return
	 */
	private ProductModel getProductForCode(final String str)
	{
		final CatalogVersionModel catalogVersionModel = catalogVersionService.getCatalogVersion("blProductCatalog", "Online");
		ProductModel productModel = null;
		try
		{
			productModel = productService.getProductsOfOnlineVersion(str, catalogVersionModel);
		}
		catch (final Exception exp)
		{
			LOG.error(exp.getMessage());
		}
		return productModel;
	}

	protected boolean occursOnlyOnce(final String input, final String value)
	{
		final int firstIndex = input.indexOf(value);
		final int lastIndex = input.lastIndexOf(value);

		return firstIndex != -1 && firstIndex == lastIndex;
	}
}
