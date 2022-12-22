/**
 *
 */
package com.bl.commercewebservices.v2.controller;

import de.hybris.platform.commercefacades.BlItemsBillingChargeData;
import de.hybris.platform.commercefacades.giftcard.data.GiftCardData;
import de.hybris.platform.commercefacades.giftcard.movement.data.GiftCardMovementData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.commercefacades.user.data.CustomerListData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.commercewebservicescommons.dto.BlItemsBillingChargeListWsDTO;
import de.hybris.platform.commercewebservicescommons.dto.UsersListWsDTO;
import de.hybris.platform.commercewebservicescommons.dto.order.OrderEntryListWsDTO;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionEntryData;
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

import com.bl.facades.commercefacades.BlItemsBillingChargeListData;
import com.bl.facades.commercefacades.CustomerListsData;
import com.bl.facades.commercefacades.giftcard.data.GiftCardListData;
import com.bl.facades.commercefacades.giftcard.movement.data.GiftCardMovementListData;
import com.bl.facades.domo.BlDomoFacade;
import com.bl.facades.giftcard.dto.GiftCardListWsDTO;
import com.bl.facades.giftcardmovements.dto.GiftCardMovementListWsDTO;
import com.bl.facades.order.data.OrderEntryListData;
import com.bl.facades.order.data.OrderListData;
import com.bl.facades.orders.dto.OrderListWsDTO;
import com.bl.facades.packageinfo.data.PackagingInfoListData;
import com.bl.facades.packageinfo.dto.PackagingInfoListWsDTO;
import com.bl.facades.paymentTransaction.data.PaymentTransactionEntryListData;
import com.bl.facades.paymentTransaction.data.PaymentTransactionListData;
import com.bl.facades.paymentTransaction.dto.PaymentTransactionEntryListWsDTO;
import com.bl.facades.paymentTransaction.dto.PaymentTransactionListWsDTO;

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

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/paymnetTransactions", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getPackagingInfos", value = "Get PackagingInfo", notes = "Returns PackagingInfo")
	@ApiBaseSiteIdAndUserIdParam
	public PaymentTransactionListWsDTO getpaymnetTransactions(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final PaymentTransactionListData paymentTransactionListData;
		paymentTransactionListData = createTransListData(blDomoFacade.getPaymentTransactions(pageableData));
		setTotalCountHeader(response, paymentTransactionListData.getPagination());
		return getDataMapper().map(paymentTransactionListData, PaymentTransactionListWsDTO.class, fields);
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/paymnetTransactionEntries", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getPackagingInfos", value = "Get PackagingInfo", notes = "Returns PackagingInfo")
	@ApiBaseSiteIdAndUserIdParam
	public PaymentTransactionEntryListWsDTO getpaymnetTransactionEntries(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final PaymentTransactionEntryListData paymentTransactionEntryListData;
		paymentTransactionEntryListData = createTransEntryListData(blDomoFacade.getPaymentTransactionEntries(pageableData));
		setTotalCountHeader(response, paymentTransactionEntryListData.getPagination());
		return getDataMapper().map(paymentTransactionEntryListData, PaymentTransactionEntryListWsDTO.class, fields);
	}

	protected PaymentTransactionListData createTransListData(final SearchPageData<PaymentTransactionData> result)
	{
		final PaymentTransactionListData paymentTransactionListData = new PaymentTransactionListData();

		paymentTransactionListData.setPaymentTransactions(result.getResults());
		paymentTransactionListData.setSorts(result.getSorts());
		paymentTransactionListData.setPagination(result.getPagination());

		return paymentTransactionListData;
	}

	protected PaymentTransactionEntryListData createTransEntryListData(final SearchPageData<PaymentTransactionEntryData> result)
	{
		final PaymentTransactionEntryListData paymentTransactionEntryListData = new PaymentTransactionEntryListData();

		paymentTransactionEntryListData.setPaymentTransactionEntries(result.getResults());
		paymentTransactionEntryListData.setSorts(result.getSorts());
		paymentTransactionEntryListData.setPagination(result.getPagination());

		return paymentTransactionEntryListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/giftCard", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getGiftCards", value = "Get Gift Cards", notes = "Returns Gift Cards")
	@ApiBaseSiteIdAndUserIdParam
	public GiftCardListWsDTO getGiftCards(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final GiftCardListData giftCardListData;
		giftCardListData = createGiftCardListData(blDomoFacade.getGiftCards(pageableData));
		setTotalCountHeader(response, giftCardListData.getPagination());
		return getDataMapper().map(giftCardListData, GiftCardListWsDTO.class, fields);
	}

	protected GiftCardListData createGiftCardListData(final SearchPageData<GiftCardData> result)
	{
		final GiftCardListData giftCardListData = new GiftCardListData();

		giftCardListData.setGiftCards(result.getResults());
		giftCardListData.setSorts(result.getSorts());
		giftCardListData.setPagination(result.getPagination());

		return giftCardListData;
	}


	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/giftCardMovements", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getGiftCardMovements", value = "Get Gift Card Movements", notes = "Returns Gift Card Movements")
	@ApiBaseSiteIdAndUserIdParam
	public GiftCardMovementListWsDTO getGiftCardMovements(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final GiftCardMovementListData giftCardMovementListData;
		giftCardMovementListData = createGiftCardMovementListData(blDomoFacade.getGiftCardMovements(pageableData));
		setTotalCountHeader(response, giftCardMovementListData.getPagination());
		return getDataMapper().map(giftCardMovementListData, GiftCardMovementListWsDTO.class, fields);
	}

	protected GiftCardMovementListData createGiftCardMovementListData(final SearchPageData<GiftCardMovementData> result)
	{
		final GiftCardMovementListData giftCardMovementListData = new GiftCardMovementListData();

		giftCardMovementListData.setGiftCardMovements(result.getResults());
		giftCardMovementListData.setSorts(result.getSorts());
		giftCardMovementListData.setPagination(result.getPagination());

		return giftCardMovementListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/orders", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getOrders", value = "Get orders", notes = "Returns orders")
	@ApiBaseSiteIdAndUserIdParam
	public OrderListWsDTO getOrders(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final OrderListData orderListData;
		orderListData = createOrderListData(blDomoFacade.getOrders(pageableData));
		setTotalCountHeader(response, orderListData.getPagination());
		return getDataMapper().map(orderListData, OrderListWsDTO.class, fields);
	}

	protected OrderListData createOrderListData(final SearchPageData<OrderData> result)
	{
		final OrderListData orderListData = new OrderListData();

		orderListData.setOrders(result.getResults());
		orderListData.setSorts(result.getSorts());
		orderListData.setPagination(result.getPagination());

		return orderListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/orderEntries", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getOrderEntries", value = "Get orders", notes = "Returns orders")
	@ApiBaseSiteIdAndUserIdParam
	public OrderEntryListWsDTO getOrderEntries(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final OrderEntryListData orderEntryListData;
		orderEntryListData = createOrderEntryListData(blDomoFacade.getOrderEntries(pageableData));
		setTotalCountHeader(response, orderEntryListData.getPagination());
		return getDataMapper().map(orderEntryListData, OrderEntryListWsDTO.class, fields);
	}

	protected OrderEntryListData createOrderEntryListData(final SearchPageData<OrderEntryData> result)
	{
		final OrderEntryListData orderEntryListData = new OrderEntryListData();

		orderEntryListData.setOrderEntries(result.getResults());
		orderEntryListData.setSorts(result.getSorts());
		orderEntryListData.setPagination(result.getPagination());

		return orderEntryListData;
	}
	
	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/blItemsBillingCharge", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getBlItemsBillingCharge", value = "Get BL Items Return Charge", notes = "Returns BL Items Return Charge")
	@ApiBaseSiteIdAndUserIdParam
	public BlItemsBillingChargeListWsDTO getBlItemsBillingCharge(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final BlItemsBillingChargeListData blItemsBillingChargeListData;
		blItemsBillingChargeListData = createBlItemsBillingChargeListData(blDomoFacade.getBlItemsBillingCharge(pageableData));
		setTotalCountHeader(response, blItemsBillingChargeListData.getPagination());
		return getDataMapper().map(blItemsBillingChargeListData, BlItemsBillingChargeListWsDTO.class, fields);
	}

	protected BlItemsBillingChargeListData createBlItemsBillingChargeListData(final SearchPageData<BlItemsBillingChargeData> searchPageData)
	{
		final BlItemsBillingChargeListData blItemsBillingChargeListData = new BlItemsBillingChargeListData();

		blItemsBillingChargeListData.setBlItemsBillingChargeData(searchPageData.getResults());
		blItemsBillingChargeListData.setSorts(searchPageData.getSorts());
		blItemsBillingChargeListData.setPagination(searchPageData.getPagination());

		return blItemsBillingChargeListData;
	}
	
	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/customers", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getCustomers", value = "Get Customers", notes = "Returns Customers")
	@ApiBaseSiteIdAndUserIdParam
	public UsersListWsDTO getCustomers(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final CustomerListsData customerListsData;
		customerListsData = createCustomerListsData(blDomoFacade.getCustomers(pageableData));
		setTotalCountHeader(response, customerListsData.getPagination());
		return getDataMapper().map(customerListsData, UsersListWsDTO.class, fields);
	}

	protected CustomerListsData createCustomerListsData(final SearchPageData<CustomerData> result)
	{
		final CustomerListsData customerListsData = new CustomerListsData();

		customerListsData.setCustomers(result.getResults());
		customerListsData.setSorts(result.getSorts());
		customerListsData.setPagination(result.getPagination());

		return customerListsData;
	}

}
