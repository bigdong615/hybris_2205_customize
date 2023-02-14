/**
 *
 */
package com.bl.commercewebservices.v2.controller;

import de.hybris.platform.commercefacades.BlItemsBillingChargeData;
import de.hybris.platform.commercefacades.giftcard.data.GiftCardData;
import de.hybris.platform.commercefacades.giftcard.movement.data.GiftCardMovementData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.AddressListData;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.commercewebservicescommons.dto.BlItemsBillingChargeListWsDTO;
import de.hybris.platform.commercewebservicescommons.dto.order.OrderEntryListWsDTO;
import de.hybris.platform.commercewebservicescommons.dto.payment.BrainTreePaymentInfoListWsDTO;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionEntryData;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;
import de.hybris.platform.warehousingfacades.product.data.StockLevelData;
import de.hybris.platform.warehousingfacades.product.data.StockLevelListData;
import de.hybris.platform.warehousingwebservices.dto.product.StockLevelListWsDTO;
import de.hybris.platform.webservicescommons.cache.CacheControl;
import de.hybris.platform.webservicescommons.cache.CacheControlDirective;
import de.hybris.platform.webservicescommons.swagger.ApiBaseSiteIdAndUserIdParam;

import java.util.Date;
import java.util.Map;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.bl.facades.blSerialLog.data.BlSerialLogData;
import com.bl.facades.blSerialLog.data.BlSerialLogListData;
import com.bl.facades.blSerialLog.dto.BlSerialLogListWsDTO;
import com.bl.facades.commercefacades.BlItemsBillingChargeListData;
import com.bl.facades.commercefacades.CustomerListsData;
import com.bl.facades.commercefacades.giftcard.data.GiftCardListData;
import com.bl.facades.commercefacades.giftcard.movement.data.GiftCardMovementListData;
import com.bl.facades.customerNotes.data.CustomerNotesData;
import com.bl.facades.customerNotes.data.CustomerNotesListData;
import com.bl.facades.customerNotes.data.NotesData;
import com.bl.facades.customerNotes.data.NotesListData;
import com.bl.facades.customerNotes.dto.CustomerNotesListWsDTO;
import com.bl.facades.customerNotes.dto.NotesListWsDTO;
import com.bl.facades.domo.BlDomoFacade;
import com.bl.facades.giftcard.dto.GiftCardListWsDTO;
import com.bl.facades.giftcardmovements.dto.GiftCardMovementListWsDTO;
import com.bl.facades.inHouseRepairLog.data.InHouseRepairLogData;
import com.bl.facades.inHouseRepairLog.data.InHouseRepairLogListData;
import com.bl.facades.inHouseRepairLog.dto.InHouseRepairLogListWsDTO;
import com.bl.facades.order.data.OrderEntryListData;
import com.bl.facades.order.data.OrderListData;
import com.bl.facades.orders.dto.OrderListWsDTO;
import com.bl.facades.packageinfo.data.PackagingInfoListData;
import com.bl.facades.packageinfo.dto.PackagingInfoListWsDTO;
import com.bl.facades.partsNeededRepairLog.data.PartsNeededRepairLogData;
import com.bl.facades.partsNeededRepairLog.data.PartsNeededRepairLogListData;
import com.bl.facades.partsNeededRepairLog.dto.PartsNeededRepairLogListWsDTO;
import com.bl.facades.paymentTransaction.data.PaymentTransactionEntryListData;
import com.bl.facades.paymentTransaction.data.PaymentTransactionListData;
import com.bl.facades.paymentTransaction.dto.PaymentTransactionEntryListWsDTO;
import com.bl.facades.paymentTransaction.dto.PaymentTransactionListWsDTO;
import com.bl.facades.vendorRepairLog.data.VendorRepairLogData;
import com.bl.facades.vendorRepairLog.data.VendorRepairLogListData;
import com.bl.facades.vendorRepairLog.dto.VendorRepairLogListWsDTO;
import com.bl.integration.dto.AddressListWsDTO;
import com.bl.integration.dto.UsersListWsDTO;
import com.braintree.hybris.data.BrainTreePaymentInfoData;
import com.braintree.hybris.data.BrainTreePaymentInfoListData;

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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final PackagingInfoListData packagingInfoListData;
		packagingInfoListData = createPackginfInfoListData(blDomoFacade.getPackagingInfos(pageableData, date));
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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final PaymentTransactionListData paymentTransactionListData;
		paymentTransactionListData = createTransListData(blDomoFacade.getPaymentTransactions(pageableData, date));
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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final PaymentTransactionEntryListData paymentTransactionEntryListData;
		paymentTransactionEntryListData = createTransEntryListData(blDomoFacade.getPaymentTransactionEntries(pageableData, date));
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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final GiftCardListData giftCardListData;
		giftCardListData = createGiftCardListData(blDomoFacade.getGiftCards(pageableData, date));
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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final GiftCardMovementListData giftCardMovementListData;
		giftCardMovementListData = createGiftCardMovementListData(blDomoFacade.getGiftCardMovements(pageableData, date));
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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final OrderListData orderListData;
		orderListData = createOrderListData(blDomoFacade.getOrders(pageableData, date));
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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final OrderEntryListData orderEntryListData;
		orderEntryListData = createOrderEntryListData(blDomoFacade.getOrderEntries(pageableData, date));
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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final BlItemsBillingChargeListData blItemsBillingChargeListData;
		blItemsBillingChargeListData = createBlItemsBillingChargeListData(blDomoFacade.getBlItemsBillingCharge(pageableData, date));
		setTotalCountHeader(response, blItemsBillingChargeListData.getPagination());
		return getDataMapper().map(blItemsBillingChargeListData, BlItemsBillingChargeListWsDTO.class, fields);
	}

	protected BlItemsBillingChargeListData createBlItemsBillingChargeListData(
			final SearchPageData<BlItemsBillingChargeData> searchPageData)
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
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final CustomerListsData customerListsData;
		customerListsData = createCustomerListsData(blDomoFacade.getCustomers(pageableData, date));
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

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/serialLogs", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getBlSerialLogs", value = "Get blSerialLogs", notes = "Returns blSerialLogs")
	@ApiBaseSiteIdAndUserIdParam
	public BlSerialLogListWsDTO getBlSerialLogs(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final BlSerialLogListData blSerialLogListData;
		blSerialLogListData = createblSerialLogListData(blDomoFacade.getBlSerialLogs(pageableData, date));
		setTotalCountHeader(response, blSerialLogListData.getPagination());
		return getDataMapper().map(blSerialLogListData, BlSerialLogListWsDTO.class, fields);
	}

	protected BlSerialLogListData createblSerialLogListData(final SearchPageData<BlSerialLogData> result)
	{
		final BlSerialLogListData blSerialLogListData = new BlSerialLogListData();

		blSerialLogListData.setBlSerialLogList(result.getResults());
		blSerialLogListData.setSorts(result.getSorts());
		blSerialLogListData.setPagination(result.getPagination());

		return blSerialLogListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/customerNotes", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getCustomerNotes", value = "Get customerNotes", notes = "Returns customerNotes")
	@ApiBaseSiteIdAndUserIdParam
	public CustomerNotesListWsDTO getCustomerNotes(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final CustomerNotesListData customerNotesListData;
		customerNotesListData = createCustomerNotesListData(blDomoFacade.getCustomerNotes(pageableData, date));
		setTotalCountHeader(response, customerNotesListData.getPagination());
		return getDataMapper().map(customerNotesListData, CustomerNotesListWsDTO.class, fields);
	}

	protected CustomerNotesListData createCustomerNotesListData(final SearchPageData<CustomerNotesData> result)
	{
		final CustomerNotesListData customerNotesListData = new CustomerNotesListData();

		customerNotesListData.setCustomerNotesList(result.getResults());
		customerNotesListData.setSorts(result.getSorts());
		customerNotesListData.setPagination(result.getPagination());

		return customerNotesListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/vendorRepairs", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getVendorRepairs", value = "Get vendorRepairs", notes = "Returns vendorRepairs")
	@ApiBaseSiteIdAndUserIdParam
	public VendorRepairLogListWsDTO getVendorRepairs(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final VendorRepairLogListData vendorRepairLogListData;
		vendorRepairLogListData = createVendorRepairsData(blDomoFacade.getVendorRepairLogs(pageableData, date));
		setTotalCountHeader(response, vendorRepairLogListData.getPagination());
		return getDataMapper().map(vendorRepairLogListData, VendorRepairLogListWsDTO.class, fields);
	}

	protected VendorRepairLogListData createVendorRepairsData(final SearchPageData<VendorRepairLogData> result)
	{
		final VendorRepairLogListData vendorRepairLogListData = new VendorRepairLogListData();

		vendorRepairLogListData.setVendorRepairLogList(result.getResults());
		vendorRepairLogListData.setSorts(result.getSorts());
		vendorRepairLogListData.setPagination(result.getPagination());

		return vendorRepairLogListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/partsNeededRepairLogs", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getPartsNeededRepairLogs", value = "Get partsNeededRepairLog", notes = "Returns partsNeededRepairLog")
	@ApiBaseSiteIdAndUserIdParam
	public PartsNeededRepairLogListWsDTO getPartsNeededRepairLogs(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final PartsNeededRepairLogListData partsNeededRepairLogListData;
		partsNeededRepairLogListData = createPartsNeededRepairLogListData(
				blDomoFacade.getPartsNeededRepairLogs(pageableData, date));
		setTotalCountHeader(response, partsNeededRepairLogListData.getPagination());
		return getDataMapper().map(partsNeededRepairLogListData, PartsNeededRepairLogListWsDTO.class, fields);
	}

	protected PartsNeededRepairLogListData createPartsNeededRepairLogListData(
			final SearchPageData<PartsNeededRepairLogData> result)
	{
		final PartsNeededRepairLogListData partsNeededRepairLogListData = new PartsNeededRepairLogListData();

		partsNeededRepairLogListData.setPartsNeededRepairLogList(result.getResults());
		partsNeededRepairLogListData.setSorts(result.getSorts());
		partsNeededRepairLogListData.setPagination(result.getPagination());

		return partsNeededRepairLogListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/inHouseRepairLogs", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getInHouseRepairLogs", value = "Get inHouseRepairLogs", notes = "Returns inHouseRepairLogs")
	@ApiBaseSiteIdAndUserIdParam
	public InHouseRepairLogListWsDTO getInHouseRepairLogs(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final InHouseRepairLogListData inHouseRepairLogListData;
		inHouseRepairLogListData = createInHouseRepairLogListData(blDomoFacade.getInHouseRepairLogs(pageableData, date));
		setTotalCountHeader(response, inHouseRepairLogListData.getPagination());
		return getDataMapper().map(inHouseRepairLogListData, InHouseRepairLogListWsDTO.class, fields);
	}

	protected InHouseRepairLogListData createInHouseRepairLogListData(final SearchPageData<InHouseRepairLogData> result)
	{
		final InHouseRepairLogListData inHouseRepairLogListData = new InHouseRepairLogListData();

		inHouseRepairLogListData.setInHouseRepairLogList(result.getResults());
		inHouseRepairLogListData.setSorts(result.getSorts());
		inHouseRepairLogListData.setPagination(result.getPagination());

		return inHouseRepairLogListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/address", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getAddress", value = "Get Address", notes = "Returns Addresses")
	@ApiBaseSiteIdAndUserIdParam
	public AddressListWsDTO getAddress(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final AddressListData addressListData;
		addressListData = createAddressesListData(blDomoFacade.getAddresses(pageableData, date));
		setTotalCountHeader(response, addressListData.getPagination());
		return getDataMapper().map(addressListData, AddressListWsDTO.class, fields);
	}

	protected AddressListData createAddressesListData(final SearchPageData<AddressData> result)
	{
		final AddressListData addressListData = new AddressListData();
		addressListData.setAddresses(result.getResults());
		addressListData.setSorts(result.getSorts());
		addressListData.setPagination(result.getPagination());
		return addressListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/brainTreePaymentInfo", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getBrainTreePaymentInfoData", value = "Get BrainTree Payment Info", notes = "Returns BrainTree Payment Info Data")
	@ApiBaseSiteIdAndUserIdParam
	public BrainTreePaymentInfoListWsDTO getBrainTreePaymentInfoData(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final BrainTreePaymentInfoListData brainTreePaymentInfoListData;
		brainTreePaymentInfoListData = createBrainTreePaymentInfoListData(blDomoFacade.getBrainTreePaymentInfo(pageableData, date));
		setTotalCountHeader(response, brainTreePaymentInfoListData.getPagination());
		return getDataMapper().map(brainTreePaymentInfoListData, BrainTreePaymentInfoListWsDTO.class, fields);
	}

	protected BrainTreePaymentInfoListData createBrainTreePaymentInfoListData(
			final SearchPageData<BrainTreePaymentInfoData> result)
	{
		final BrainTreePaymentInfoListData brainTreePaymentInfoListData = new BrainTreePaymentInfoListData();
		brainTreePaymentInfoListData.setBrainTreePaymentInfo(result.getResults());
		brainTreePaymentInfoListData.setSorts(result.getSorts());
		brainTreePaymentInfoListData.setPagination(result.getPagination());
		return brainTreePaymentInfoListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/stockLevels", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getStockLevels", value = "Get StockLevels", notes = "Returns StockLevels")
	@ApiBaseSiteIdAndUserIdParam
	public StockLevelListWsDTO getStockLevels(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final StockLevelListData stockLevelListData;
		stockLevelListData = createstockLevelListData(blDomoFacade.getStockLevels(pageableData, date));
		setTotalCountHeader(response, stockLevelListData.getPagination());
		return getDataMapper().map(stockLevelListData, StockLevelListWsDTO.class, fields);
	}

	protected StockLevelListData createstockLevelListData(final SearchPageData<StockLevelData> result)
	{
		final StockLevelListData stockLevelListData = new StockLevelListData();
		stockLevelListData.setStockLevels(result.getResults());
		stockLevelListData.setSorts(result.getSorts());
		stockLevelListData.setPagination(result.getPagination());
		return stockLevelListData;
	}

	@CacheControl(directive = CacheControlDirective.PUBLIC, maxAge = 120)
	@RequestMapping(value = "/notes", method = RequestMethod.GET)
	@ResponseBody
	@ApiOperation(nickname = "getNotes", value = "Get Notes", notes = "Returns Notes")
	@ApiBaseSiteIdAndUserIdParam
	public NotesListWsDTO getNotes(@ApiParam(value = "The current result page requested.")
	@RequestParam(defaultValue = DEFAULT_CURRENT_PAGE)
	final int currentPage, @ApiParam(value = "The number of results returned per page.")
	@RequestParam(value = "date", defaultValue = DEFAULT_DATE)
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	final Date date, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_PAGE_SIZE)
	final int pageSize, @ApiParam(value = "Sorting method applied to the return results.")
	@RequestParam(defaultValue = DEFAULT_FIELD_SET)
	final String fields, @RequestParam
	final Map<String, String> params, final HttpServletResponse response)
	{
		final PageableData pageableData = createPageableData(currentPage, pageSize);
		final NotesListData notesListData;
		notesListData = createNotesListData(blDomoFacade.getNotes(pageableData, date));
		setTotalCountHeader(response, notesListData.getPagination());
		return getDataMapper().map(notesListData, NotesListWsDTO.class, fields);
	}

	protected NotesListData createNotesListData(final SearchPageData<NotesData> result)
	{
		final NotesListData notesListData = new NotesListData();
		notesListData.setNotesList(result.getResults());
		notesListData.setSorts(result.getSorts());
		notesListData.setPagination(result.getPagination());
		return notesListData;
	}

}
