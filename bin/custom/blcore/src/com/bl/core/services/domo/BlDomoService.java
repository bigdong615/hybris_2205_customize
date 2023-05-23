/**
 *
 */
package com.bl.core.services.domo;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.Date;

import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlSerialLogModel;
import com.bl.core.model.CustomerNotesModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.InHouseRepairLogModel;
import com.bl.core.model.NotesModel;
import com.bl.core.model.PartsNeededRepairLogModel;
import com.bl.core.model.VendorRepairLogModel;
import com.braintree.model.BrainTreePaymentInfoModel;


public interface BlDomoService
{
	/**
	 * To fetch packagingInfos for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of packagingInfos
	 */
	SearchPageData<PackagingInfoModel> getPackagingInfos(final PageableData pageableData, final Date date);

	/**
	 * To fetch paymentTransactions for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of paymentTransactions
	 */
	SearchPageData<PaymentTransactionModel> getPaymentTransactions(final PageableData pageableData, final Date date);

	/**
	 * To fetch paymentTransactionEntries for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of paymentTransactionEntries
	 */
	SearchPageData<PaymentTransactionEntryModel> getPaymentTransactionEntries(final PageableData pageableData, final Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<GiftCardModel> getGiftCards(PageableData pageableData, Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<GiftCardMovementModel> getGiftCardMovements(PageableData pageableData, Date date);

	/**
	 * To fetch orders for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of orders
	 */
	SearchPageData<OrderModel> getOrders(final PageableData pageableData, final Date date);

	/**
	 * To fetch orderentries for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of orderentries
	 */
	SearchPageData<OrderEntryModel> getOrderEntries(final PageableData pageableData, final Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<BlItemsBillingChargeModel> getBlItemsBillingCharge(PageableData pageableData, Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<CustomerModel> getCustomers(PageableData pageableData, Date date);

	/**
	 * To fetch BlSerialLog for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of BlSerialLog
	 */

	SearchPageData<BlSerialLogModel> getBlSerialLogs(final PageableData pageableData, final Date date);

	/**
	 * To fetch CustomerNote for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of CustomerNote
	 */
	SearchPageData<CustomerNotesModel> getCustomerNotes(final PageableData pageableData, final Date date);

	/**
	 * To fetch VendorRepairLog for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of VendorRepairLog
	 */
	SearchPageData<VendorRepairLogModel> getVendorRepairLogs(final PageableData pageableData, final Date date);

	/**
	 * To fetch PartsNeededRepairLog for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of PartsNeededRepairLog
	 */
	SearchPageData<PartsNeededRepairLogModel> getPartsNeededRepairLogs(final PageableData pageableData, final Date date);

	/**
	 * To fetch InHouseRepair for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of InHouseRepair
	 */
	SearchPageData<InHouseRepairLogModel> getInHouseRepairLogs(final PageableData pageableData, final Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<AddressModel> getAddresses(PageableData pageableData, Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<BrainTreePaymentInfoModel> getBrainTreePaymentInfos(PageableData pageableData, Date date);

	/**
	 * @param pageableData
	 * @param toDate
	 * @return
	 */
	SearchPageData<StockLevelModel> getStockLevels(PageableData pageableData, Date fromDate, Date toDate);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<NotesModel> getNotes(final PageableData pageableData, final Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<RegionModel> getRegions(PageableData pageableData, Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<CartModel> getCarts(PageableData pageableData, Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<CartEntryModel> getCartEntries(PageableData pageableData, Date date);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<StockLevelModel> getStockModifiedTime(PageableData pageableData, Date date);

}
