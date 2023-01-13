/**
 *
 */
package com.bl.core.domo;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlSerialLogModel;
import com.bl.core.model.CustomerNotesModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.InHouseRepairLogModel;
import com.bl.core.model.PartsNeededRepairLogModel;
import com.bl.core.model.VendorRepairLogModel;


public interface BlDomoDao
{
	/**
	 * To fetch packagingInfos for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of packagingInfos
	 */
	SearchPageData<PackagingInfoModel> getPackagingInfos(final PageableData pageableData);

	/**
	 * To fetch paymentTransactions for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of paymentTransactions
	 */
	SearchPageData<PaymentTransactionModel> getPaymentTransactions(final PageableData pageableData);

	/**
	 * To fetch paymentTransactions for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of paymentTransactions
	 */
	SearchPageData<PaymentTransactionEntryModel> getPaymentTransactionEntries(final PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<GiftCardModel> getGiftCards(PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<GiftCardMovementModel> getGiftCardMovements(PageableData pageableData);

	/**
	 * To fetch orders for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of orders
	 */
	SearchPageData<OrderModel> getOrders(final PageableData pageableData);

	/**
	 * To fetch orderentries for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of orderentries
	 */
	SearchPageData<OrderEntryModel> getOrderEntries(final PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<BlItemsBillingChargeModel> getBlItemsBillingCharge(PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<CustomerModel> getCustomers(PageableData pageableData);


	/**
	 * To fetch BlSerialLog for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of BlSerialLog
	 */

	SearchPageData<BlSerialLogModel> getBlSerialLogs(final PageableData pageableData);

	/**
	 * To fetch CustomerNote for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of CustomerNote
	 */
	SearchPageData<CustomerNotesModel> getCustomerNotes(final PageableData pageableData);

	/**
	 * To fetch VendorRepairLog for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of VendorRepairLog
	 */
	SearchPageData<VendorRepairLogModel> getVendorRepairLogs(final PageableData pageableData);

	/**
	 * To fetch PartsNeededRepairLog for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of PartsNeededRepairLog
	 */
	SearchPageData<PartsNeededRepairLogModel> getPartsNeededRepairLogs(final PageableData pageableData);

	/**
	 * To fetch InHouseRepair for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of InHouseRepair
	 */
	SearchPageData<InHouseRepairLogModel> getInHouseRepairLogs(final PageableData pageableData);

}
