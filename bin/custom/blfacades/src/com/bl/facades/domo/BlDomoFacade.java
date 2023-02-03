/**
 *
 */
package com.bl.facades.domo;

import de.hybris.platform.commercefacades.BlItemsBillingChargeData;
import de.hybris.platform.commercefacades.giftcard.data.GiftCardData;
import de.hybris.platform.commercefacades.giftcard.movement.data.GiftCardMovementData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionEntryData;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;
import de.hybris.platform.warehousingfacades.product.data.StockLevelData;

import com.bl.facades.blSerialLog.data.BlSerialLogData;
import com.bl.facades.customerNotes.data.CustomerNotesData;
import com.bl.facades.customerNotes.data.NotesData;
import com.bl.facades.inHouseRepairLog.data.InHouseRepairLogData;
import com.bl.facades.partsNeededRepairLog.data.PartsNeededRepairLogData;
import com.bl.facades.vendorRepairLog.data.VendorRepairLogData;
import com.braintree.hybris.data.BrainTreePaymentInfoData;


public interface BlDomoFacade
{
	/**
	 * To fetch PackagingInfos for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of PackagingInfos
	 */
	SearchPageData<PackagingInfoData> getPackagingInfos(final PageableData pageableData);

	/**
	 * To fetch PackagingInfos for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of PackagingInfos
	 */
	SearchPageData<PaymentTransactionData> getPaymentTransactions(final PageableData pageableData);

	/**
	 * To fetch PackagingInfos for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of PackagingInfos
	 */
	SearchPageData<PaymentTransactionEntryData> getPaymentTransactionEntries(final PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<GiftCardData> getGiftCards(PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<GiftCardMovementData> getGiftCardMovements(PageableData pageableData);

	/**
	 * To fetch orders for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of orders
	 */
	SearchPageData<OrderData> getOrders(final PageableData pageableData);

	/**
	 * To fetch orderentries for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of orderentries
	 */
	SearchPageData<OrderEntryData> getOrderEntries(final PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<BlItemsBillingChargeData> getBlItemsBillingCharge(PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<CustomerData> getCustomers(PageableData pageableData);

	/**
	 * To fetch blSerialLogData for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of blSerialLogData
	 */
	SearchPageData<BlSerialLogData> getBlSerialLogs(final PageableData pageableData);

	/**
	 * To fetch CustomerNotes for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of CustomerNotes
	 */
	SearchPageData<CustomerNotesData> getCustomerNotes(final PageableData pageableData);

	/**
	 * To fetch VendorRepairLogData for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of VendorRepairLogData
	 */
	SearchPageData<VendorRepairLogData> getVendorRepairLogs(final PageableData pageableData);

	/**
	 * To fetch PartsNeededRepairLogData for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of PartsNeededRepairLogData
	 */
	SearchPageData<PartsNeededRepairLogData> getPartsNeededRepairLogs(final PageableData pageableData);

	/**
	 * To fetch InHouseRepairLogData for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of InHouseRepairLogData
	 */
	SearchPageData<InHouseRepairLogData> getInHouseRepairLogs(final PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<AddressData> getAddresses(PageableData pageableData);

	SearchPageData<BrainTreePaymentInfoData> getBrainTreePaymentInfo(PageableData pageableData);

	SearchPageData<StockLevelData> getStockLevels(PageableData pageableData);

	/**
	 * @param pageableData
	 * @return
	 */
	SearchPageData<NotesData> getNotes(PageableData pageableData);

}
