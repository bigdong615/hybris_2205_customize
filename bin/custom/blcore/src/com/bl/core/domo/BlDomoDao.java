/**
 *
 */
package com.bl.core.domo;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;


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

}
