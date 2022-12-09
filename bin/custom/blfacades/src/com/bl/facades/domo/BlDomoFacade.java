/**
 *
 */
package com.bl.facades.domo;

import de.hybris.platform.commercefacades.giftcard.data.GiftCardData;
import de.hybris.platform.commercefacades.giftcard.movement.data.GiftCardMovementData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionData;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionEntryData;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;


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

}
