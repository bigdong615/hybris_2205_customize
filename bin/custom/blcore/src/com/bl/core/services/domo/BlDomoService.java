/**
 *
 */
package com.bl.core.services.domo;

import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

public interface BlDomoService
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
	 * To fetch paymentTransactionEntries for the pageable data
	 *
	 * @param pageableData
	 * @return SearchPageData of paymentTransactionEntries
	 */
	SearchPageData<PaymentTransactionEntryModel> getPaymentTransactionEntries(final PageableData pageableData);

}
