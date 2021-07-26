package com.braintree.customersupportbackoffice.widgets.advancedsearch.engine;

import com.braintree.customersupportbackoffice.facade.BrainTreeCustomerSupportFacade;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.braintree.model.BraintreeCustomerDetailsModel;
import com.hybris.backoffice.widgets.advancedsearch.impl.AdvancedSearchData;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.search.data.SortData;
import com.hybris.cockpitng.search.data.pageable.Pageable;
import org.springframework.beans.factory.annotation.Required;

import java.util.Collections;
import java.util.List;

public class BrainTreeAdvancedSearchEngineController extends com.hybris.backoffice.widgets.advancedsearch.engine.AdvancedSearchEngineController {
    private BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade;
    private int pageSize;

    @Override
    @SocketEvent(socketId = "searchData")
    public void onSearchDataInput(AdvancedSearchData searchData) {
        switch (searchData.getTypeCode()) {
            case "BrainTreeBackofficeTransactionCustomer":
                searchTransactionCustomers(searchData);
                break;
            case "BrainTreeBackofficeTransactionDetail":
                searchTransactions(searchData);
                break;
            default:
                super.onSearchDataInput(searchData);
        }
    }

    private void searchTransactions(AdvancedSearchData searchData) {
        BackofficeTransactionSearchPageable pageableOutput = null;

        List<BrainTreeTransactionDetailModel> transactions = brainTreeCustomerSupportFacade.findTransactions(searchData);

        if (transactions != null && !transactions.isEmpty()) {
            pageableOutput = new BackofficeTransactionSearchPageable(transactions);
            if (this.pageSize <= 0) {
                this.pageSize = this.getWidgetSettings().getInt("defaultPageSize");
            }
            pageableOutput.setPageSize(this.pageSize);
        }

        this.sendOutput("pageable", pageableOutput);
    }

    private void searchTransactionCustomers(AdvancedSearchData searchData) {
        BackofficeCustomerSearchPageable pageableOutput = null;

        List<BraintreeCustomerDetailsModel> customers = brainTreeCustomerSupportFacade.findCustomers(searchData);

        if (customers != null && !customers.isEmpty()) {
            pageableOutput = new BackofficeCustomerSearchPageable(customers);
            if (this.pageSize <= 0) {
                this.pageSize = this.getWidgetSettings().getInt("defaultPageSize");
            }
            pageableOutput.setPageSize(this.pageSize);
        }

        this.sendOutput("pageable", pageableOutput);
    }

    @Required
    @SuppressWarnings("unused")
    public void setBrainTreeCustomerSupportFacade(BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade) {
        this.brainTreeCustomerSupportFacade = brainTreeCustomerSupportFacade;
    }

    @SuppressWarnings("unused")
    public void setPageSize(int pageSize) {
        this.pageSize = pageSize;
    }

    private class BackofficeTransactionSearchPageable implements Pageable<BrainTreeTransactionDetailModel> {
        private List<BrainTreeTransactionDetailModel> transactions;
        private int pageNumber;
        private int pageSize;
        private SortData sortData;

        public BackofficeTransactionSearchPageable(List<BrainTreeTransactionDetailModel> transactions) {
            this.transactions = transactions;
        }

        @Override
        public List<BrainTreeTransactionDetailModel> getCurrentPage() {
            int fromIndex = pageNumber * pageSize;
            int toIndex = Math.min((pageNumber + 1) * pageSize, transactions.size());
            return transactions.subList(fromIndex, toIndex);
        }

        @Override
        public void refresh() {

        }

        @Override
        public List<BrainTreeTransactionDetailModel> nextPage() {
            int fromIndex = (pageNumber + 1) * pageSize;
            if (fromIndex >= transactions.size())
                return Collections.emptyList();
            else {
                int toIndex = Math.min((pageNumber + 2) * pageSize, transactions.size());
                return transactions.subList(fromIndex, toIndex);
            }
        }

        @Override
        public List<BrainTreeTransactionDetailModel> previousPage() {
            if (pageNumber == 0)
                return Collections.emptyList();
            else {
                int fromIndex = (pageNumber - 1) * pageSize;
                int toIndex = pageNumber * pageSize;
                return transactions.subList(fromIndex, toIndex);
            }
        }

        @Override
        public boolean hasNextPage() {
            return (pageNumber + 1) * pageSize < transactions.size();
        }

        @Override
        public boolean hasPreviousPage() {
            return pageNumber > 0;
        }

        @Override
        public int getPageSize() {
            return pageSize;
        }

        @Override
        public String getTypeCode() {
            return "BrainTreeBackofficeTransactionDetail";
        }

        @Override
        public List<BrainTreeTransactionDetailModel> setPageSize(int pageSize) {
            this.pageSize = pageSize;
            this.pageNumber = 0;
            return getCurrentPage();
        }

        @Override
        public int getTotalCount() {
            return transactions.size();
        }

        @Override
        public int getPageNumber() {
            return pageNumber;
        }

        @Override
        public void setPageNumber(int pageNumber) {
            this.pageNumber = pageNumber;
        }

        @Override
        public SortData getSortData() {
            return sortData;
        }

        @Override
        public void setSortData(SortData sortData) {
            this.sortData = sortData;
        }

        @Override
        public List<BrainTreeTransactionDetailModel> getAllResults() {
            return transactions;
        }
    }

    private class BackofficeCustomerSearchPageable implements Pageable<BraintreeCustomerDetailsModel> {
        private List<BraintreeCustomerDetailsModel> customers;
        private int pageNumber;
        private int pageSize;
        private SortData sortData;

        BackofficeCustomerSearchPageable(List<BraintreeCustomerDetailsModel> customers) {
            this.customers = customers;
        }

        @Override
        public List<BraintreeCustomerDetailsModel> getCurrentPage() {
            int fromIndex = pageNumber * pageSize;
            int toIndex = Math.min((pageNumber + 1) * pageSize, customers.size());
            return customers.subList(fromIndex, toIndex);
        }

        @Override
        public void refresh() {

        }

        @Override
        public List<BraintreeCustomerDetailsModel> nextPage() {
            int fromIndex = (pageNumber + 1) * pageSize;
            if (fromIndex >= customers.size())
                return Collections.emptyList();
            else {
                int toIndex = Math.min((pageNumber + 2) * pageSize, customers.size());
                return customers.subList(fromIndex, toIndex);
            }
        }

        @Override
        public List<BraintreeCustomerDetailsModel> previousPage() {
            if (pageNumber == 0)
                return Collections.emptyList();
            else {
                int fromIndex = (pageNumber - 1) * pageSize;
                int toIndex = pageNumber * pageSize;
                return customers.subList(fromIndex, toIndex);
            }
        }

        @Override
        public boolean hasNextPage() {
            return (pageNumber + 1) * pageSize < customers.size();
        }

        @Override
        public boolean hasPreviousPage() {
            return pageNumber > 0;
        }

        @Override
        public int getPageSize() {
            return pageSize;
        }

        @Override
        public String getTypeCode() {
            return "BrainTreeBackofficeTransactionCustomer";
        }

        @Override
        public List<BraintreeCustomerDetailsModel> setPageSize(int pageSize) {
            this.pageSize = pageSize;
            this.pageNumber = 0;
            return getCurrentPage();
        }

        @Override
        public int getTotalCount() {
            return customers.size();
        }

        @Override
        public int getPageNumber() {
            return pageNumber;
        }

        @Override
        public void setPageNumber(int pageNumber) {
            this.pageNumber = pageNumber;
        }

        @Override
        public SortData getSortData() {
            return sortData;
        }

        @Override
        public void setSortData(SortData sortData) {
            this.sortData = sortData;
        }

        @Override
        public List<BraintreeCustomerDetailsModel> getAllResults() {
            return customers;
        }
    }
}
