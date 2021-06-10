package com.braintree.customersupportbackoffice.commands.impl;

import com.braintree.command.result.BrainTreeFindCustomerResult;
import com.braintree.command.result.BrainTreeFindCustomersResult;
import com.braintree.commands.impl.AbstractCommand;
import com.braintree.customersupportbackoffice.commands.BrainTreeFindCustomerBackofficeCommand;
import com.braintree.customersupportbackoffice.commands.request.BrainTreeCustomerBackofficeRequest;
import com.braintreegateway.Customer;
import com.braintreegateway.CustomerSearchRequest;
import com.braintreegateway.ResourceCollection;
import com.braintreegateway.exceptions.NotFoundException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

public class FindCustomerBackofficeCommandImpl extends AbstractCommand<BrainTreeCustomerBackofficeRequest, BrainTreeFindCustomerResult>
        implements BrainTreeFindCustomerBackofficeCommand {

    private final static Logger LOG = Logger.getLogger(FindCustomerBackofficeCommandImpl.class);

    @Override
    public BrainTreeFindCustomerResult perform(final BrainTreeCustomerBackofficeRequest customerSearchRequest) {
        validateParameterNotNullStandardMessage("Find User Request", customerSearchRequest);
        final String customerId = customerSearchRequest.getCustomerId();
        validateParameterNotNullStandardMessage("customerId", customerId);
        try {
            final Customer customer = getBraintreeGateway().customer().find(customerId);
            boolean isCustomerExist = false;
            if (customer != null) {
                isCustomerExist = true;
            }
            return new BrainTreeFindCustomerResult(isCustomerExist);
        } catch (final Exception exception) {
            if (exception instanceof NotFoundException) {
                LOG.error("[BT Find Customer] Can't find BrainTree customer with id: " + customerId);
                return new BrainTreeFindCustomerResult(false);
            } else {
                LOG.error("[BT Find Customer] Error during try to find customer: " + customerId);
                throw new IllegalArgumentException(exception.getMessage());
            }
        }
    }

    @Override
    public BrainTreeFindCustomersResult process(final BrainTreeCustomerBackofficeRequest request) {
        final CustomerSearchRequest customerSearchRequest = translateRequest(request);
        final ResourceCollection<Customer> customers = getBraintreeGateway().customer().search(customerSearchRequest);
        return new BrainTreeFindCustomersResult(customers);
    }

    private CustomerSearchRequest translateRequest(BrainTreeCustomerBackofficeRequest request) {
        final CustomerSearchRequest customerSearchRequest = new CustomerSearchRequest();
        if (request != null) {
            if (StringUtils.isNotBlank(request.getCustomerId())) {
                customerSearchRequest.id().is(request.getCustomerId());
            }
            if (StringUtils.isNotBlank(request.getCustomerEmail())) {
                switch (request.getCustomerEmailOperator()) {
                    case CONTAINS:
                        customerSearchRequest.email().contains(request.getCustomerEmail());
                        break;

                    case STARTS_WITH:
                        customerSearchRequest.email().startsWith(request.getCustomerEmail());
                        break;

                    case ENDS_WITH:
                        customerSearchRequest.email().endsWith(request.getCustomerEmail());
                        break;

                    default:
                        customerSearchRequest.email().is(request.getCustomerEmail());
                        break;
                }
            }
        }
        return customerSearchRequest;
    }

}
