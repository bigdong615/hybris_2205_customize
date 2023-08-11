package com.bl.core.esp.populators;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.esp.dto.billpaid.OrderBillReceiptEventRequest;
import com.bl.esp.dto.billpaid.data.OrderBillItemInfoData;
import com.bl.esp.dto.billpaid.data.OrderBillReceiptData;
import com.bl.esp.dto.customer.data.CustomerInfoData;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class BlOrderBillPaidReceiptRequestPopulator extends
        ESPEventCommonPopulator<OrderModel, OrderBillReceiptEventRequest> {
    @Override
    public void populate(final OrderModel orderModel,
                         final OrderBillReceiptEventRequest orderBillReceiptEventRequest)
            throws ConversionException {
        Assert.notNull(orderModel, "Parameter emailId cannot be null.");
        Assert.notNull(orderBillReceiptEventRequest, "Parameter contactRequest cannot be null.");
        final UserModel userModel = orderModel.getUser();
        if (Objects.nonNull(userModel)) {
            orderBillReceiptEventRequest.setContactKey(getRequestValue(userModel.getUid()));
        }
        orderBillReceiptEventRequest
                .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
                        getString(BlCoreConstants.ORDER_BILL_PAID_RECEIPT_DEFINITION)));
        populateOrderBillData(orderModel, orderBillReceiptEventRequest);
    }
    private void populateOrderBillData(final OrderModel orderModel, final OrderBillReceiptEventRequest orderBillReceiptEventRequest) {
        final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
        final OrderBillReceiptData data = new OrderBillReceiptData();
        populateCommonData(orderModel, data);
        data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_BILL_PAID_RECEIPT_TEMPLATE)));
        populateOrderBillCommonData(orderModel, data, formatter);
        final CustomerInfoData customerInfoData = populateCustomerInfoData(orderModel);
        data.setCustomerInfo(customerInfoData);
        final List<OrderBillItemInfoData> itemsInfo = populateBillItemsInfo(orderModel);
        data.setItemsInfo(itemsInfo);
        orderBillReceiptEventRequest.setData(data);
    }
    private void populateOrderBillCommonData(final OrderModel orderModel, final OrderBillReceiptData data, final SimpleDateFormat formatter) {
        BigDecimal totalAmount=BigDecimal.ZERO;
        data.setDatePlaced(formatter.format(orderModel.getDate()));
        if (BooleanUtils.isTrue(orderModel.getIsRentalOrder()) && BooleanUtils.isFalse(
                orderModel.isGiftCardOrder())) {
            data.setRentalStartDate(formatter.format(orderModel.getRentalStartDate()));
            data.setRentalEndDate(formatter.format(orderModel.getRentalEndDate()));
        }
        data.setOrderTotal(BlDateTimeUtils.formatAmount(getDoubleValueForRequest(orderModel.getTotalPrice())));
        data.setStatus(getRequestValue(getOrderStatus(orderModel)));
        data.setOrderStatusSub(getRequestValue(getOrderStatus(orderModel)));
        for (BlItemsBillingChargeModel bill : orderModel.getOrderBills()) {
            totalAmount=totalAmount.add(bill.getChargedAmount());
        }
        data.setBillAmount(totalAmount.toString());
        data.setBillBalance(totalAmount.toString());
    }
    private CustomerInfoData populateCustomerInfoData(final OrderModel orderModel) {
        CustomerInfoData customerInfoData = new CustomerInfoData();
        final UserModel userModel = orderModel.getUser();
        if (Objects.nonNull(userModel)) {
            customerInfoData.setCustomerName(getRequestValue(userModel.getName()));
        }
        final AddressModel deliveryAddress = orderModel.getDeliveryAddress();
        customerInfoData.setCustomerAddressLine1(deliveryAddress.getLine1());
        customerInfoData.setCustomerAddressLine2(deliveryAddress.getLine2());
        customerInfoData.setCustomerCity(deliveryAddress.getTown());
        customerInfoData.setCustomerState(deliveryAddress.getRegion().getIsocode());
        customerInfoData.setCustomerZipcode(deliveryAddress.getPostalcode());
        customerInfoData.setCustomerPhone(deliveryAddress.getPhone1());
        customerInfoData.setCustomerEmail(deliveryAddress.getEmail());
        BrainTreePaymentInfoModel paymentInfoModel=(BrainTreePaymentInfoModel) orderModel.getPaymentInfo();
        customerInfoData.setPaymentSummary(paymentInfoModel.getCardNumber());
        return customerInfoData;
    }
    private List<OrderBillItemInfoData> populateBillItemsInfo(final OrderModel orderModel) {
        List<OrderBillItemInfoData> billInfoList = new ArrayList<>();
        orderModel.getOrderBills().forEach(blItemsBillingChargeModel -> {
            OrderBillItemInfoData orderBillItemInfoData = new OrderBillItemInfoData();
            orderBillItemInfoData.setItemId(blItemsBillingChargeModel.getSerialCodes().stream().collect(Collectors.joining(",")));
            orderBillItemInfoData.setItemTitle(blItemsBillingChargeModel.toString());
            orderBillItemInfoData.setItemIssue(blItemsBillingChargeModel.getBillChargeType().toString());
            orderBillItemInfoData.setItemBillAmount(blItemsBillingChargeModel.getChargedAmount().toString());
            orderBillItemInfoData.setItemNotes(blItemsBillingChargeModel.getUnPaidBillingNotes());
            billInfoList.add(orderBillItemInfoData);
        });
        return billInfoList;
    }
    private String getOrderStatus(final OrderModel orderModel) {
        return Objects.isNull(orderModel.getStatus()) ? StringUtils.EMPTY : orderModel.getStatus().getCode();
    }
}