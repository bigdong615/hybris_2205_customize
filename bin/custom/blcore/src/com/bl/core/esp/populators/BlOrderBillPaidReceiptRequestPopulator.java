package com.bl.core.esp.populators;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.esp.dto.billpaid.OrderBillReceiptEventRequest;
import com.bl.esp.dto.billpaid.data.OrderBillReceiptData;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class BlOrderBillPaidReceiptRequestPopulator extends
        ESPEventCommonPopulator<OrderModel, OrderBillReceiptEventRequest> {

    private static final Logger LOG = Logger.getLogger(BlOrderBillPaidReceiptRequestPopulator.class);
    private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";

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
        data.setOrderId(orderModel.getCode());
        data.setSubscriberid(orderModel.getUser().getUid());
        data.setEmailAddress(orderModel.getUser().getUid());
        data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_BILL_PAID_RECEIPT_TEMPLATE)));
        populateOrderBillCommonData(orderModel, data, formatter);
        populateCustomerInfoData(orderModel,data);
        populateBillItemsInfo(orderModel,data);
        orderBillReceiptEventRequest.setData(data);
    }
    private void populateOrderBillCommonData(final OrderModel orderModel, final OrderBillReceiptData data, final SimpleDateFormat formatter) {
        BigDecimal totalAmount=BigDecimal.ZERO;
        BigDecimal totalTax = BigDecimal.ZERO;
        BigDecimal totalFee = BigDecimal.valueOf(5.00);
        data.setDatePlaced(formatter.format(orderModel.getDate()));
        if (BooleanUtils.isTrue(orderModel.getIsRentalOrder()) && BooleanUtils.isFalse(
                orderModel.isGiftCardOrder())) {
            data.setRentalStartDate(formatter.format(orderModel.getRentalStartDate()));
            data.setRentalEndDate(formatter.format(orderModel.getRentalEndDate()));
        }

        data.setOrderTotal(BlDateTimeUtils.formatAmount(getDoubleValueForRequest(orderModel.getTotalPrice())));
        data.setStatus(getRequestValue(getOrderStatus(orderModel)));
        data.setOrderStatusSub(getRequestValue(getOrderStatus(orderModel)));
        int serialQuantity = 0;
        BigDecimal subTotalWithoutTax = BigDecimal.valueOf(0.0);
        for (BlItemsBillingChargeModel bill : orderModel.getOrderBills()) {
            totalAmount=totalAmount.add(bill.getChargedAmount());
            totalTax = totalTax.add(bill.getTaxAmount());
            serialQuantity = serialQuantity + bill.getSerialCodes().size();
            for(String serial: bill.getSerialCodes()) {
                String[] serialDetail = serial.split(",");
                subTotalWithoutTax = subTotalWithoutTax.add(BigDecimal.valueOf(Double.valueOf(serialDetail[2])));
            }
        }
        data.setBillAmount(String.format("%.2f",totalAmount));
        data.setBillBalance(String.format("%.2f",(((CustomerModel) orderModel.getUser()).getTotalAmountPastDue())));
        data.setBillTax(String.format("%.2f",totalTax));
        data.setBillFees(String.format("%.2f",totalFee.multiply(BigDecimal.valueOf(serialQuantity))));

        data.setBillItemsAmount(String.format("%.2f",subTotalWithoutTax));
    }
    private void populateCustomerInfoData(final OrderModel orderModel, OrderBillReceiptData data) {
        try {
            final Document orderItemsInXMLDocument = createNewXMLDocument();
            final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument,
                    "Customer");
            final UserModel userModel = orderModel.getUser();
            if (Objects.nonNull(userModel)) {
                createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Customer_Name",
                        getRequestValue(userModel.getName()));
            }
            final AddressModel deliveryAddress = orderModel.getDeliveryAddress();
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Customer_Address_1",
                    getRequestValue(deliveryAddress.getLine1()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Customer_Address_2",
                    getRequestValue(deliveryAddress.getLine2()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Customer_City",
                    getRequestValue(deliveryAddress.getTown()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Customer_State",
                    getRequestValue(deliveryAddress.getRegion().getIsocode()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Customer_Zipcode",
                    getRequestValue(deliveryAddress.getPostalcode()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Customer_Phone",
                    getRequestValue(deliveryAddress.getPhone1()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Customer_Email",
                    getRequestValue(deliveryAddress.getEmail()));

            BrainTreePaymentInfoModel paymentInfoModel = (BrainTreePaymentInfoModel) orderModel.getPaymentInfo();
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,"Payment_Summary",
                    getRequestValue(paymentInfoModel.getPaymentProvider() + " - " + paymentInfoModel.getCardNumber()));

            final Transformer transformer = getTransformerFactoryObject();
            final StringWriter writer = new StringWriter();

            //transform document to string
            transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
            data.setCustomerInfo(writer.getBuffer().toString());

        }
        catch (final Exception exception){
            BlLogger.logMessage(LOG, Level.ERROR, POPULATOR_ERROR, exception);
            throw new BlESPIntegrationException(exception.getMessage(),
                    LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode(), exception);
        }
    }
    private void populateBillItemsInfo(final OrderModel orderModel, OrderBillReceiptData data) {
        try {
            final Document orderItemsInXMLDocument = createNewXMLDocument();
            final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument,
                    "Items");
            orderModel.getOrderBills().forEach(blItemsBillingChargeModel -> {
                blItemsBillingChargeModel.getSerialCodes().forEach(item -> {
                    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
                            rootOrderItems, "Item_Info");
                    String[] serialInfo = item.split(",");
                    //serial code
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, "Item_Id", getRequestValue(serialInfo[0]));
                    //serial main product
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, "Item_Title", getRequestValue(serialInfo[1]));
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, "Item_Issue", getRequestValue(blItemsBillingChargeModel.getBillChargeType().toString()));

                    double subtotal = Double.parseDouble(serialInfo[2]);
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, "Item_Bill_Amount", getRequestValue(String.format("%.2f", subtotal)));

                    String unPaidNote = blItemsBillingChargeModel.getUnPaidBillingNotes().stream().filter(note -> note.contains(serialInfo[0])).findFirst().orElse("Empty");
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, "Item_Notes", getRequestValue(unPaidNote));

                });
            });
            final Transformer transformer = getTransformerFactoryObject();
            final StringWriter writer = new StringWriter();

            //transform document to string
            transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
            data.setItemsInfo(writer.getBuffer().toString());

        }
        catch (Exception exception){
            BlLogger.logMessage(LOG, Level.ERROR, POPULATOR_ERROR, exception);
            throw new BlESPIntegrationException(exception.getMessage(),
                    LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode(), exception);
        }
    }
    private String getOrderStatus(final OrderModel orderModel) {
        return Objects.isNull(orderModel.getStatus()) ? StringUtils.EMPTY : orderModel.getStatus().getCode();
    }
}